##############################################################################
# Copyright 2018 Rigetti Computing
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
##############################################################################
import asyncio
import logging
import os
import signal
import sys
import time
from multiprocessing import Process
from warnings import warn, catch_warnings

import pytest
import zmq
from zmq.auth import certs

from rpcq._base import to_msgpack, from_msgpack
from rpcq._server import Server, ServerAuthConfig
from rpcq._client import Client, ClientAuthConfig
from rpcq._utils import rpc_request, RPCErrorError, RPCError

# Set up logging for easier debugging of test failures, but disable logging
#   any exceptions thrown in the mock server since those are expected
logging.basicConfig(
    format='%(asctime)s - %(name)s %(levelname)s : %(message)s',
    level=logging.DEBUG)
logging.getLogger('rpcq._spec').setLevel(logging.CRITICAL)

# Valid, sample Z85-encoded keys specified by zmq curve for testing:
#   http://api.zeromq.org/master:zmq-curve#toc4
CLIENT_PUBLIC_KEY = b"Yne@$w-vo<fVvi]a<NY6T1ed:M$fCG*[IaLV{hID"
CLIENT_SECRET_KEY = b"D:)Q[IlAW!ahhC2ac:9*A}h:p?([4%wOTJ%JR%cs"
SERVER_PUBLIC_KEY = b"rq:rM>}U?@Lns47E1%kR.o@n%FcmmsL/@{H8]yf7"
SERVER_SECRET_KEY = b"JTKVSB%%)wK0E.X)V>+}o?pNmC{O&4W4b!Ni{Lh6"


def write_key(directory, public_key, secret_key=None):
    certs._write_key_file(os.path.join(directory, f'{time.time()}.key'), '',
                          public_key, secret_key)


def build_mock_server(tmpdir, enable_auth=True):
    write_key(tmpdir, CLIENT_PUBLIC_KEY)

    if enable_auth:
        auth_config = ServerAuthConfig(server_secret_key=SERVER_SECRET_KEY,
                                       server_public_key=SERVER_PUBLIC_KEY,
                                       client_keys_directory=tmpdir)
    else:
        auth_config = None

    mock = Server(auth_config=auth_config)

    @mock.rpc_handler
    async def add(a, b):
        return a + b

    @mock.rpc_handler
    def foo():
        return 'bar'

    @mock.rpc_handler
    async def sleep(n: int):
        await asyncio.sleep(n)
        return n

    @mock.rpc_handler
    def raise_error():
        do_oops()

    @mock.rpc_handler
    def just_a_warning():
        warn("Watch out!")
        return 5

    # Some functions that will eventually raise an error.
    def do_oops():
        oops()

    def oops():
        raise ValueError("Oops.")

    return mock


def build_auth_client(endpoint):
    auth_config = ClientAuthConfig(client_secret_key=CLIENT_SECRET_KEY,
                                   client_public_key=CLIENT_PUBLIC_KEY,
                                   server_public_key=SERVER_PUBLIC_KEY)

    client = Client(endpoint, auth_config=auth_config)
    return client


@pytest.fixture
def m_endpoints():
    return "tcp://localhost:5557", "tcp://*:5557"


def run_mock(_, endpoint, tmpdir, server_auth=True):
    # Need a new event loop for a new process
    build_mock_server(tmpdir, enable_auth=server_auth).run(
        endpoint, loop=asyncio.new_event_loop())


def test_encrypted_server_rejects_unencrypted_client(server, client_no_auth):
    client_no_auth.timeout = 1
    with pytest.raises(TimeoutError):
        client_no_auth.call('add', 1, 1)


def test_encrypted_client_rejects_unencrypted_server(server_no_auth,
                                                     client_auth):
    client_auth.timeout = 1
    with pytest.raises(TimeoutError):
        client_auth.call('add', 1, 1)


def test_unencrypted_server_accepts_unencrypted_client(server_no_auth,
                                                       client_no_auth):
    assert client_no_auth.call('add', 1, 1) == 2


@pytest.fixture
def server_no_auth(request, m_endpoints, tmpdir):
    proc = Process(target=run_mock, args=(*m_endpoints, tmpdir, False))
    proc.start()
    yield proc
    os.kill(proc.pid, signal.SIGINT)


@pytest.fixture
def server(request, m_endpoints, tmpdir):
    proc = Process(target=run_mock, args=(*m_endpoints, tmpdir))
    proc.start()
    yield proc
    os.kill(proc.pid, signal.SIGINT)


@pytest.fixture
def client_no_auth(request, m_endpoints):
    client = Client(m_endpoints[0])
    request.addfinalizer(client.close)
    return client


@pytest.fixture
def client_auth(request, m_endpoints):
    client = build_auth_client(m_endpoints[0])
    request.addfinalizer(client.close)
    return client

if sys.version_info < (3, 7):
    OOPS_VALUE_ERROR_STR = "ValueError('Oops.',)\nTraceback (most recent call last):\n  "
else:
    # The default repr for BaseException was changed in 3.7 to elide the trailing comma.
    OOPS_VALUE_ERROR_STR = "ValueError('Oops.')\nTraceback (most recent call last):\n  "


def test_client_warning(server, client_auth):
    with catch_warnings(record=True) as warnings:
        result = client_auth.call('just_a_warning')
        assert result == 5
        assert len(warnings) > 0
        assert str(warnings[0].message) == "UserWarning: Watch out!"


def test_client_simple(server, client_auth):
    assert client_auth.call('add', 1, 1) == 2
    assert client_auth.call('foo') == 'bar'
    with pytest.raises(RPCError):
        client_auth.call('non_existent_method')
    try:
        client_auth.call('raise_error')
    except RPCError as e:
        # Get the full traceback and make sure it gets propagated correctly. Remove line numbers.
        full_traceback = ''.join([i for i in str(e) if not i.isdigit()])
        assert OOPS_VALUE_ERROR_STR in full_traceback
        assert 'ValueError: Oops.' in full_traceback


def test_client_timeout(server, client_auth):
    client_auth.timeout = 0.05
    with pytest.raises(TimeoutError):
        client_auth.call('sleep', 0.1)


# regression test for buildup of requests to Server until Client is closed and reopened
def test_client_backlog(server, client_auth):
    # Test 1: The call to 'add' will actually receive the response to 'sleep' so we need to make sure it will discard it
    # This should fail if you remove the while loop from Client.call
    client_auth.timeout = 0.2
    with pytest.raises(TimeoutError):
        client_auth.call('sleep', 0.4)
    time.sleep(0.8)
    assert client_auth.call('add', 1, 1) == 2

    # Test 2: Keep track of timeouts correctly even when the client has received a response for a different request
    # This should fail if you remove the manual elapsed time tracking from Client.call
    with pytest.raises(TimeoutError):
        client_auth.call('sleep', 0.28)
    with pytest.raises(TimeoutError):
        client_auth.call('sleep', 0.22)


@pytest.mark.asyncio
async def test_async_client_rpcerrorerror(server, client_auth):
    reply = await client_auth.call_async('foo')
    with pytest.raises(RPCErrorError):  # RPCErrorError is deprecated
        await client_auth.call_async('non_existent_method')
    try:
        await client_auth.call_async('raise_error')
    except RPCErrorError as e:  # RPCErrorError is deprecated
        # Get the full traceback and make sure it gets propagated correctly. Remove line numbers.
        full_traceback = ''.join([i for i in str(e) if not i.isdigit()])
        assert OOPS_VALUE_ERROR_STR in full_traceback
        assert 'ValueError: Oops.' in full_traceback

    assert reply == "bar"


@pytest.mark.asyncio
async def test_async_client(server, client_auth):
    reply = await client_auth.call_async('foo')
    with pytest.raises(RPCError):
        await client_auth.call_async('non_existent_method')
    try:
        await client_auth.call_async('raise_error')
    except RPCError as e:
        # Get the full traceback and make sure it gets propagated correctly. Remove line numbers.
        full_traceback = ''.join([i for i in str(e) if not i.isdigit()])
        assert OOPS_VALUE_ERROR_STR in full_traceback
        assert 'ValueError: Oops.' in full_traceback

    assert reply == "bar"


@pytest.mark.asyncio
async def test_async_client_timeout(server, client_auth):
    client_auth.timeout = 0.05
    with pytest.raises(TimeoutError):
        await client_auth.call_async('sleep', 0.1)


@pytest.mark.asyncio
async def test_parallel_calls(server, client_auth):
    # Add a sleep to the first call to force the server to return the replies out of order
    a = asyncio.ensure_future(client_auth.call_async('sleep', 0.5))
    b = asyncio.ensure_future(client_auth.call_async('sleep', 0))

    assert await a == 0.5
    assert await b == 0


@pytest.mark.asyncio
async def test_cancelling(server, client_auth):
    # This is a very deliberate test. It tests the case where one client has sent a request but was cancelled prior
    # to receiving anything from the server. This means another client will receive an orphaned message and needs to
    # store it but continue asking for messages.
    a = asyncio.ensure_future(client_auth.call_async('sleep', 0.3))
    b = asyncio.ensure_future(client_auth.call_async('sleep', 0.6))

    # Add a small sleep here to release the event loop and allow both of the above calls to be sent
    await asyncio.sleep(0.1)
    a.cancel()
    assert await b == 0.6


@pytest.mark.asyncio
async def test_three_cancelling(server, client_auth):
    # Same as above but with three async calls
    a = asyncio.ensure_future(client_auth.call_async('sleep', 0.3))
    b = asyncio.ensure_future(client_auth.call_async('sleep', 0.6))
    c = asyncio.ensure_future(client_auth.call_async('sleep', 0.6))

    await asyncio.sleep(0.1)
    a.cancel()
    assert await b == 0.6
    assert await c == 0.6


@pytest.mark.asyncio
async def test_two_clients(server, request, m_endpoints):
    # ZeroMQ should be able to handle two independent client sockets by giving them each separate identities
    client1 = build_auth_client(m_endpoints[0])
    request.addfinalizer(client1.close)

    client2 = build_auth_client(m_endpoints[0])
    request.addfinalizer(client2.close)

    a = asyncio.ensure_future(client1.call_async('sleep', 0.3))
    b = asyncio.ensure_future(client1.call_async('sleep', 0.1))

    c = asyncio.ensure_future(client2.call_async('sleep', 0.2))
    d = asyncio.ensure_future(client2.call_async('sleep', 0.4))

    assert await a == 0.3
    assert await b == 0.1
    assert await c == 0.2
    assert await d == 0.4


@pytest.mark.parametrize('authorized_key_count', (1, 10, 100, 1000, 10000))
def test_benchmark_key_authorization(authorized_key_count, benchmark, m_endpoints, tmpdir):
    """
    Measure the effect that increasing authorized key count has on connection latency. 

    The list of authorized keys must be maintained in memory by the ZeroMQ server, and this test
    is meant to assess how well it scales with the number of keys authorized for connection.
    How long does connection (and a "no-op" RPC call) take, relative to the number of other keys
    also authorized?
    """

    client = build_auth_client(m_endpoints[0])

    # Generate many keys which are not the test client's key in order to fill up the authorized
    # key list. Write them to the temporary directory for use by the server.
    for key in [zmq.curve_keypair()[0] for _ in range(authorized_key_count - 1)]:
        write_key(tmpdir, key)

    # Write the correct key
    write_key(tmpdir, CLIENT_PUBLIC_KEY)

    def run_server():
        auth_config = ServerAuthConfig(
            server_secret_key=SERVER_SECRET_KEY,
            server_public_key=SERVER_PUBLIC_KEY,
            client_keys_directory=tmpdir
        )

        server = Server(auth_config=auth_config)

        @server.rpc_handler
        async def no_op():
            return

        server.run(m_endpoints[1], loop=asyncio.new_event_loop())

    proc = Process(target=run_server)
    proc.start()

    try:
        # Assert that the service is listening, give it time to process all of its keys and start up
        client.call('no_op', rpc_timeout=10)

        def run_client():
            client.call('no_op', rpc_timeout=1)

        benchmark(run_client)

        # Forcibly kill the server process and release the socket in time for the next test
    finally:
        os.kill(proc.pid, signal.SIGINT)
