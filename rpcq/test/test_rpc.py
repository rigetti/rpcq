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
import time
from multiprocessing import Process

import pytest
import zmq

from rpcq._base import to_msgpack, from_msgpack
from rpcq._server import Server
from rpcq._client import Client
from rpcq._utils import rpc_request, RPCErrorError

# Set up logging for easier debugging of test failures, but disable logging any exceptions thrown in the mock server
# since those are expected
logging.basicConfig(format='%(asctime)s - %(name)s %(levelname)s : %(message)s', level=logging.DEBUG)
logging.getLogger('rpcq._spec').setLevel(logging.CRITICAL)

mock = Server()


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


# Some functions that will eventually raise an error.
def do_oops():
    oops()


def oops():
    raise ValueError("Oops.")


@pytest.fixture
def m_endpoints():
    return "tcp://localhost:5557", "tcp://*:5557"


def run_mock(_, endpoint):
    # Need a new event loop for a new process
    mock.run(endpoint, loop=asyncio.new_event_loop())


def test_server(request, m_endpoints):
    context = zmq.Context()
    backend = context.socket(zmq.DEALER)
    backend.connect(m_endpoints[0])
    request.addfinalizer(backend.close)

    proc = Process(target=run_mock, args=m_endpoints)
    proc.start()

    request = rpc_request("add", 1, 2)
    backend.send(to_msgpack(request))
    reply = from_msgpack(backend.recv())
    assert reply.result == 3

    os.kill(proc.pid, signal.SIGINT)


@pytest.fixture
def server(request, m_endpoints):
    proc = Process(target=run_mock, args=m_endpoints)
    proc.start()
    yield proc
    os.kill(proc.pid, signal.SIGINT)


@pytest.fixture
def client(request, m_endpoints):
    client = Client(m_endpoints[0])
    request.addfinalizer(client.close)
    return client


def test_client(server, client):
    assert client.call('add', 1, 1) == 2
    assert client.call('foo') == 'bar'
    with pytest.raises(RPCErrorError):
        client.call('non_existent_method')
    try:
        client.call('raise_error')
    except RPCErrorError as e:
        # Get the full traceback and make sure it gets propagated correctly. Remove line numbers.
        full_traceback = ''.join([i for i in str(e) if not i.isdigit()])
        assert 'Oops.\nTraceback (most recent call last):\n  ' in full_traceback
        assert 'ValueError: Oops.' in full_traceback


def test_client_timeout(server, client):
    client.timeout = 0.05
    with pytest.raises(TimeoutError):
        client.call('sleep', 0.1)


# regression test for buildup of requests to Server until Client is closed and reopened
def test_client_backlog(server, client):
    # Test 1: The call to 'add' will actually receive the response to 'sleep' so we need to make sure it will discard it
    # This should fail if you remove the while loop from Client.call
    client.timeout = 0.2
    with pytest.raises(TimeoutError):
        client.call('sleep', 0.4)
    time.sleep(0.8)
    assert client.call('add', 1, 1) == 2

    # Test 2: Keep track of timeouts correctly even when the client has received a response for a different request
    # This should fail if you remove the manual elapsed time tracking from Client.call
    with pytest.raises(TimeoutError):
        client.call('sleep', 0.28)
    with pytest.raises(TimeoutError):
        client.call('sleep', 0.22)


@pytest.mark.asyncio
async def test_async_client(server, client):
    reply = await client.call_async('foo')
    with pytest.raises(RPCErrorError):
        await client.call_async('non_existent_method')
    try:
        await client.call_async('raise_error')
    except RPCErrorError as e:
        # Get the full traceback and make sure it gets propagated correctly. Remove line numbers.
        full_traceback = ''.join([i for i in str(e) if not i.isdigit()])
        assert 'Oops.\nTraceback (most recent call last):\n  ' in full_traceback
        assert 'ValueError: Oops.' in full_traceback

    assert reply == "bar"


@pytest.mark.asyncio
async def test_async_client_timeout(server, client):
    client.timeout = 0.05
    with pytest.raises(TimeoutError):
        await client.call_async('sleep', 0.1)


@pytest.mark.asyncio
async def test_parallel_calls(server, client):
    # Add a sleep to the first call to force the server to return the replies out of order
    a = asyncio.ensure_future(client.call_async('sleep', 0.5))
    b = asyncio.ensure_future(client.call_async('sleep', 0))

    assert await a == 0.5
    assert await b == 0


@pytest.mark.asyncio
async def test_cancelling(server, client):
    # This is a very deliberate test. It tests the case where one client has sent a request but was cancelled prior
    # to receiving anything from the server. This means another client will receive an orphaned message and needs to
    # store it but continue asking for messages.
    a = asyncio.ensure_future(client.call_async('sleep', 0.3))
    b = asyncio.ensure_future(client.call_async('sleep', 0.6))

    # Add a small sleep here to release the event loop and allow both of the above calls to be sent
    await asyncio.sleep(0.1)
    a.cancel()
    assert await b == 0.6


@pytest.mark.asyncio
async def test_three_cancelling(server, client):
    # Same as above but with three async calls
    a = asyncio.ensure_future(client.call_async('sleep', 0.3))
    b = asyncio.ensure_future(client.call_async('sleep', 0.6))
    c = asyncio.ensure_future(client.call_async('sleep', 0.6))

    await asyncio.sleep(0.1)
    a.cancel()
    assert await b == 0.6
    assert await c == 0.6


@pytest.mark.asyncio
async def test_two_clients(server, request, m_endpoints):
    # ZeroMQ should be able to handle two independent client sockets by giving them each separate identities
    client1 = Client(m_endpoints[0])
    request.addfinalizer(client1.close)

    client2 = Client(m_endpoints[0])
    request.addfinalizer(client2.close)

    a = asyncio.ensure_future(client1.call_async('sleep', 0.3))
    b = asyncio.ensure_future(client1.call_async('sleep', 0.1))

    c = asyncio.ensure_future(client2.call_async('sleep', 0.2))
    d = asyncio.ensure_future(client2.call_async('sleep', 0.4))

    assert await a == 0.3
    assert await b == 0.1
    assert await c == 0.2
    assert await d == 0.4
