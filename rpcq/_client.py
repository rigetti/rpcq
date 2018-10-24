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
import time
from typing import Dict, Union

import zmq
import zmq.asyncio

from rpcq._base import to_msgpack, from_msgpack
from rpcq._utils import rpc_request, RPCErrorError
from rpcq.messages import RPCError, RPCReply

_log = logging.getLogger(__name__)


class Client:
    """
    Client that executes methods on a remote server by sending JSON RPC requests to a socket.
    """
    def __init__(self, endpoint: str, timeout: float = None):
        """
        Create a client that connects to a server at <endpoint>.

        :param str endpoint: Socket endpoint, e.g. "tcp://localhost:1234"
        :param float timeout: Timeout in seconds for Server response, set to None to disable the timeout
        """
        # TODO: leaving self.timeout for backwards compatibility; we should move towards using rpc_timeout only
        self.timeout = timeout
        self.rpc_timeout = timeout
        self.endpoint = endpoint

        self._socket = self._connect_to_socket(zmq.Context(), endpoint)
        # The async socket can't be created yet because it's possible that the current event loop during Client creation
        # is different to the one used later to call a method, so we need to create the socket after the first call and
        # then cache it
        self._async_socket_cache = None

        # Mapping from request id to an event used to wake up the call that's waiting on that request.
        # This is necessary to support parallel, asynchronous calls where we don't know which
        # receive task will receive which reply.
        self._events: Dict[str, asyncio.Event] = {}

        # Cache of replies so that different tasks can share results with each other
        self._replies: Dict[str, Union[RPCReply, RPCError]] = {}

    def __setattr__(self, key, value):
        """
        Ensure rpc_timeout attribute gets update with timeout. Currently keeping self.timeout and
        self.rpc_timeout for backwards compatibility. We should move towards using rpc_timeout only.

        :param key: attribute key
        :param value: attribute value
        :return:
        """
        if key == 'timeout':
            self.rpc_timeout = value
        super().__setattr__(key, value)

    async def call_async(self, method_name: str, *args, rpc_timeout: float = None, **kwargs):
        """
        Send JSON RPC request to a backend socket and receive reply (asynchronously)

        :param method_name: Method name
        :param args: Args that will be passed to the remote function
        :param float rpc_timeout: Timeout in seconds for Server response, set to None to disable the timeout
        :param kwargs: Keyword args that will be passed to the remote function
        """
        # if an rpc_timeout override is not specified, use the one set in the Client attributes
        if rpc_timeout is None:
            rpc_timeout = self.rpc_timeout

        if rpc_timeout:
            # Implementation note: this simply wraps the call in a timeout and converts to the built-in TimeoutError
            try:
                return await asyncio.wait_for(self._call_async(method_name, *args, **kwargs), timeout=rpc_timeout)
            except asyncio.TimeoutError:
                raise TimeoutError(f"Timeout on client {self.endpoint}, method name {method_name}, class info: {self}")
        else:
            return await self._call_async(method_name, *args, **kwargs)

    async def _call_async(self, method_name: str, *args, **kwargs):
        """
        Sends a request to the socket and then wait for the reply.

        To deal with multiple, asynchronous requests we do not expect that the receive reply task
        scheduled from this call is the one that receives this call's reply and instead rely on
        Events to signal across multiple _async_call/_recv_reply tasks.
        """
        request = rpc_request(method_name, *args, **kwargs)
        _log.debug("Sending request: %s", request)

        # setup an event to notify us when the reply is received (potentially by a task scheduled by
        # another call to _async_call). we do this before we send the request to catch the case
        # where the reply comes back before we re-enter this thread
        self._events[request.id] = asyncio.Event()

        # schedule a task to receive the reply to ensure we have a task to receive the reply
        asyncio.ensure_future(self._recv_reply())

        await self._async_socket.send_multipart([to_msgpack(request)])
        await self._events[request.id].wait()

        reply = self._replies.pop(request.id)
        if isinstance(reply, RPCError):
            raise RPCErrorError(reply.error)
        else:
            return reply.result

    async def _recv_reply(self):
        """
        Helper task to recieve a reply store the result and trigger the associated event.
        """
        raw_reply, = await self._async_socket.recv_multipart()
        reply = from_msgpack(raw_reply)
        _log.debug("Received reply: %s", reply)
        self._replies[reply.id] = reply
        self._events.pop(reply.id).set()

    def call(self, method_name: str, *args, rpc_timeout: float = None, **kwargs):
        """
        Send JSON RPC request to a backend socket and receive reply
        Note that this uses the default event loop to run in a blocking manner. If you would rather run in an async
        fashion or provide your own event loop then use .async_call instead

        :param method_name: Method name
        :param args: Args that will be passed to the remote function
        :param float rpc_timeout: Timeout in seconds for Server response, set to None to disable the timeout
        :param kwargs: Keyword args that will be passed to the remote function
        """
        request = rpc_request(method_name, *args, **kwargs)
        _log.debug("Sending request: %s", request)

        self._socket.send_multipart([to_msgpack(request)])

        # if an rpc_timeout override is not specified, use the one set in the Client attributes
        if rpc_timeout is None:
            rpc_timeout = self.rpc_timeout

        start_time = time.time()
        while True:
            # Need to keep track of timeout manually in case this loop runs more than once. We subtract off already
            # elapsed time from the timeout. The call to max is to make sure we don't send a negative value
            # which would throw an error.
            timeout = max((start_time + rpc_timeout - time.time()) * 1000, 0) if rpc_timeout is not None else None
            if self._socket.poll(timeout) == 0:
                raise TimeoutError(f"Timeout on client {self.endpoint}, method name {method_name}, class info: {self}")

            raw_reply, = self._socket.recv_multipart()
            reply = from_msgpack(raw_reply)
            _log.debug("Received reply: %s", reply)

            # there's a possibility that the socket will have some leftover replies from a previous
            # request on it if that .call() was cancelled or timed out. Therefore, we need to discard replies that
            # don't match the request just like in the call_async case.
            if reply.id == request.id:
                break
            else:
                _log.debug('Discarding reply: %s', reply)

        if isinstance(reply, RPCError):
            raise RPCErrorError(reply.error)
        else:
            return reply.result

    def close(self):
        """
        Close the sockets
        """
        self._socket.close()
        if self._async_socket_cache:
            self._async_socket_cache.close()
            self._async_socket_cache = None

    def _connect_to_socket(self, context: zmq.Context, endpoint: str):
        """
        Connect to a DEALER socket at endpoint and turn off lingering.

        :param context: ZMQ Context to use (potentially async)
        :param endpoint: Endpoint
        :return: Connected socket
        """
        socket = context.socket(zmq.DEALER)
        socket.connect(endpoint)
        socket.setsockopt(zmq.LINGER, 0)
        _log.debug("Client connected to endpoint %s", self.endpoint)
        return socket

    @property
    def _async_socket(self):
        """
        Creates a new async socket if one doesn't already exist for this Client
        """
        if not self._async_socket_cache:
            self._async_socket_cache = self._connect_to_socket(zmq.asyncio.Context(), self.endpoint)

        return self._async_socket_cache
