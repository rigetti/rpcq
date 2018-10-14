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
"""
Server that accepts JSON RPC requests and returns JSON RPC replies/errors.
"""
import asyncio
import logging
from asyncio import AbstractEventLoop
from typing import Callable

import zmq.asyncio

from rpcq._base import to_msgpack, from_msgpack
from rpcq._spec import RPCSpec
from rpcq.messages import RPCRequest

_log = logging.getLogger(__name__)


class Server:
    """
    Server that accepts JSON RPC calls through a socket.
    """
    def __init__(self, rpc_spec: RPCSpec = None):
        """
        Create a server that will be linked to a socket

        :param rpc_spec: JSON RPC spec
        """
        self.rpc_spec = rpc_spec if rpc_spec else RPCSpec()
        self._exit_handlers = []

        self._socket = None

    def rpc_handler(self, f: Callable):
        """
        Add a function to the server. It will respond to JSON RPC requests with the corresponding method name.
        This can be used as both a side-effecting function or as a decorator.

        :param f: Function to add
        :return: Function wrapper (so it can be used as a decorator)
        """
        return self.rpc_spec.add_handler(f)

    def exit_handler(self, f: Callable):
        """
        Add an exit handler - a function which will be called when the server shuts down.

        :param f: Function to add
        """
        self._exit_handlers.append(f)

    async def run_async(self, endpoint: str):
        """
        Run server main task (asynchronously).

        :param endpoint: Socket endpoint to listen to, e.g. "tcp://*:1234"
        """
        self._connect(endpoint)

        while True:
            try:
                # empty_frame may either be:
                # 1. a single null frame if the client is a REQ socket
                # 2. an empty list (ie. no frames) if the client is a DEALER socket
                identity, *empty_frame, msg = await self._socket.recv_multipart()
                request = from_msgpack(msg)

                asyncio.ensure_future(self._process_request(identity, empty_frame, request))
            except Exception:
                _log.exception('Exception thrown in Server run loop')

    def run(self, endpoint: str, loop: AbstractEventLoop = None):
        """
        Run server main task.

        :param endpoint: Socket endpoint to listen to, e.g. "tcp://*:1234"
        :param loop: Event loop to run server in (alternatively just use run_async method)
        """
        if not loop:
            loop = asyncio.get_event_loop()

        try:
            loop.run_until_complete(self.run_async(endpoint))
        except KeyboardInterrupt:
            self._shutdown()

    def stop(self):
        """
        DEPRECATED
        """
        pass

    def _shutdown(self):
        """
        Shut down the server.
        """
        for exit_handler in self._exit_handlers:
            exit_handler()

        if self._socket:
            self._socket.close()
            self._socket = None

    def _connect(self, endpoint: str):
        """
        Connect the server to an endpoint. Creates a ZMQ ROUTER socket for the given endpoint.

        :param endpoint: Socket endpoint, e.g. "tcp://*:1234"
        """
        if self._socket:
            raise RuntimeError('Cannot run multiple Servers on the same socket')

        context = zmq.asyncio.Context()
        self._socket = context.socket(zmq.ROUTER)
        self._socket.bind(endpoint)

        _log.info("Starting server, listening on endpoint {}".format(endpoint))

    async def _process_request(self, identity: bytes, empty_frame: list, request: RPCRequest):
        """
        Executes the method specified in a JSON RPC request and then sends the reply to the socket.

        :param identity: Client identity provided by ZeroMQ
        :param empty_frame: Either an empty list or a single null frame depending on the client type
        :param request: JSON RPC request
        """
        try:
            _log.debug("Client %s sent request: %s", identity, request)
            reply = await self.rpc_spec.run_handler(request)

            _log.debug("Sending client %s reply: %s", identity, reply)
            await self._socket.send_multipart([identity, *empty_frame, to_msgpack(reply)])
        except Exception:
            _log.exception('Exception thrown in _process_request')
