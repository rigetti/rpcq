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
import time

import pytest

from rpcq.messages import RPCError
from rpcq._spec import RPCSpec
from rpcq._utils import rpc_request


class MyClass(object):
    classvar = 4

    def __init__(self, num):
        self.num = num

    async def add(self, *args):
        asyncio.sleep(0.1)
        return sum(args) + self.num

    def blocking_add(self, *args):
        time.sleep(0.1)
        return sum(args) + self.num

    @classmethod
    def add_to_classvar(cls, num):
        return cls.classvar + num


def foo():
    return 'bar'


def two_params(a: int, b: int):
    return a * b


obj = MyClass(5)

json_rpc_spec = RPCSpec()
json_rpc_spec.add_handler(obj.add)
json_rpc_spec.add_handler(obj.blocking_add)
json_rpc_spec.add_handler(obj.add_to_classvar)
json_rpc_spec.add_handler(foo)
json_rpc_spec.add_handler(two_params)


@pytest.mark.asyncio
async def test_json_rpc_call():
    request = rpc_request("add", 1, 2)

    # Execute method on object
    reply = await json_rpc_spec.run_handler(request)

    assert reply.result == 8
    assert reply.id == request.id

    request2 = rpc_request("add", 1)

    # Execute method on object
    reply2 = await json_rpc_spec.run_handler(request2)

    assert reply2.result == 6
    assert reply2.id == request2.id

    request3 = rpc_request("bad_name", 1)

    # Execute method on object
    reply3 = await json_rpc_spec.run_handler(request3)
    assert isinstance(reply3, RPCError)


@pytest.mark.asyncio
async def test_blocking_json_rpc_call():
    request = rpc_request("blocking_add", 1, 2)
    reply = await json_rpc_spec.run_handler(request)

    assert reply.result == 8
    assert reply.id == request.id


@pytest.mark.asyncio
async def test_json_rpc_function():
    request = rpc_request("foo")
    reply = await json_rpc_spec.run_handler(request)

    assert reply["result"] == "bar"


@pytest.mark.asyncio
async def test_json_rpc_classmethod_call():
    request = rpc_request("add_to_classvar", 1)
    reply = await json_rpc_spec.run_handler(request)

    assert reply["result"] == 5


@pytest.mark.asyncio
async def test_mixed_args_kwargs():
    request = rpc_request('two_params', 2, 3)
    reply = await json_rpc_spec.run_handler(request)
    assert reply["result"] == 6

    request = rpc_request('two_params', 2, b=3)
    reply = await json_rpc_spec.run_handler(request)
    assert reply["result"] == 6

    request = rpc_request('two_params', a=2, b=3)
    reply = await json_rpc_spec.run_handler(request)
    assert reply["result"] == 6
