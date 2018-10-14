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
"""Utils for message passing"""
import uuid
from typing import Optional, Tuple, Union

from rpcq.messages import RPCRequest, RPCReply, RPCError


def rpc_request(method_name: str, *args, **kwargs) -> RPCRequest:
    """
    Create RPC request

    :param method_name: Method name
    :param args: Positional arguments
    :param kwargs: Keyword arguments
    :return: JSON RPC formatted dict
    """
    if args:
        kwargs['*args'] = args

    return RPCRequest(
        jsonrpc='2.0',
        id=str(uuid.uuid4()),
        method=method_name,
        params=kwargs
    )


def rpc_reply(id: Union[str, int], result: Optional[object]) -> RPCReply:
    """
    Create RPC reply

    :param str|int id: Request ID
    :param result: Result
    :return: JSON RPC formatted dict
    """
    return RPCReply(
        jsonrpc='2.0',
        id=id,
        result=result
    )


def rpc_error(id: Union[str, int], error_msg: str) -> RPCError:
    """
    Create RPC error

    :param id: Request ID
    :param error_msg: Error message
    :return: JSON RPC formatted dict
    """
    return RPCError(
        jsonrpc='2.0',
        id=id,
        error=error_msg)


def get_input(params: Union[dict, list]) -> Tuple[list, dict]:
    """
    Get positional or keyword arguments from JSON RPC params

    :param params: Parameters passed through JSON RPC
    :return: args, kwargs
    """
    # Backwards compatibility for old clients that send params as a list
    if isinstance(params, list):
        args = params
        kwargs = {}
    elif isinstance(params, dict):
        args = params.pop('*args', [])
        kwargs = params
    else:  # pragma no coverage
        raise TypeError(
            'Unknown type {} of params, must be list or dict'.format(type(params)))

    return args, kwargs


class RPCErrorError(IOError):
    """JSON RPC error that is raised by a Client when it receives an RPCError message"""


class RPCMethodError(AttributeError):
    """JSON RPC error that is raised by JSON RPC spec for nonexistent methods"""

