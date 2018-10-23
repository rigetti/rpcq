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
from __future__ import print_function
import logging

import pytest
from rpcq.messages import (RPCError)

log = logging.getLogger(__file__)


def test_messages():
    e = "Error"
    i = "asefa32423"
    m = RPCError(error=e, id=i)
    assert m.error == e
    assert m.id == i
    assert m.jsonrpc == "2.0"

    assert m['error'] == e
    assert m.get('error', 1) == e

    assert m.asdict() == {"error": e,
                        "id": i,
                        "jsonrpc": "2.0"}

    with pytest.raises(TypeError):
        RPCError(bad_field=1)
