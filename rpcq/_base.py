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
import rapidjson
from copy import deepcopy
from ruamel import yaml

import msgpack

REPR_LIST_TRUNCATION = 10
"Number of list elements to print when calling repr on a Message with a list field."


def repr_value(value):
    """
    Represent a value in human readable form. For long list's this truncates the printed
    representation.

    :param value: The value to represent.
    :return: A string representation.
    :rtype: basestring
    """
    if isinstance(value, list) and len(value) > REPR_LIST_TRUNCATION:
        return "[{},...]".format(", ".join(map(repr, value[:REPR_LIST_TRUNCATION])))
    else:
        return repr(value)


class UnknownMessageType(Exception):
    """Raised when trying to decode an unknown message type."""


class Message(object):
    """
    Base class for messages.
    """

    def __repr__(self):
        return "{}({})".format(
            self.__class__.__name__,
            ", ".join("{}={}".format(k, repr_value(v))
                      for k, v in sorted(self.asdict().items(), key=lambda kv: kv[0])))

    def __getitem__(self, item):
        return self.asdict()[item]

    def items(self):
        return self.asdict().items()

    def get(self, key, default):
        return self.asdict().get(key, default)

    def __eq__(self, other):
        return type(self) == type(other) and self.astuple() == other.astuple()

    def __copy__(self):
        return self.__class__(**self.asdict())

    def __deepcopy__(self, memo):
        ret = self.__class__(**deepcopy(self.asdict(), memo=memo))
        memo[id(self)] = ret
        return ret

    def copy(self, **kwargs):
        """
        Create a copy with (optionally) substituted values.

        :param kwargs: Any fields that should be substituted.
        :return: A new message object of the same class.
        """
        values = self.asdict()
        values.update(kwargs)
        return self.__class__(**values)

    def asdict(self):
        """
        Create a dictionary ``{fieldname1: fieldvalue1, ...}`` of the Message object.

        :return: A dictionary representation of the message.
        :rtype: Dict[str,Any]
        """
        raise NotImplementedError(self.__class__.__name__)

    def astuple(self):
        """
        Create a tuple ``{fieldvalue1, ...}`` of the Message object.

        :return: A tuple representation of the message.
        :rtype: Tuple[Any]
        """
        raise NotImplementedError(self.__class__.__name__)

    def __hash__(self):
        return hash((self.__class__, self.astuple()))

    def replace(self, **kwargs):
        """
        Return a copy of the message object where the fields given in kwargs are
        replaced.

        :param kwargs: The replaced fields.
        :return: A copy of self.
        """
        d = self.asdict()
        d.update(kwargs)
        return type(self)(**d)

    _types = None

    @staticmethod
    def types():
        """
        Return a mapping ``{type_name: message_type}`` for all defined Message's.

        :return: A dictionary of ``Message`` types.
        :rtype: Dict[str,type]
        """
        if Message._types is None:
            Message._types = {c.__name__: c for c in Message.__subclasses__()}
        return Message._types


def _default(obj):
    if isinstance(obj, Message):
        d = obj.asdict()
        d["_type"] = obj.__class__.__name__
        return d
    else:
        raise TypeError('Object of type {} is not JSON serializable'.format(obj.__class__.__name__))


def _object_hook(obj):
    if '_type' in obj:
        try:
            msg_type = Message.types()[obj["_type"]]
        except KeyError:  # pragma no coverage
            raise UnknownMessageType("The message type {} is unknown".format(obj["_type"]))

        itms = {k: v for k, v in obj.items() if k != "_type"}
        return msg_type(**itms)
    else:
        return obj


def to_msgpack(obj):
    """
    Convert Python objects (including rpcq objects) to a msgpack byte array
    :rtype: bytes
    """
    # Docs for `use_bin_type` parameter are somewhat hard to find so they're copied here:
    #   Use bin type introduced in msgpack spec 2.0 for bytes.
    #   It also enables str8 type for unicode.
    return msgpack.dumps(obj, default=_default, use_bin_type=True)


def from_msgpack(b):
    """
    Convert a msgpack byte array into Python objects (including rpcq objects)
    """
    # Docs for raw parameter are somewhat hard to find so they're copied here:
    #   If true, unpack msgpack raw to Python bytes (default).
    #   Otherwise, unpack to Python str (or unicode on Python 2) by decoding with UTF-8 encoding (recommended).
    return msgpack.loads(b, object_hook=_object_hook, raw=False)


def to_json(obj):
    """
    Convert Python objects (including rpcq objects) to a JSON string.
    :rtype: str
    """
    return rapidjson.dumps(obj, default=_default)


def from_json(s):
    """
    Convert a JSON string into Python objects (including rpcq objects).
    """
    return rapidjson.loads(s, object_hook=_object_hook)


def to_yaml_file(obj, f):
    """
    Convert Python objects (including rpcq messages) to yaml and write it to `f`.
    """
    yaml.dump(rapidjson.loads(to_json(obj)), f)


def from_yaml_file(f):
    """
    Read a yaml file and convert to Python objects (including rpcq messages).
    """
    return from_json(to_json(yaml.load(f, Loader=yaml.Loader)))
