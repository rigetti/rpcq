#!/usr/bin/env python

"""
WARNING: This file is auto-generated, do not edit by hand. See README.md.
"""

import warnings
from rpcq._base import Message
from typing import List, Dict, Optional

# Python 2/3 str/unicode compatibility
from past.builtins import basestring


class CoreMessages(object):
    """
    WARNING: This class is auto-generated, do not edit by hand. See README.md.
    This class is also DEPRECATED.
    """


class _deprecated_property(object):

    def __init__(self, prop):
        self.prop = prop

    def __get__(self, *args):
        warnings.warn(
            "'CoreMessages.{0}' is deprecated. Please access '{0}' directly at the module level.".format(
                self.prop.__name__),
            UserWarning)
        return self.prop


class ParameterSpec(Message):
    """Specification of a dynamic parameter type and array-length."""

    # fix slots
    __slots__ = (
        'type',
        'length',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'type': self.type,
            'length': self.length
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.type,
            self.length
        )

    def __init__(self,
                 type=None,
                 length=1,
                 **kwargs):
        # type: (str, int) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if type is None:
            raise ValueError("The field 'type' cannot be None")
        if length is None:
            raise ValueError("The field 'length' cannot be None")

        # verify types
        if not isinstance(type, basestring):
            raise TypeError("Parameter type must be of type basestring, "
                            + "but object of type {} given".format(type(type)))
        if not isinstance(length, int):
            raise TypeError("Parameter length must be of type int, "
                            + "but object of type {} given".format(type(length)))

        self.type = type  # type: str
        """The parameter type, e.g., one of 'INTEGER', or 'FLOAT'."""

        self.length = length  # type: int
        """If this is not 1, the parameter is an array of this length."""

CoreMessages.ParameterSpec = _deprecated_property(ParameterSpec)

class ParameterAref(Message):
    """A parametric expression."""

    # fix slots
    __slots__ = (
        'name',
        'index',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'name': self.name,
            'index': self.index
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.name,
            self.index
        )

    def __init__(self,
                 name,
                 index,
                 **kwargs):
        # type: (str, int) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if name is None:
            raise ValueError("The field 'name' cannot be None")
        if index is None:
            raise ValueError("The field 'index' cannot be None")

        # verify types
        if not isinstance(name, basestring):
            raise TypeError("Parameter name must be of type basestring, "
                            + "but object of type {} given".format(type(name)))
        if not isinstance(index, int):
            raise TypeError("Parameter index must be of type int, "
                            + "but object of type {} given".format(type(index)))

        self.name = name  # type: str
        """The parameter name"""

        self.index = index  # type: int
        """The array index."""

CoreMessages.ParameterAref = _deprecated_property(ParameterAref)

class PatchTarget(Message):
    """Patchable memory location descriptor."""

    # fix slots
    __slots__ = (
        'patch_type',
        'patch_offset',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'patch_type': self.patch_type,
            'patch_offset': self.patch_offset
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.patch_type,
            self.patch_offset
        )

    def __init__(self,
                 patch_type,
                 patch_offset,
                 **kwargs):
        # type: (ParameterSpec, int) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if patch_type is None:
            raise ValueError("The field 'patch_type' cannot be None")
        if patch_offset is None:
            raise ValueError("The field 'patch_offset' cannot be None")

        # verify types
        if not isinstance(patch_type, ParameterSpec):
            raise TypeError("Parameter patch_type must be of type ParameterSpec, "
                            + "but object of type {} given".format(type(patch_type)))
        if not isinstance(patch_offset, int):
            raise TypeError("Parameter patch_offset must be of type int, "
                            + "but object of type {} given".format(type(patch_offset)))

        self.patch_type = patch_type  # type: ParameterSpec
        """Data type at this address."""

        self.patch_offset = patch_offset  # type: int
        """Memory address of the patch."""

CoreMessages.PatchTarget = _deprecated_property(PatchTarget)

class RPCRequest(Message):
    """A single request object according to the JSONRPC standard."""

    # fix slots
    __slots__ = (
        'jsonrpc',
        'method',
        'params',
        'id',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'jsonrpc': self.jsonrpc,
            'method': self.method,
            'params': self.params,
            'id': self.id
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.jsonrpc,
            self.method,
            self.params,
            self.id
        )

    def __init__(self,
                 method,
                 params,
                 id,
                 jsonrpc="2.0",
                 **kwargs):
        # type: (str, object, str, str) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if jsonrpc is None:
            raise ValueError("The field 'jsonrpc' cannot be None")
        if method is None:
            raise ValueError("The field 'method' cannot be None")
        if params is None:
            raise ValueError("The field 'params' cannot be None")
        if id is None:
            raise ValueError("The field 'id' cannot be None")

        # verify types
        if not isinstance(jsonrpc, basestring):
            raise TypeError("Parameter jsonrpc must be of type basestring, "
                            + "but object of type {} given".format(type(jsonrpc)))
        if not isinstance(method, basestring):
            raise TypeError("Parameter method must be of type basestring, "
                            + "but object of type {} given".format(type(method)))
        if not isinstance(params, object):
            raise TypeError("Parameter params must be of type object, "
                            + "but object of type {} given".format(type(params)))
        if not isinstance(id, basestring):
            raise TypeError("Parameter id must be of type basestring, "
                            + "but object of type {} given".format(type(id)))

        self.jsonrpc = jsonrpc  # type: str
        """The JSONRPC version."""

        self.method = method  # type: str
        """The RPC function name."""

        self.params = params  # type: object
        """The RPC function arguments."""

        self.id = id  # type: str
        """RPC request id (used to verify that request and response belong together)."""

CoreMessages.RPCRequest = _deprecated_property(RPCRequest)

class RPCReply(Message):
    """The reply for a JSONRPC request."""

    # fix slots
    __slots__ = (
        'jsonrpc',
        'result',
        'id',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'jsonrpc': self.jsonrpc,
            'result': self.result,
            'id': self.id
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.jsonrpc,
            self.result,
            self.id
        )

    def __init__(self,
                 id,
                 jsonrpc="2.0",
                 result=None,
                 **kwargs):
        # type: (str, str, Optional[object]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if jsonrpc is None:
            raise ValueError("The field 'jsonrpc' cannot be None")
        if id is None:
            raise ValueError("The field 'id' cannot be None")

        # verify types
        if not isinstance(jsonrpc, basestring):
            raise TypeError("Parameter jsonrpc must be of type basestring, "
                            + "but object of type {} given".format(type(jsonrpc)))
        if not (result is None or isinstance(result, object)):
            raise TypeError("Parameter result must be of type object, "
                            + "but object of type {} given".format(type(result)))
        if not isinstance(id, basestring):
            raise TypeError("Parameter id must be of type basestring, "
                            + "but object of type {} given".format(type(id)))

        self.jsonrpc = jsonrpc  # type: str
        """The JSONRPC version."""

        self.result = result  # type: Optional[object]
        """The RPC result."""

        self.id = id  # type: str
        """The RPC request id."""

CoreMessages.RPCReply = _deprecated_property(RPCReply)

class RPCError(Message):
    """A error message for JSONRPC requests."""

    # fix slots
    __slots__ = (
        'jsonrpc',
        'error',
        'id',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'jsonrpc': self.jsonrpc,
            'error': self.error,
            'id': self.id
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.jsonrpc,
            self.error,
            self.id
        )

    def __init__(self,
                 error,
                 id,
                 jsonrpc="2.0",
                 **kwargs):
        # type: (str, str, str) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if jsonrpc is None:
            raise ValueError("The field 'jsonrpc' cannot be None")
        if error is None:
            raise ValueError("The field 'error' cannot be None")
        if id is None:
            raise ValueError("The field 'id' cannot be None")

        # verify types
        if not isinstance(jsonrpc, basestring):
            raise TypeError("Parameter jsonrpc must be of type basestring, "
                            + "but object of type {} given".format(type(jsonrpc)))
        if not isinstance(error, basestring):
            raise TypeError("Parameter error must be of type basestring, "
                            + "but object of type {} given".format(type(error)))
        if not isinstance(id, basestring):
            raise TypeError("Parameter id must be of type basestring, "
                            + "but object of type {} given".format(type(id)))

        self.jsonrpc = jsonrpc  # type: str
        """The JSONRPC version."""

        self.error = error  # type: str
        """The error message."""

        self.id = id  # type: str
        """The RPC request id."""

CoreMessages.RPCError = _deprecated_property(RPCError)

class TargetDevice(Message):
    """ISA and specs for a particular device."""

    # fix slots
    __slots__ = (
        'isa',
        'specs',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'isa': self.isa,
            'specs': self.specs
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.isa,
            self.specs
        )

    def __init__(self,
                 isa,
                 specs,
                 **kwargs):
        # type: (Dict[str,dict], Dict[str,dict]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if isa is None:
            raise ValueError("The field 'isa' cannot be None")
        if specs is None:
            raise ValueError("The field 'specs' cannot be None")

        # verify types
        if not isinstance(isa, dict):
            raise TypeError("Parameter isa must be of type dict, "
                            + "but object of type {} given".format(type(isa)))
        if not isinstance(specs, dict):
            raise TypeError("Parameter specs must be of type dict, "
                            + "but object of type {} given".format(type(specs)))

        self.isa = isa  # type: Dict[str,dict]
        """Instruction-set architecture for this device."""

        self.specs = specs  # type: Dict[str,dict]
        """Fidelities and coherence times for this device."""

CoreMessages.TargetDevice = _deprecated_property(TargetDevice)

class RandomizedBenchmarkingRequest(Message):
    """RPC request payload for generating a randomized benchmarking sequence."""

    # fix slots
    __slots__ = (
        'depth',
        'qubits',
        'gateset',
        'seed',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'depth': self.depth,
            'qubits': self.qubits,
            'gateset': self.gateset,
            'seed': self.seed
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.depth,
            self.qubits,
            self.gateset,
            self.seed
        )

    def __init__(self,
                 depth,
                 qubits,
                 gateset,
                 seed=None,
                 **kwargs):
        # type: (int, int, List[str], Optional[int]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if depth is None:
            raise ValueError("The field 'depth' cannot be None")
        if qubits is None:
            raise ValueError("The field 'qubits' cannot be None")
        if gateset is None:
            raise ValueError("The field 'gateset' cannot be None")

        # verify types
        if not isinstance(depth, int):
            raise TypeError("Parameter depth must be of type int, "
                            + "but object of type {} given".format(type(depth)))
        if not isinstance(qubits, int):
            raise TypeError("Parameter qubits must be of type int, "
                            + "but object of type {} given".format(type(qubits)))
        if not isinstance(gateset, list):
            raise TypeError("Parameter gateset must be of type list, "
                            + "but object of type {} given".format(type(gateset)))
        if not (seed is None or isinstance(seed, int)):
            raise TypeError("Parameter seed must be of type int, "
                            + "but object of type {} given".format(type(seed)))

        self.depth = depth  # type: int
        """Depth of the benchmarking sequence."""

        self.qubits = qubits  # type: int
        """Number of qubits involved in the benchmarking sequence."""

        self.gateset = gateset  # type: List[str]
        """List of Quil programs, each describing a Clifford."""

        self.seed = seed  # type: Optional[int]
        """PRNG seed. Set this to guarantee repeatable results."""

CoreMessages.RandomizedBenchmarkingRequest = _deprecated_property(RandomizedBenchmarkingRequest)

class RandomizedBenchmarkingResponse(Message):
    """RPC reply payload for a randomly generated benchmarking sequence."""

    # fix slots
    __slots__ = (
        'sequence',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'sequence': self.sequence
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.sequence
        )

    def __init__(self,
                 sequence,
                 **kwargs):
        # type: (List[List[int]]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if sequence is None:
            raise ValueError("The field 'sequence' cannot be None")

        # verify types
        if not isinstance(sequence, list):
            raise TypeError("Parameter sequence must be of type list, "
                            + "but object of type {} given".format(type(sequence)))

        self.sequence = sequence  # type: List[List[int]]
        """List of Cliffords, each expressed as a list of generator indices."""

CoreMessages.RandomizedBenchmarkingResponse = _deprecated_property(RandomizedBenchmarkingResponse)

class PauliTerm(Message):
    """Specification of a single Pauli term as a tensor product of Pauli factors."""

    # fix slots
    __slots__ = (
        'indices',
        'symbols',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'indices': self.indices,
            'symbols': self.symbols
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.indices,
            self.symbols
        )

    def __init__(self,
                 indices,
                 symbols,
                 **kwargs):
        # type: (List[int], List[str]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if indices is None:
            raise ValueError("The field 'indices' cannot be None")
        if symbols is None:
            raise ValueError("The field 'symbols' cannot be None")

        # verify types
        if not isinstance(indices, list):
            raise TypeError("Parameter indices must be of type list, "
                            + "but object of type {} given".format(type(indices)))
        if not isinstance(symbols, list):
            raise TypeError("Parameter symbols must be of type list, "
                            + "but object of type {} given".format(type(symbols)))

        self.indices = indices  # type: List[int]
        """Qubit indices onto which the factors of a Pauli term are applied."""

        self.symbols = symbols  # type: List[str]
        """Ordered factors of a Pauli term."""

CoreMessages.PauliTerm = _deprecated_property(PauliTerm)

class ConjugateByCliffordRequest(Message):
    """RPC request payload for conjugating a Pauli element by a Clifford element."""

    # fix slots
    __slots__ = (
        'pauli',
        'clifford',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'pauli': self.pauli,
            'clifford': self.clifford
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.pauli,
            self.clifford
        )

    def __init__(self,
                 pauli,
                 clifford,
                 **kwargs):
        # type: (PauliTerm, str) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if pauli is None:
            raise ValueError("The field 'pauli' cannot be None")
        if clifford is None:
            raise ValueError("The field 'clifford' cannot be None")

        # verify types
        if not isinstance(pauli, PauliTerm):
            raise TypeError("Parameter pauli must be of type PauliTerm, "
                            + "but object of type {} given".format(type(pauli)))
        if not isinstance(clifford, basestring):
            raise TypeError("Parameter clifford must be of type basestring, "
                            + "but object of type {} given".format(type(clifford)))

        self.pauli = pauli  # type: PauliTerm
        """Specification of a Pauli element."""

        self.clifford = clifford  # type: str
        """Specification of a Clifford element."""

CoreMessages.ConjugateByCliffordRequest = _deprecated_property(ConjugateByCliffordRequest)

class ConjugateByCliffordResponse(Message):
    """RPC reply payload for a Pauli element as conjugated by a Clifford element."""

    # fix slots
    __slots__ = (
        'phase',
        'pauli',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'phase': self.phase,
            'pauli': self.pauli
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.phase,
            self.pauli
        )

    def __init__(self,
                 phase,
                 pauli,
                 **kwargs):
        # type: (int, str) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if phase is None:
            raise ValueError("The field 'phase' cannot be None")
        if pauli is None:
            raise ValueError("The field 'pauli' cannot be None")

        # verify types
        if not isinstance(phase, int):
            raise TypeError("Parameter phase must be of type int, "
                            + "but object of type {} given".format(type(phase)))
        if not isinstance(pauli, basestring):
            raise TypeError("Parameter pauli must be of type basestring, "
                            + "but object of type {} given".format(type(pauli)))

        self.phase = phase  # type: int
        """Encoded global phase factor on the emitted Pauli."""

        self.pauli = pauli  # type: str
        """Description of the encoded Pauli."""

CoreMessages.ConjugateByCliffordResponse = _deprecated_property(ConjugateByCliffordResponse)

class NativeQuilRequest(Message):
    """Quil and the device metadata necessary for quilc."""

    # fix slots
    __slots__ = (
        'quil',
        'target_device',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'quil': self.quil,
            'target_device': self.target_device
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.quil,
            self.target_device
        )

    def __init__(self,
                 quil,
                 target_device,
                 **kwargs):
        # type: (str, TargetDevice) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if quil is None:
            raise ValueError("The field 'quil' cannot be None")
        if target_device is None:
            raise ValueError("The field 'target_device' cannot be None")

        # verify types
        if not isinstance(quil, basestring):
            raise TypeError("Parameter quil must be of type basestring, "
                            + "but object of type {} given".format(type(quil)))
        if not isinstance(target_device, TargetDevice):
            raise TypeError("Parameter target_device must be of type TargetDevice, "
                            + "but object of type {} given".format(type(target_device)))

        self.quil = quil  # type: str
        """Arbitrary Quil to be sent to quilc."""

        self.target_device = target_device  # type: TargetDevice
        """Specifications for the device to target with quilc."""

CoreMessages.NativeQuilRequest = _deprecated_property(NativeQuilRequest)

class NativeQuilMetadata(Message):
    """Metadata for a native quil program."""

    # fix slots
    __slots__ = (
        'final_rewiring',
        'gate_depth',
        'gate_volume',
        'multiqubit_gate_depth',
        'program_duration',
        'program_fidelity',
        'topological_swaps',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'final_rewiring': self.final_rewiring,
            'gate_depth': self.gate_depth,
            'gate_volume': self.gate_volume,
            'multiqubit_gate_depth': self.multiqubit_gate_depth,
            'program_duration': self.program_duration,
            'program_fidelity': self.program_fidelity,
            'topological_swaps': self.topological_swaps
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.final_rewiring,
            self.gate_depth,
            self.gate_volume,
            self.multiqubit_gate_depth,
            self.program_duration,
            self.program_fidelity,
            self.topological_swaps
        )

    def __init__(self,
                 final_rewiring=None,
                 gate_depth=None,
                 gate_volume=None,
                 multiqubit_gate_depth=None,
                 program_duration=None,
                 program_fidelity=None,
                 topological_swaps=None,
                 **kwargs):
        # type: (List[int], Optional[int], Optional[int], Optional[int], Optional[float], Optional[float], Optional[int]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # initialize default values of collections
        if final_rewiring is None:
            final_rewiring = []

        # verify types
        if not (final_rewiring is None or isinstance(final_rewiring, list)):
            raise TypeError("Parameter final_rewiring must be of type list, "
                            + "but object of type {} given".format(type(final_rewiring)))
        if not (gate_depth is None or isinstance(gate_depth, int)):
            raise TypeError("Parameter gate_depth must be of type int, "
                            + "but object of type {} given".format(type(gate_depth)))
        if not (gate_volume is None or isinstance(gate_volume, int)):
            raise TypeError("Parameter gate_volume must be of type int, "
                            + "but object of type {} given".format(type(gate_volume)))
        if not (multiqubit_gate_depth is None or isinstance(multiqubit_gate_depth, int)):
            raise TypeError("Parameter multiqubit_gate_depth must be of type int, "
                            + "but object of type {} given".format(type(multiqubit_gate_depth)))
        if not (program_duration is None or isinstance(program_duration, float)):
            raise TypeError("Parameter program_duration must be of type float, "
                            + "but object of type {} given".format(type(program_duration)))
        if not (program_fidelity is None or isinstance(program_fidelity, float)):
            raise TypeError("Parameter program_fidelity must be of type float, "
                            + "but object of type {} given".format(type(program_fidelity)))
        if not (topological_swaps is None or isinstance(topological_swaps, int)):
            raise TypeError("Parameter topological_swaps must be of type int, "
                            + "but object of type {} given".format(type(topological_swaps)))

        self.final_rewiring = final_rewiring  # type: List[int]
        """Output qubit index relabeling due to SWAP insertion."""

        self.gate_depth = gate_depth  # type: Optional[int]
        """Maximum number of successive gates in the native quil program."""

        self.gate_volume = gate_volume  # type: Optional[int]
        """Total number of gates in the native quil program."""

        self.multiqubit_gate_depth = multiqubit_gate_depth  # type: Optional[int]
        """Maximum number of successive two-qubit gates in the native quil program."""

        self.program_duration = program_duration  # type: Optional[float]
        """Rough estimate of native quil program length in nanoseconds."""

        self.program_fidelity = program_fidelity  # type: Optional[float]
        """Rough estimate of the fidelity of the full native quil program, uses specs."""

        self.topological_swaps = topological_swaps  # type: Optional[int]
        """Total number of SWAPs in the native quil program."""

CoreMessages.NativeQuilMetadata = _deprecated_property(NativeQuilMetadata)

class NativeQuilResponse(Message):
    """Native Quil and associated metadata returned from quilc."""

    # fix slots
    __slots__ = (
        'quil',
        'metadata',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'quil': self.quil,
            'metadata': self.metadata
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.quil,
            self.metadata
        )

    def __init__(self,
                 quil,
                 metadata=None,
                 **kwargs):
        # type: (str, Optional[NativeQuilMetadata]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if quil is None:
            raise ValueError("The field 'quil' cannot be None")

        # verify types
        if not isinstance(quil, basestring):
            raise TypeError("Parameter quil must be of type basestring, "
                            + "but object of type {} given".format(type(quil)))
        if not (metadata is None or isinstance(metadata, NativeQuilMetadata)):
            raise TypeError("Parameter metadata must be of type NativeQuilMetadata, "
                            + "but object of type {} given".format(type(metadata)))

        self.quil = quil  # type: str
        """Native Quil returned from quilc."""

        self.metadata = metadata  # type: Optional[NativeQuilMetadata]
        """Metadata for the returned Native Quil."""

CoreMessages.NativeQuilResponse = _deprecated_property(NativeQuilResponse)

class BinaryExecutableRequest(Message):
    """Native Quil and the information needed to create binary executables."""

    # fix slots
    __slots__ = (
        'quil',
        'num_shots',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'quil': self.quil,
            'num_shots': self.num_shots
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.quil,
            self.num_shots
        )

    def __init__(self,
                 quil,
                 num_shots,
                 **kwargs):
        # type: (str, int) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if quil is None:
            raise ValueError("The field 'quil' cannot be None")
        if num_shots is None:
            raise ValueError("The field 'num_shots' cannot be None")

        # verify types
        if not isinstance(quil, basestring):
            raise TypeError("Parameter quil must be of type basestring, "
                            + "but object of type {} given".format(type(quil)))
        if not isinstance(num_shots, int):
            raise TypeError("Parameter num_shots must be of type int, "
                            + "but object of type {} given".format(type(num_shots)))

        self.quil = quil  # type: str
        """Native Quil to be translated into an executable program."""

        self.num_shots = num_shots  # type: int
        """The number of times to repeat the program."""

CoreMessages.BinaryExecutableRequest = _deprecated_property(BinaryExecutableRequest)

class BinaryExecutableResponse(Message):
    """Program to run on the QPU."""

    # fix slots
    __slots__ = (
        'program',
        'memory_descriptors',
        'ro_sources',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'program': self.program,
            'memory_descriptors': self.memory_descriptors,
            'ro_sources': self.ro_sources
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.program,
            self.memory_descriptors,
            self.ro_sources
        )

    def __init__(self,
                 program,
                 memory_descriptors=None,
                 ro_sources=None,
                 **kwargs):
        # type: (str, Dict[str,ParameterSpec], List[object]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # initialize default values of collections
        if memory_descriptors is None:
            memory_descriptors = {}
        if ro_sources is None:
            ro_sources = []

        # check presence of required fields
        if program is None:
            raise ValueError("The field 'program' cannot be None")

        # verify types
        if not isinstance(program, basestring):
            raise TypeError("Parameter program must be of type basestring, "
                            + "but object of type {} given".format(type(program)))
        if not (memory_descriptors is None or isinstance(memory_descriptors, dict)):
            raise TypeError("Parameter memory_descriptors must be of type dict, "
                            + "but object of type {} given".format(type(memory_descriptors)))
        if not (ro_sources is None or isinstance(ro_sources, list)):
            raise TypeError("Parameter ro_sources must be of type list, "
                            + "but object of type {} given".format(type(ro_sources)))

        self.program = program  # type: str
        """Execution settings and sequencer binaries."""

        self.memory_descriptors = memory_descriptors  # type: Dict[str,ParameterSpec]
        """Internal field for constructing patch tables."""

        self.ro_sources = ro_sources  # type: List[object]
        """Internal field for reshaping returned buffers."""

CoreMessages.BinaryExecutableResponse = _deprecated_property(BinaryExecutableResponse)

class PyQuilExecutableResponse(Message):
    """Pidgin-serializable form of a pyQuil Program object."""

    # fix slots
    __slots__ = (
        'program',
        'attributes',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'program': self.program,
            'attributes': self.attributes
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.program,
            self.attributes
        )

    def __init__(self,
                 program,
                 attributes,
                 **kwargs):
        # type: (str, Dict[str,object]) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if program is None:
            raise ValueError("The field 'program' cannot be None")
        if attributes is None:
            raise ValueError("The field 'attributes' cannot be None")

        # verify types
        if not isinstance(program, basestring):
            raise TypeError("Parameter program must be of type basestring, "
                            + "but object of type {} given".format(type(program)))
        if not isinstance(attributes, dict):
            raise TypeError("Parameter attributes must be of type dict, "
                            + "but object of type {} given".format(type(attributes)))

        self.program = program  # type: str
        """String representation of a Quil program."""

        self.attributes = attributes  # type: Dict[str,object]
        """Miscellaneous attributes to be unpacked onto the pyQuil Program object."""

CoreMessages.PyQuilExecutableResponse = _deprecated_property(PyQuilExecutableResponse)

class QPURequest(Message):
    """Program and patch values to send to the QPU for execution."""

    # fix slots
    __slots__ = (
        'program',
        'patch_values',
        'id',
    )

    def asdict(self):
        """Generate dictionary representation of self."""
        return {
            'program': self.program,
            'patch_values': self.patch_values,
            'id': self.id
        }

    def astuple(self):
        """Generate tuple representation of self."""
        return (
            self.program,
            self.patch_values,
            self.id
        )

    def __init__(self,
                 program,
                 patch_values,
                 id,
                 **kwargs):
        # type: (object, Dict[str,List[object]], str) -> None

        if kwargs:
            warnings.warn(("Message {} ignoring unexpected keyword arguments: "
                    "{}.").format(self.__class__.__name__, ", ".join(kwargs.keys())))

        # check presence of required fields
        if program is None:
            raise ValueError("The field 'program' cannot be None")
        if patch_values is None:
            raise ValueError("The field 'patch_values' cannot be None")
        if id is None:
            raise ValueError("The field 'id' cannot be None")

        # verify types
        if not isinstance(program, object):
            raise TypeError("Parameter program must be of type object, "
                            + "but object of type {} given".format(type(program)))
        if not isinstance(patch_values, dict):
            raise TypeError("Parameter patch_values must be of type dict, "
                            + "but object of type {} given".format(type(patch_values)))
        if not isinstance(id, basestring):
            raise TypeError("Parameter id must be of type basestring, "
                            + "but object of type {} given".format(type(id)))

        self.program = program  # type: object
        """Execution settings and sequencer binaries."""

        self.patch_values = patch_values  # type: Dict[str,List[object]]
        """Dictionary mapping data names to data values for patching the binary."""

        self.id = id  # type: str
        """QPU request ID."""

CoreMessages.QPURequest = _deprecated_property(QPURequest)
