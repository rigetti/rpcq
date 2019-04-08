"""
Specification of a dynamic parameter type and array-length.
"""
Base.@kwdef struct ParameterSpec
    "The parameter type, e.g., one of 'INTEGER', or 'FLOAT'."
    type::String = nothing

    "If this is not 1, the parameter is an array of this length."
    length::Int32 = 1
end

"""
A parametric expression.
"""
Base.@kwdef struct ParameterAref
    "The parameter name"
    name::String

    "The array index."
    index::Int32
end

"""
Patchable memory location descriptor.
"""
Base.@kwdef struct PatchTarget
    "Data type at this address."
    patch_type::ParameterSpec

    "Memory address of the patch."
    patch_offset::Int32
end

"""
A single request object according to the JSONRPC standard.
"""
Base.@kwdef struct RPCRequest
    "The JSONRPC version."
    jsonrpc::String = "2.0"

    "The RPC function name."
    method::String

    "The RPC function arguments."
    params::Any

    "RPC request id (used to verify that request and response belong together)."
    id::String
end

"""
The reply for a JSONRPC request.
"""
Base.@kwdef struct RPCReply
    "The JSONRPC version."
    jsonrpc::String = "2.0"

    "The RPC result."
    result::Union{Any, Nothing} = nothing

    "The RPC request id."
    id::String
end

"""
A error message for JSONRPC requests.
"""
Base.@kwdef struct RPCError
    "The JSONRPC version."
    jsonrpc::String = "2.0"

    "The error message."
    error::String

    "The RPC request id."
    id::String
end

"""
ISA and specs for a particular device.
"""
Base.@kwdef struct TargetDevice
    "Instruction-set architecture for this device."
    isa::Dict{String, Dict}

    "Fidelities and coherence times for this device."
    specs::Dict{String, Dict}
end

"""
RPC request payload for generating a randomized benchmarking sequence.
"""
Base.@kwdef struct RandomizedBenchmarkingRequest
    "Depth of the benchmarking sequence."
    depth::Int32

    "Number of qubits involved in the benchmarking sequence."
    qubits::Int32

    "List of Quil programs, each describing a Clifford."
    gateset::Array{String}

    "PRNG seed. Set this to guarantee repeatable results."
    seed::Union{Int32, Nothing} = nothing

    "Fixed Clifford, specified as a Quil string, to interleave through an RB sequence."
    interleaver::Union{String, Nothing} = nothing
end

"""
RPC reply payload for a randomly generated benchmarking sequence.
"""
Base.@kwdef struct RandomizedBenchmarkingResponse
    "List of Cliffords, each expressed as a list of generator indices."
    sequence::Array{Array{Int32}}
end

"""
Specification of a single Pauli term as a tensor product of Pauli factors.
"""
Base.@kwdef struct PauliTerm
    "Qubit indices onto which the factors of a Pauli term are applied."
    indices::Array{Int32}

    "Ordered factors of a Pauli term."
    symbols::Array{String}
end

"""
RPC request payload for conjugating a Pauli element by a Clifford element.
"""
Base.@kwdef struct ConjugateByCliffordRequest
    "Specification of a Pauli element."
    pauli::PauliTerm

    "Specification of a Clifford element."
    clifford::String
end

"""
RPC reply payload for a Pauli element as conjugated by a Clifford element.
"""
Base.@kwdef struct ConjugateByCliffordResponse
    "Encoded global phase factor on the emitted Pauli."
    phase::Int32

    "Description of the encoded Pauli."
    pauli::String
end

"""
Quil and the device metadata necessary for quilc.
"""
Base.@kwdef struct NativeQuilRequest
    "Arbitrary Quil to be sent to quilc."
    quil::String

    "Specifications for the device to target with quilc."
    target_device::TargetDevice
end

"""
Metadata for a native quil program.
"""
Base.@kwdef struct NativeQuilMetadata
    "Output qubit index relabeling due to SWAP insertion."
    final_rewiring::Array{Int32} = []

    "Maximum number of successive gates in the native quil program."
    gate_depth::Union{Int32, Nothing} = nothing

    "Total number of gates in the native quil program."
    gate_volume::Union{Int32, Nothing} = nothing

    "Maximum number of successive two-qubit gates in the native quil program."
    multiqubit_gate_depth::Union{Int32, Nothing} = nothing

    "Rough estimate of native quil program length in nanoseconds."
    program_duration::Union{Float32, Nothing} = nothing

    "Rough estimate of the fidelity of the full native quil program, uses specs."
    program_fidelity::Union{Float32, Nothing} = nothing

    "Total number of SWAPs in the native quil program."
    topological_swaps::Union{Int32, Nothing} = nothing
end

"""
Native Quil and associated metadata returned from quilc.
"""
Base.@kwdef struct NativeQuilResponse
    "Native Quil returned from quilc."
    quil::String

    "Metadata for the returned Native Quil."
    metadata::Union{NativeQuilMetadata, Nothing} = nothing
end

"""
A request type to handle compiling arithmetic out of gate parameters.
"""
Base.@kwdef struct RewriteArithmeticRequest
    "Native Quil for which to rewrite arithmetic parameters."
    quil::String
end

"""
The data needed to run programs with gate arithmetic on the hardware.
"""
Base.@kwdef struct RewriteArithmeticResponse
    "Native Quil rewritten with no arithmetic in gate parameters."
    quil::String

    "The declared memory descriptors in the Quil of the related request."
    original_memory_descriptors::Dict{String, ParameterSpec} = Dict()

    "A mapping from memory references to the original gate arithmetic."
    recalculation_table::Dict{ParameterAref, String} = Dict()
end

"""
Native Quil and the information needed to create binary executables.
"""
Base.@kwdef struct BinaryExecutableRequest
    "Native Quil to be translated into an executable program."
    quil::String

    "The number of times to repeat the program."
    num_shots::Int32
end

"""
Program to run on the QPU.
"""
Base.@kwdef struct BinaryExecutableResponse
    "Execution settings and sequencer binaries."
    program::String

    "Internal field for constructing patch tables."
    memory_descriptors::Dict{String, ParameterSpec} = Dict()

    "Internal field for reshaping returned buffers."
    ro_sources::Array{Any} = []
end

"""
rpcQ-serializable form of a pyQuil Program object.
"""
Base.@kwdef struct PyQuilExecutableResponse
    "String representation of a Quil program."
    program::String

    "Miscellaneous attributes to be unpacked onto the pyQuil Program object."
    attributes::Dict{String, Any}
end

"""
Program and patch values to send to the QPU for execution.
"""
Base.@kwdef struct QPURequest
    "Execution settings and sequencer binaries."
    program::Any

    "Dictionary mapping data names to data values for patching the binary."
    patch_values::Dict{String, Array{Any}}

    "QPU request ID."
    id::String
end