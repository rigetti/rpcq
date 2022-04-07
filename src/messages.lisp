;;;; messages.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2018 Rigetti Computing
;;;;
;;;;    Licensed under the Apache License, Version 2.0 (the "License");
;;;;    you may not use this file except in compliance with the License.
;;;;    You may obtain a copy of the License at
;;;;
;;;;        http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;;    Unless required by applicable law or agreed to in writing, software
;;;;    distributed under the License is distributed on an "AS IS" BASIS,
;;;;    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;;    See the License for the specific language governing permissions and
;;;;    limitations under the License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:rpcq)

(defmessage |ParameterSpec| ()
    (
     (|type|
      :documentation "The parameter type, e.g., one of 'INTEGER', or 'FLOAT'."
      :type :string
      :required t
      :default "")

     (|length|
      :documentation "If this is not 1, the parameter is an array of this length."
      :type :integer
      :required t
      :default 1))

  :documentation "Specification of a dynamic parameter type and array-length.")

(defmessage |ParameterAref| ()
    (
     (|name|
      :documentation "The parameter name"
      :type :string
      :required t)

     (|index|
      :documentation "The array index."
      :type :integer
      :required t))

  :documentation "A parametric expression.")

(defmessage |PatchTarget| ()
    (
     (|patch_type|
      :documentation "Data type at this address."
      :type |ParameterSpec|
      :required t)

     (|patch_offset|
      :documentation "Memory address of the patch."
      :type :integer
      :required t))


  :documentation "Patchable memory location descriptor.")

(defmessage |RPCRequest| ()
    (
     (|jsonrpc|
      :documentation "The JSONRPC version."
      :type :string
      :required t
      :default "2.0")

     (|method|
      :documentation "The RPC function name."
      :type :string
      :required t)

     (|params|
      :documentation "The RPC function arguments."
      :type :any
      :required t)

     (|id|
      :documentation "RPC request id (used to verify that request and response belong together)."
      :type :string
      :required t)

     (|client_timeout|
      :documentation "The client-side timeout for the request. The server itself may be configured with a timeout that is greater than the client-side timeout, in which case the server can choose to terminate any processing of the request."
      :type :float
      :required nil
      :default nil)

     (|client_key|
      :documentation "The ZeroMQ CURVE public key used to make the request, as received by the server. Empty if no key is used."
      :type :string
      :required nil))

  :documentation "A single request object according to the JSONRPC standard.")

(defmessage |RPCWarning| ()
    (
     (|body|
      :documentation "The warning string."
      :type :string
      :required t)

     (|kind|
      :documentation "The type of the warning raised."
      :type :string
      :required nil)
    )
  :documentation "An individual warning emitted in the course of RPC processing.")

(defmessage |RPCReply| ()
    (
     (|jsonrpc|
      :documentation "The JSONRPC version."
      :type :string
      :required t
      :default "2.0")

     (|result|
      :documentation "The RPC result."
      :type :any
      :default nil)

     (|id|
      :documentation "The RPC request id."
      :type :string

      :required t)

     (|warnings|
      :documentation "A list of warnings that occurred during request processing."
      :type (:list |RPCWarning|)
      :required t
      :default nil))
  :documentation "The reply for a JSONRPC request.")

(defmessage |RPCError| ()
    (
     (|jsonrpc|
      :documentation "The JSONRPC version."
      :type :string
      :required t
      :default "2.0")

     (|error|
      :documentation "The error message."
      :type :string
      :required t)

     (|id|
      :documentation "The RPC request id."
      :type :string
      :required t)

     (|warnings|
      :documentation "A list of warnings that occurred during request processing."
      :type (:list |RPCWarning|)
      :required t
      :default nil))
  :documentation "A error message for JSONRPC requests.")

(defmessage |TargetDevice| ()
    ((|isa|
      :documentation "Instruction-set architecture for this device."
      :type (:map :string -> :map)
      :required t)

     (|specs|
      :documentation "Fidelities and coherence times for this device."
      :type (:map :string -> :map)
      :required t))
  :documentation "ISA and specs for a particular device.")

(defmessage |RandomizedBenchmarkingRequest| ()
    ((|depth|
      :documentation "Depth of the benchmarking sequence."
      :type :integer
      :required t)

     (|qubits|
      :documentation "Number of qubits involved in the benchmarking sequence."
      :type :integer
      :required t)

     (|gateset|
      :documentation "List of Quil programs, each describing a Clifford."
      :type (:list :string)
      :required t)

     (|seed|
      :documentation "PRNG seed. Set this to guarantee repeatable results."
      :type :integer
      :required nil)

     (|interleaver|
      :documentation "Fixed Clifford, specified as a Quil string, to interleave through an RB sequence."
      :type :string
      :required nil))

  :documentation "RPC request payload for generating a randomized benchmarking sequence.")

(defmessage |RandomizedBenchmarkingResponse| ()
    ((|sequence|
      :documentation "List of Cliffords, each expressed as a list of generator indices."
      :type (:list (:list :integer))
      :required t))

  :documentation "RPC reply payload for a randomly generated benchmarking sequence.")

(defmessage |PauliTerm| ()
    ((|indices|
      :documentation "Qubit indices onto which the factors of a Pauli term are applied."
      :type (:list :integer)
      :required t)

     (|symbols|
      :documentation "Ordered factors of a Pauli term."
      :type (:list :string)
      :required t))

  :documentation "Specification of a single Pauli term as a tensor product of Pauli factors.")

(defmessage |ConjugateByCliffordRequest| ()
    ((|pauli|
      :documentation "Specification of a Pauli element."
      :type |PauliTerm|
      :required t)

     (|clifford|
      :documentation "Specification of a Clifford element."
      :type :string
      :required t))

  :documentation "RPC request payload for conjugating a Pauli element by a Clifford element.")

(defmessage |ConjugateByCliffordResponse| ()
    ((|phase|
      :documentation "Encoded global phase factor on the emitted Pauli."
      :type :integer
      :required t)

     (|pauli|
      :documentation "Description of the encoded Pauli."
      :type :string
      :required t))

  :documentation "RPC reply payload for a Pauli element as conjugated by a Clifford element.")

(defmessage |NativeQuilRequest| ()
    ((|quil|
      :documentation "Arbitrary Quil to be sent to quilc."
      :type :string
      :required t)

     (|target_device|
      :documentation "Specifications for the device to target with quilc."
      :type |TargetDevice|
      :required t))
  :documentation "Quil and the device metadata necessary for quilc.")

(defmessage |NativeQuilMetadata| ()
    ((|final_rewiring|
      :documentation "Output qubit index relabeling due to SWAP insertion."
      :type (:list :integer)
      :required nil)

     (|gate_depth|
      :documentation "Maximum number of successive gates in the native quil program."
      :type :integer
      :required nil)

     (|gate_volume|
      :documentation "Total number of gates in the native quil program."
      :type :integer
      :required nil)

     (|multiqubit_gate_depth|
      :documentation "Maximum number of successive two-qubit gates in the native quil program."
      :type :integer
      :required nil)

     (|program_duration|
      :documentation "Rough estimate of native quil program length in nanoseconds."
      :type :float
      :required nil)

     (|program_fidelity|
      :documentation "Rough estimate of the fidelity of the full native quil program, uses specs."
      :type :float
      :required nil)

     (|topological_swaps|
      :documentation "Total number of SWAPs in the native quil program."
      :type :integer
      :required nil)

     (|qpu_runtime_estimation|
      :documentation "The estimated runtime (milliseconds) on a Rigetti QPU for a protoquil program."
      :type :float
      :required nil))
  :documentation "Metadata for a native quil program.")

(defmessage |NativeQuilResponse| ()
    ((|quil|
      :documentation "Native Quil returned from quilc."
      :type :string
      :required t)

     (|metadata|
      :documentation "Metadata for the returned Native Quil."
      :type |NativeQuilMetadata|
      :required nil))
  :documentation "Native Quil and associated metadata returned from quilc.")

(defmessage |RewriteArithmeticRequest| ()
    ((|quil|
      :documentation "Native Quil for which to rewrite arithmetic parameters."
      :type :string
      :required t))
  :documentation "A request type to handle compiling arithmetic out of gate parameters.")

(defmessage |RewriteArithmeticResponse| ()
    ((|quil|
      :documentation "Native Quil rewritten with no arithmetic in gate parameters."
      :type :string
      :required t)

     (|original_memory_descriptors|
      :documentation "The declared memory descriptors in the Quil of the related request."
      :type (:map :string -> |ParameterSpec|)
      :required nil)

     (|recalculation_table|
      :documentation "A mapping from memory references to the original gate arithmetic."
      :type (:map |ParameterAref| -> :string)
      :required nil))
  :documentation "The data needed to run programs with gate arithmetic on the hardware.")

(defmessage |BinaryExecutableRequest| ()
    ((|quil|
      :documentation "Native Quil to be translated into an executable program."
      :type :string
      :required t)

     (|num_shots|
      :documentation "The number of times to repeat the program."
      :type :integer
      :required t))
  :documentation "Native Quil and the information needed to create binary executables.")

(defmessage |BinaryExecutableResponse| ()
    ((|program|
      :documentation "Execution settings and sequencer binaries."
      :type :string
      :required t)

     (|memory_descriptors|
      :documentation "Internal field for constructing patch tables."
      :type (:map :string -> |ParameterSpec|)
      :required nil
      :default nil)

     (|ro_sources|
      :documentation "Internal field for reshaping returned buffers."
      :type (:list :any)
      :required nil
      :default nil))
  :documentation "Program to run on the QPU.")

(defmessage |QuiltBinaryExecutableRequest| ()
  ((|quilt|
    :documentation "Native Quilt to be translated into an executable program."
    :type :string
    :required t)

   (|num_shots|
    :documentation "The number of times to repeat the program."
    :type :integer
    :required t))
  :documentation "Native Quilt and the information needed to create binary executables.")

(defmessage |QuiltBinaryExecutableResponse| ()
  ((|program|
    :documentation "Execution settings and sequencer binaries."
    :type :string
    :required t)

   (|memory_descriptors|
    :documentation "Internal field for constructing patch tables."
    :type (:map :string -> |ParameterSpec|)
    :required nil
    :default nil)

   (|ro_sources|
    :documentation "Internal field for reshaping returned buffers."
    :type (:list :any)
    :required nil
    :default nil)

   (|debug|
    :documentation "Debug information associated with the translation process."
    :type (:map :string -> :any)
    :required t))
  :documentation "Program to run on the QPU.")

(defmessage |PyQuilExecutableResponse| ()
    ((|program|
      :documentation "String representation of a Quil program."
      :type :string
      :required t)

     (|attributes|
      :documentation "Miscellaneous attributes to be unpacked onto the pyQuil Program object."
      :type (:map :string -> :any)
      :required t))
  :documentation "rpcQ-serializable form of a pyQuil Program object.")

(defmessage |QPURequest| ()
    ((|program|
      :documentation "Execution settings and sequencer binaries."
      :type :any
      :required t)

     (|patch_values|
      :documentation "Dictionary mapping data names to data values for patching the binary."
      :type (:map :string -> (:list :any))
      :required t)

     (|id|
      :documentation "QPU request ID."
      :type :string
      :required t))
  :documentation "Program and patch values to send to the QPU for execution.")

(defmessage |QuiltCalibrationsRequest| ()
  ((|target_device|
    :documentation "Specifications for the device to get calibrations for."
    :type |TargetDevice|
    :required t))
  :documentation "A request for up-to-date Quilt calibrations.")

(defmessage |QuiltCalibrationsResponse| ()
  ((|quilt|
    :documentation "Quilt code with definitions for frames, waveforms, and calibrations."
    :type :string
    :required t))
  :documentation "Up-to-date Quilt calibrations.")

(defmessage |GetExecutionResultsResponse| ()
  ((|buffers|
    :documentation "Result buffers for a completed ExecutorJob."
    :type (:map :string -> (:map :string -> :any))
    :required t)

   (|execution_duration_microseconds|
    :documentation "Duration (in microseconds) ExecutorJob held exclusive access to quantum hardware."
    :type :integer
    :required t))
  :documentation "Results of a completed ExecutorJob execution.")
