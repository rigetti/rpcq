;;;; core-messages.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Copyright 2019 Rigetti Computing
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

(defmessage |Frame| ()
    (
     (|direction|
      :documentation "'rx' or 'tx'"
      :type :string
      :required t)

     (|sample_rate|
      :documentation "The sample rate [Hz] of the associated AWG/ADC"
      :type :float
      :required t)

     (|frequency|
      :documentation "The frame frequency [Hz]"
      :type :float
      :required t))

  :documentation "A frame encapsulates any rotating frame\
      relative to which control or readout waveforms may be defined.")

(defmessage |Resources| ()
    (
     (|qubits|
      :documentation "A list of qubits blocked/required by a job."
      :type (:list :string)
      :required t
      :default nil)

     (|frames|
      :documentation "RF/UHF frames by label."
      :type (:map :string -> |Frame|)
      :required t
      :default nil)

     (|frames_to_controls|
      :documentation "Mapping of frames to control channels by labels."
      :type (:map :string -> :string)
      :required t
      :default nil))

  :documentation "The resources required by a job")

(defmessage |AbstractWaveform| ()
    (
     (|frame|
      :documentation "The label of the associated tx-frame."
      :type :string
      :required t))

  :documentation "A waveform envelope defined for a specific frame. This abstract class is made concrete by either a `Waveform` or a templated waveform such as `GaussianWaveform`")

(defmessage |Waveform| (|AbstractWaveform|)
    (
     (|iqs|
      :documentation "The raw waveform envelope samples, alternating I and Q values."
      :type (:list :float)
      :required t
      :default nil))

  :documentation "A waveform envelope defined by specific IQ values for a specific frame.")

(defmessage |TemplateWaveform| (|AbstractWaveform|)
    (
     (|duration|
      :documentation "Length of the pulse in seconds"
      :type :float
      :required t)

     (|scale|
       :documentation "Scale to apply to waveform envelope"
       :type :float
       :required t
       :default 1.0)

     (|phase|
       :documentation "Phase [units of tau=2pi] to rotate the complex waveform envelope."
       :type :float
       :required t
       :default 0.0)

     (|detuning|
      :documentation "Modulation to apply to the waveform in Hz"
      :type :float
      :required t
      :default 0.0))


  :documentation "A waveform envelope defined for a specific frame. A templated waveform is defined by a parameterized pulseshape rather than explicit IQ values. The message specification does not enforce that the duration is implementable on the hardware.")

(defmessage |GaussianWaveform| (|TemplateWaveform|)
    (
     (|fwhm|
       :documentation "Full Width Half Max shape paramter in seconds"
       :type :float
       :required nil)

     (|t0|
       :documentation "Center time coordinate of the shape in seconds. Defaults to mid-point of pulse."
       :type :float
       :required nil))


 :documentation "A Gaussian shaped waveform envelope defined for a specific frame.")

(defmessage |DragGaussianWaveform| (|TemplateWaveform|)
   (
    (|fwhm|
      :documentation "Full Width Half Max shape paramter in seconds"
      :type :float
      :required nil)

    (|t0|
      :documentation "Center time coordinate of the shape in seconds. Defaults to mid-point of pulse."
      :type :float
      :required nil)

    (|anh|
      :documentation "Anharmonicity of the qubit, f01-f12 in (Hz)"
      :type :float
      :required t
      :default -210e6)

    (|alpha|
      :documentation "Dimensionless DRAG parameter"
      :type :float
      :required t
      :default 0.0))

  :documentation "A DRAG Gaussian shaped waveform envelope defined for a specific frame.")

(defmessage |HermiteGaussianWaveform| (|TemplateWaveform|)
  (
   (|fwhm|
    :documentation "Full Width Half Max shape paramter in seconds"
    :type :float
    :required nil)

   (|t0|
    :documentation "Center time coordinate of the shape in seconds. Defaults to mid-point of pulse."
    :type :float
    :required t
    :default nil)

   (|anh|
    :documentation "Anharmonicity of the qubit, f01-f12 in Hz"
    :type :float
    :required t
    :default -210e6)

   (|alpha|
    :documentation "Dimensionless DRAG parameter"
    :type :float
    :required t
    :default 0.0)

   (|second_order_hrm_coeff|
    :documentation "Second order coefficient (see paper)"
    :type :float
    :required t
    :default 0.956))

  :documentation "Hermite-Gaussian shaped pulse. Reference: Effects of arbitrary laser\
      or NMR pulse shapes on population inversion and coherence Warren S. Warren.\
      81, (1984); doi: 10.1063/1.447644")

(defmessage |ErfSquareWaveform| (|TemplateWaveform|)
    (
     (|risetime|
       :documentation "The width of the rise and fall sections in seconds."
       :type :float
       :required t
       :default 1e-9)

     (|pad_left|
      :documentation "Length of zero-amplitude padding before the pulse in seconds."
      :type :float
      :required t
      :default 0.0)

     (|pad_right|
      :documentation "Length of zero-amplitude padding after the pulse in seconds."
      :type :float
      :required t
      :default 0.0))

  :documentation "Pulse with a flat top and rounded shoulders given by error functions")

(defmessage |FlatWaveform| (|TemplateWaveform|)
    (
     (|iq|
      :documentation "Individual IQ point to hold constant"
      :type (:list :float)
      :required t
      :default nil))

  :documentation "Flat pulse.")

(defmessage |AbstractKernel| ()
    (
     (|frame|
      :documentation "The label of the associated rx-frame."
      :type :string
      :required t))

  :documentation "An integration kernel defined for a specific frame. This abstract class is made concrete by either a `FilterKernel` or `TemplateKernel`")


(defmessage |FilterKernel| (|AbstractKernel|)
    (
     (|iqs|
      :documentation "The raw kernel coefficients, alternating real and imaginary parts."
      :type (:list :float)
      :required t
      :default nil)

     (|bias|
      :documentation "The kernel is offset by this real value. Can be used to ensure the decision threshold lies at 0.0."
      :type :float
      :required t
      :default 0.0))

  :documentation "A filter kernel to produce scalar readout features from acquired readout waveforms.")

(defmessage |TemplateKernel| (|AbstractKernel|)
    (
     (|duration|
      :documentation "Length of the boxcar kernel in seconds"
      :type :float
      :required t)

     (|bias|
      :documentation "The kernel is offset by this real value. Can be used to ensure the decision threshold lies at 0.0."
      :type :float
      :required t
      :default 0.0)

     (|scale|
      :documentation "Scale to apply to boxcar kernel"
      :type :float
      :required t
      :default 1.0)

     (|phase|
      :documentation "Phase [units of tau=2pi] to rotate the kernel by."
      :type :float
      :required t
      :default 0.0)

     (|detuning|
      :documentation "Modulation to apply to the filter kernel in Hz"
      :type :float
      :required t
      :default 0.0))

  :documentation "An integration kernel defined for a specific frame.")

(defmessage |FlatKernel| (|TemplateKernel|) ()
  :documentation "An unnormalized flat or boxcar integration kernel.")

(defmessage |BoxcarAveragerKernel| (|TemplateKernel|) ()
  :documentation "A normalized flat or boxcar integration kernel.")


(defmessage |FilterNode| ()
    (
     (|module|
      :documentation "Absolute python module import path in which the filter\
          class is defined."
      :type :string
      :required t)

     (|filter_type|
      :documentation "The type (class name) of the filter."
      :type :string
      :required t)

     (|params|
      :documentation "Additional filter parameters."
      :type (:map :string -> :float)
      :required t
      :default nil)

     (|source|
      :documentation "Filter node label of the input to this node."
      :type :string
      :required t)

     (|publish|
      :documentation "If True, return the output of this node with the job\
          results (and publish a stream for it)."
      :type :bool
      :required t))

  :documentation "A node in the filter pipeline.")

(defmessage |DataAxis| ()
    (
     (|name|
      :documentation "Label for the axis, e.g., 'time' or 'shot_index'."
      :type :string
      :required t)

     (|points|
      :documentation "The sequence of values along the axis."
      :type (:list :float)
      :required t
      :default nil))

  :documentation "A data axis allows to label element(s) of a stream.")

(defmessage |Receiver| ()
    (
     (|instrument|
      :documentation "The instrument name"
      :type :string
      :required t)

     (|channel|
      :documentation "The instrument channel (label)"
      :type :string
      :required t)

     (|stream|
      :documentation "Name of the associated (raw) output stream that\
          should be published."
      :type :string
      :required t)

     (|publish|
      :documentation "Whether to publish the raw output stream."
      :type :bool
      :required t)

     (|data_axes|
      :documentation "Ordered list of DataAxis objects that together
          uniquely label each element in the stream."
      :type (:list |DataAxis|)
      :required t
      :default nil))

  :documentation "The receiver settings generated by the low-level\
      translator.")


(defmessage |ParameterExpression| ()
    (
     (|operator|
      :documentation "The operator '+', '-', '*'. The operands can be
          constant floating point numbers or strings referencing a dynamic
          program parameter or a ParameterAref to index into an array or
          itself a ParameterExpression."
      :type :string
      :required t)

     (|a|
      :documentation "The first operand"
      :type :any
      :required t)

     (|b|
      :documentation "The second operand"
      :type :any
      :required t))

  :documentation "A parametric expression.")

(defmessage |Instruction| ()
  (
   (|time|
    :documentation "The time at which the instruction is emitted [in seconds]."
    :type :float
    :required t))

  :documentation "An instruction superclass.")

(defmessage |DebugMessage| (|Instruction|)
    (
     (|frame|
      :documentation "The frame label that owns this debug message."
      :type :string
      :required t)

     (|message|
      :documentation "The 2-byte wide debug message to emit."
      :type :integer
      :required t))

  :documentation "Instructs the target to emit a specified debug message.")

(defmessage |Pulse| (|Instruction|)
    (
     (|frame|
      :documentation "The tx-frame label on which the pulse is played."
      :type :string
      :required t)

     (|waveform|
      :documentation "The waveform label"
      :type :string
      :required t)

     (|scale|
      :documentation "Dimensionless (re-)scaling factor which is applied to\
          the envelope."
      :type :float
      :required nil
      :default 1.0)

     (|phase|
      :documentation "Static phase angle [units of tau=2pi] by which the\
          envelope quadratures are rotated."
      :type :float
      :required t
      :default 0.0)

     (|detuning|
      :documentation "Detuning [Hz] with which the pulse envelope should be\
          modulated relative to the frame frequency."
      :type :float
      :required t
      :default 0.0))

  :documentation "Instruction to play a pulse with some (modified) waveform\
      envelope at a specific time on a specific frame.")

(defmessage |FlatPulse| (|Instruction|)
    (
     (|frame|
      :documentation "The tx-frame label on which the pulse is played."
      :type :string
      :required t)

     (|iq|
      :documentation "The I and Q value of the constant pulse."
      :type (:list :float)
      :required t)

     (|duration|
      :documentation "The duration of the pulse in [seconds], should be a\
          multiple of the associated tx-frame's inverse sample rate."
      :type :float
      :required t)

     (|phase|
      :documentation "Static phase angle [units of tau=2pi] by which the\
          envelope quadratures are rotated."
      :type :float
      :required t
      :default 0.0)

     (|detuning|
      :documentation "Detuning [Hz] with which the pulse envelope should be\
          modulated relative to the frame frequency."
      :type :float
      :required t
      :default 0.0)

     (|scale|
      :documentation "Dimensionless (re-)scaling factor which is applied to\
          the envelope."
      :type :float
      :required nil
      :default 1.0))

  :documentation "Instruction to play a pulse with a constant amplitude\
      (except for phase modulation) at a specific time on a specific frame.")

(defmessage |SetPhase| (|Instruction|)
    (
     (|frame|
      :documentation "The frame label for which to set the phase."
      :type :string
      :required t)

     (|phase|
      :documentation "Phase angle [units of tau=2pi] to update the frame phase\
          to."
      :type :float
      :required t
      :default 0.0))

  :documentation "Set the phase of a frame to an absolute value at a specific\
      time.")

(defmessage |ShiftPhase| (|Instruction|)
    (
     (|frame|
      :documentation "The frame label for which to set the phase."
      :type :string
      :required t)

     (|delta|
      :documentation "Phase angle [units of tau=2pi] by which to shift the\
          frame phase.  Can be a numerical value, a ParameterExpression or a\
          ParameterAref."
      :type :any
      :required t
      :default 0.0))

  :documentation "Shift the phase of a frame by a relative value at a\
      specific time.")

(defmessage |SwapPhases| (|Instruction|)
    (
     (|frame_a|
      :documentation "The first frame's label."
      :type :string
      :required t)

     (|frame_b|
      :documentation "The second frame's label."
      :type :string
      :required t))

  :documentation "Swap the phases of two tx-frames at a specific time.")

(defmessage |SetFrequency| (|Instruction|)
    (
     (|frame|
      :documentation "The frame label for which to set the frequency."
      :type :string
      :required t)

     (|frequency|
      :documentation "The frequency [Hz] to set the frame frequency to."
      :type :float
      :required t
      :default 0.0))

  :documentation "Set the frequency of a tx-frame to a specific value at a\
      specific time.")

(defmessage |ShiftFrequency| (|Instruction|)
    (
     (|frame|
      :documentation "The frame label for which to set the frequency."
      :type :string
      :required t)

     (|delta|
      :documentation "Frequency shift (new-old) [Hz] to apply to the frame\
          frequency."
      :type :float
      :required t
      :default 0.0))

  :documentation "Shift the frequency of a tx-frame by a specific amount at a\
      specific time.")

(defmessage |SetScale| (|Instruction|)
  (
   (|frame|
    :documentation "The frame label for which to set the scale."
    :type :string
    :required t)

   (|scale|
    :documentation "Scale (unitless) to apply to waveforms generated on the frame."
    :type :float
    :required t
    :default 1.0))

  :documentation "Set the scale of a tx-frame to a value at a specific time.")

(defmessage |Capture| (|Instruction|)
    (
     (|frame|
      :documentation "The rx-frame label on which to trigger the acquisition."
      :type :string
      :required t)

     (|duration|
      :documentation "The duration of the acquisition in [seconds]"
      :type :float
      :required t)

     (|filters|
      :documentation "An ordered list of labels of filter kernels to apply to\
          the captured waveform."
      :type (:list :string)
      :required t
      :default nil)

     (|send_to_host|
      :documentation "Transmit the readout bit back to Lodgepole.\
          (Unnecessary for fully calibrated active reset captures)."
      :type :bool
      :required t
      :default t)

     (|phase|
      :documentation "Static phase angle [units of tau=2pi] by which the\
          envelope quadratures are rotated."
      :type :float
      :required t
      :default 0.0)

     (|detuning|
      :documentation "Detuning [Hz] with which the pulse envelope should be\
          modulated relative to the frame frequency."
      :type :float
      :required t
      :default 0.0))

  :documentation "Specify an acquisition on an rx-frame as well as the\
      filters to apply.")

(defmessage |Program| ()
    (
     (|waveforms|
      :documentation "The waveforms appearing in the program by waveform\
          label."
      :type (:map :string -> |AbstractWaveform|)
      :required t
      :default nil)

     (|filters|
      :documentation "The readout filter kernels appearing in the program by\
          feature label."
      :type (:map :string -> |AbstractKernel|)
      :required t
      :default nil)

     (|scheduled_instructions|
      :documentation "The ordered sequence of scheduled instruction objects."
      :type (:list |Instruction|)
      :required t
      :default nil)

     (|parameters|
      :documentation "A mapping of dynamic parameter names to their type\
          specification."
      :type (:map :string -> |ParameterSpec|)
      :required t
      :default nil))

  :documentation "The dynamic aspects (waveforms, readout kernels, scheduled\
  instructions and parameters) of a job.")

(defmessage |ScheduleIRJob| ()
    (
     (|job_id|
      :documentation "A unique ID to help the submitter track the job."
      :type :string
      :required nil
      :deprecated t)

     (|num_shots|
      :documentation "How many repetitions the job should be executed for."
      :type :integer
      :required t)

     (|resources|
      :documentation "The resources required by the job."
      :type |Resources|
      :required t)

     (|operating_point|
      :documentation "Operating points or static instrument channel settings\
          (mapping control_name (instrument name) -> instrument channel settings\
          (instrument settings) dictionary)."
      :type (:map :string -> :map)
      :required t
      :default nil)

     (|program|
      :documentation "The actual program to be executed."
      :type |Program|
      :required t)

     (|filter_pipeline|
      :documentation "The filter pipeline. Mapping of node labels to\
          FilterNode's."
      :type (:map :string -> |FilterNode|)
      :required t
      :default nil))

  :documentation "The unit of work to be executed.")

(defmessage |RackMeta| ()
    (
     (|rack_id|
      :documentation "A unique identifier for the rack."
      :type :string
      :required nil)

     (|rack_version|
      :documentation "A version of the rack configuration."
      :type :integer
      :required nil)

     (|schema_version|
      :documentation "A version of the rack configuration."
      :type :integer
      :required t
      :default 0))

  :documentation "Meta information about a rack configuration.")

(defmessage |QPU| ()
    (
     (|chip_label|
      :documentation "The fabrication label for the QPU chip."
      :type :string
      :required t)

     (|qubits|
      :documentation "A list of qubits labels."
      :type (:list :string)
      :required t
      :default nil)

     (|controls|
      :documentation "A mapping of control labels to tuples (instrument\
          label, channel label)."
      :type (:map :string -> :list)
      :required t
      :default nil)

     (|controls_by_qubit|
      :documentation "A map of qubit label to list of controls that should be\
          considered blocked when the qubit is part of a job execution."
      :type (:map :string -> :list)
      :required t
      :default nil))

  :documentation "Configuration info for the QPU")

(defmessage |Instrument| ()
    (
     (|address|
      :documentation "The full address of a QPU."
      :type :string
      :required t)

     (|module|
      :documentation "Full python import path for the module that includes\
          the instrument driver."
      :type :string
      :required t)

     (|instrument_type|
      :documentation "Instrument type (driver class name)."
      :type :string
      :required t)

     (|channels|
      :documentation "Mapping of channel labels to channel settings"
      :type (:map :string -> :any)
      :required nil)

     (|virtual|
      :documentation "Whether the instrument is virtual."
      :type :bool
      :required t
      :default nil)

     (|setup|
      :documentation "Any additional information used by the instrument for\
          one-time-setup"
      :type (:map :string -> :any)
      :required nil))

  :documentation "Instrument settings.")

(defmessage |DeployedRack| ()
    (
     (|rack_meta|
      :documentation "Meta information about the deployed rack."
      :type |RackMeta|
      :required t)

     (|qpu|
      :documentation "Information about the QPU."
      :type |QPU|
      :required t)

     (|instruments|
      :documentation "Mapping of instrument name to instrument settings."
      :type (:map :string -> |Instrument|)
      :required t
      :default nil))

  :documentation "The rack configuration for lodgepole.")

(defmessage |AWGChannel| ()
    (
     (|sample_rate|
      :documentation "The sampling rate [Hz] of the associated DAC/ADC\
          component."
      :type :float
      :required t)

     (|lo_frequency|
      :documentation "The local oscillator frequency [Hz] of the channel."
      :type :float
      :required nil)

     (|gain|
      :documentation "If there is an amplifier, the amplifier gain [dB]."
      :type :float
      :required nil)

     (|direction|
      :documentation "'rx' or 'tx'"
      :type :string
      :required t)

     (|delay|
      :documentation "Delay [seconds] to account for inter-channel skew."
      :type :float
      :required t
      :default 0.0))

  :documentation "Configuration of a single RF channel.")

(defmessage |QFDChannel| ()
    (
     (|direction|
      :documentation "The QFD is a device that transmits pulses."
      :type :string
      :required nil
      :default "tx")

     (|nco_frequency|
      :documentation "The DAC NCO frequency [Hz]."
      :type :float
      :required nil
      :default 0.0)

     (|gain|
      :documentation "The output gain on the DAC in [dB]. Note that this\
          should be in the range -45dB to 0dB and is rounded to the\
          nearest 3dB step."
      :type :float
      :required nil
      :default 0.0)

     (|channel_index|
      :documentation "The channel index on the QFD, zero indexed from the\
          lowest channel, as installed in the box."
      :type :integer
      :required t)

     (|delay|
      :documentation "Delay [seconds] to account for inter-channel skew."
      :type :float
      :required t
      :default 0.0)

     (|flux_current|
      :documentation "Slow flux current [Amps]."
      :type :float
      :required nil)

     (|flux_relay|
      :documentation "Set the state of the Flux relay.\
          True  - Relay closed, allows flux current to flow.\
          False - Relay open, no flux current can flow."
      :type :bool
      :required nil))

  :documentation "Configuration for a single QFD Channel.")

(defmessage |QGSChannel| ()
    (
     (|direction|
      :documentation "The QGS is a device that transmits pulses."
      :type :string
      :required nil
      :default "tx")

     (|nco_frequency|
      :documentation "The DAC NCO frequency [Hz]."
      :type :float
      :required nil
      :default 2000000000.0)

     (|gain|
      :documentation "The output gain on the DAC in [dB]. Note that this\
          should be in the range -45dB to 0dB and is rounded to the\
          nearest 3dB step."
      :type :float
      :required nil
      :default 0.0)

     (|channel_index|
      :documentation "The channel index on the QGS, zero indexed from the lowest channel,
        as installed in the box."
      :type :integer
      :required nil
      :default 0)

     (|delay|
      :documentation "Delay [seconds] to account for inter-channel skew."
      :type :float
      :required t
      :default 0.0))

  :documentation "Configuration for a single QGS Channel.")

(defmessage |QRTChannel| ()
    (
     (|direction|
      :documentation "The QRT is a device that transmits readout pulses."
      :type :string
      :required nil
      :default "tx")

     (|nco_frequency|
      :documentation "The DAC NCO frequency [Hz]."
      :type :float
      :required nil
      :default 1.25e9)

     (|gain|
      :documentation "The output gain on the DAC in [dB]. Note that this should be in the range\
       -45dB to 0dB and is rounded to the nearest 3dB step."
      :type :float
      :required nil
      :default 0.0)

     (|channel_index|
      :documentation "The channel index on the QRT, zero indexed from the lowest channel,
        as installed in the box."
      :type :integer
      :required nil
      :default 0)

     (|delay|
      :documentation "Delay [seconds] to account for inter-channel skew."
      :type :float
      :required t
      :default 0.0))

  :documentation "Configuration for a single QRT Channel.")

(defmessage |QRRChannel| ()
    (
     (|direction|
      :documentation "The QRR is a device that receives readout pulses."
      :type :string
      :required nil
      :default "rx")

     (|nco_frequency|
      :documentation "The ADC NCO frequency [Hz]."
      :type :float
      :required nil
      :default 0.0)

     (|gain|
      :documentation "The input gain on the ADC in [dB]. Note that this should be in the range\
       -45dB to 0dB and is rounded to the nearest 3dB step."
      :type :float
      :required nil
      :default 0.0)

     (|channel_index|
      :documentation "The channel index on the QRR, zero indexed from the lowest channel,
        as installed in the box."
      :type :integer
      :required t)

     (|delay|
      :documentation "Delay [seconds] to account for inter-channel skew."
      :type :float
      :required t
      :default 0.0))

  :documentation "Configuration for a single QRR Channel.")

(defmessage |CWChannel| ()
    (
     (|channel_index|
      :documentation "The zero-indexed channel of the generator's output."
      :type :integer
      :required t
      :default 0)

     (|rf_output_frequency|
      :documentation "The CW generator's output frequency [Hz]."
      :type :integer
      :required nil
      :default 1000000000)

     (|rf_output_power|
      :documentation "The power of CW generator's output [dBm]."
      :type :float
      :required nil
      :default 0.0)

     (|rf_output_enabled|
      :documentation "The state (on/off) of CW generator's output."
      :type :bool
      :required nil
      :default t))

  :documentation "Configuration for a single CW Generator Channel.")

(defmessage |LegacyUSRPSequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated tx channel."
      :type :string
      :required nil)

     (|rx_channel|
      :documentation "The label of the associated rx channel."
      :type :string
      :required nil))

  :documentation "Configuration for a Legacy USRP Sequencer")

(defmessage |QFDSequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated channel."
      :type :string
      :required t))

  :documentation "Configuration for a single QFD Sequencer.")

(defmessage |QFDx2Sequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated channel."
      :type :string
      :required t))

  :documentation "Configuration for a single QFDx2 Sequencer.")

(defmessage |QGSSequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated channel."
      :type :string
      :required t))

  :documentation "Configuration for a single QGS Sequencer.")

(defmessage |QGSx2Sequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated channel."
      :type :string
      :required t))

  :documentation "Configuration for a single QGSx2 Sequencer.")

(defmessage |QRTSequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated tx channel."
      :type :string
      :required t)

     (|sequencer_index|
       :documentation "The sequencer index (0-7) of this sequencer."
       :type :integer
       :required t)

     (|low_freq_range|
      :documentation "Used to signal if this sequencer is in the low frequency configuration."
      :type :bool
      :required nil
      :default nil))

  :documentation "Configuration for a single readout transmit (QRT) sequencer.")

(defmessage |QRTx2Sequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated tx channel."
      :type :string
      :required t)

     (|sequencer_index|
       :documentation "The sequencer index (0-15) of this sequencer."
       :type :integer
       :required t)

     (|low_freq_range|
      :documentation "Used to signal if this sequencer is in the low frequency configuration."
      :type :bool
      :required nil
      :default nil))

  :documentation "Configuration for a dual readout transmit (QRTx2) sequencer.")

(defmessage |QRRSequencer| ()
    (
     (|rx_channel|
      :documentation "The label of the associated rx channel."
      :type :string
      :required t)

     (|sequencer_index|
       :documentation "The sequencer index (0-15) to assign. Note that only\
         sequencer 0 can return raw readout measurements."
       :type :integer
       :required t))

  :documentation "Configuration for a single readout receive (QRR) sequencer.")

(defmessage |USICardSequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated channel."
      :type :string
      :required t))

  :documentation "Configuration for the card which\
      interfaces with the USI Target on the USRP.")

(defmessage |USITargetSequencer| ()
    (
     (|tx_channel|
      :documentation "The label of the associated intial tx channel."
      :type :string
      :required t)

     (|rx_channel|
      :documentation "The label of the associated initial rx channel."
      :type :string
      :required t)

     (|sequencer_index|
      :documentation "The sequencer index (0-7) to assign. Note that only\
           sequencer 0 has the ability to use the NCO or capture raw readout\
           streams."
      :type :integer
      :required t))
  :documentation "Configuration for a single USITarget Sequencer.")

(defmessage |CWFrequencySweep| ()
    (
     (|start|
      :documentation "Start frequency of the sweep, in Hz"
      :type :float
      :required t)
     (|stop|
      :documentation "Stop frequency of the sweep, in Hz"
      :type :float
      :required t)
     (|num_pts|
      :documentation "Number of frequency points to sample, cast to int."
      :type :integer
      :required t)
     (|source|
      :documentation "Source port number"
      :type :integer
      :required t)
     (|measure|
      :documentation "Measure port number"
      :type :integer
      :required t))

  :documentation "Configuration of a continuous wave frequency sweep.")


(defmessage |VNASettings| ()
    (
     (|e_delay|
      :documentation "Electrical delay in seconds from source to measure port"
      :type :float
      :required t)
     (|phase_offset|
      :documentation "Phase offset in degrees from measured to reported phase"
      :type :float
      :required t)
     (|bandwidth|
      :documentation "Bandwidth of the sweep, in Hz"
      :type :float
      :required t)
     (|power|
      :documentation "Source power in dBm"
      :type :float
      :required t)
     (|averaging|
      :documentation "Sets the number of points to combine into an averaged\
          trace"
      :type :integer
      :required t
      :default 1)
     (|freq_sweep|
      :documentation "Frequency sweep settings"
      :type |CWFrequencySweep|
      :required t))

  :documentation "Configuration of VNA settings for a continuous wave sweep.")


(defmessage |TimeBomb| ()
    (
     (|deadline|
      :documentation "Deadline, specified in the format\
          '%Y-%m-%dT%H:%M:%S.000Z', after which this job becomes unexecutable."
      :type :string
      :required t)

     (|chip_label|
      :documentation "Label string for the chip on which this job is meant to\
          execute."
      :type :string
      :required t))


  :documentation "Payload used to match a job with a particular execution\
      target.")

(defmessage |MicrowaveSourceSettings| ()
    (
     (|frequency|
      :documentation "Frequency setting for microwave source (Hz)."
      :type :float
      :required t)

     (|power|
      :documentation "Power setting for microwave source (dBm)."
      :type :float
      :required t)

     (|output|
      :documentation "Output setting for microwave source. If true, the source will be turned on."
      :type :bool
      :required t))


  :documentation "Configuration of Microwave Source settings for operating amplifiers.")


(defmessage |ExecutorJob| ()
    (
     (|instrument_settings|
      :documentation "Dict mapping instrument names to arbitrary instrument\
          settings."
      :type (:map :string -> :any)
      :required t)

     (|filter_pipeline|
      :documentation "The filter pipeline to process measured data."
      :type (:map :string -> |FilterNode|)
      :required t)

     (|receivers|
      :documentation "Dict mapping stream names to receiver settings."
      :type (:map :string -> |Receiver|)
      :required t)

     (|duration|
      :documentation "The total duration of the program execution in seconds."
      :type :float
      :required nil)

     (|timebomb|
      :documentation "An optional payload used to match this job with a\
          particular execution target."
      :type |TimeBomb|
      :required nil))

  :documentation "Job which is sent directly to the executor")

(defmessage |PatchableBinary| ()
    (
     (|base_binary|
      :documentation "Raw Tsunami binary object."
      :type :any
      :required t)

     (|patch_table|
      :documentation "Dictionary mapping patch names to their memory\
          descriptors."
      :type (:map :string -> |PatchTarget|)
      :required t))


  :documentation "Tsunami binary with patching metadata for classical\
      parameter modification.")

(defmessage |ActiveReset| ()
    (
     (|time|
      :documentation "Time at which the ActiveReset begins in [seconds]."
      :type :float
      :required t)

     (|attempts|
      :documentation "The number of times to repeat the active reset sequence."
      :type :integer
      :required t
      :default 3)

     (|measurement_duration|
      :documentation "The duration of measurement block in [seconds]. The\
          measurement bit is expected to have arrived on the QGS after\
          this time relative to the overall start of the ActiveReset block."
      :type :float
      :required t)

     (|feedback_duration|
      :documentation "The duration of feedback block in [seconds]"
      :type :float
      :required t)

     (|measurement_instructions|
      :documentation "The ordered sequence of scheduled measurement\
          instructions."
      :type (:list :map)
      :required t
      :default nil)


     (|measurement_bit|
      :documentation "The address of the readout bit to condition the\
          feedback on.  The bit is first accessed after measurement_duration\
          has elapsed."
      :type :integer
      :required t)


     (|apply_feedback_when|
      :documentation "Apply the feedback when the measurement_bit equals the\
          value of this flag."
      :type :bool
      :required t
      :default t)


     (|feedback_instructions|
      :documentation "The ordered sequence of scheduled feedback instructions."
      :type (:list :map)
      :required t
      :default nil))


  :documentation "An active reset control sequence consisting of a repeated\
      sequence of a measurement block and a feedback block conditional on the\
      outcome of a specific measurement bit.  Regardless of the measurement\
      outcomes the total duration of the control sequence is [attempts x\
      (measurement_duration + feedback_duration)].  The total\
      measurement_duration must be chosen to allow for enough time after any\
      Capture commands for the measurement bit to propagate back to the gate\
      cards that are actuating the feedback.")
