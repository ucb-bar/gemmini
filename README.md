Gemmini
====================================

The Gemmini project is developing a systolic-array based matrix multiplication accelerator generator for the investigation of SoC integration of such accelerators. It is inspired by recent trends in machine learning accelerators for edge and mobile SoCs.

Gemmini is part of the [Chipyard](https://github.com/ucb-bar/chipyard) ecosystem. **For instructions on how to produce Gemmini RTL or to run Gemmini simulations, consult [this page](https://chipyard.readthedocs.io/en/latest/Generators/Gemmini.html) in the Chipyard documentation**. This document is intended to provide more in-depth information for those who might want to start hacking on Gemmini's source code.


**Developer Note**
To track compatible versions of Chipyard and Spike, please update the CHIPYARD.hash and SPIKE.hash files with updated hashes of Chipyard and Spike commits when bumping Chipyard or Spike.

![Image description](./gemmini-system.png)

Architecture
================

Gemmini is implemented as a RoCC accelerator with non-standard RISC-V custom instructions. The Gemmini unit uses the RoCC port of a Rocket or BOOM _tile_, and by default connects to the memory system through the System Bus (i.e., directly to the L2 cache).

At the heart of the accelerator lies a systolic array which performs matrix multiplications. By default, the matrix multiplication support both _output-stationary_ and _weight-stationary_ dataflows, which programmers can pick between at runtime. However, the dataflow can also be hardened at elaboration time.

The systolic array's inputs and outputs are stored in an explicity managed scratchpad, made up of banked SRAMs. A DMA engine facilitates the transfer of data between main memory and the scratchpad.

Because weight-stationary dataflows require an accumulator outside the systolic array, we add a final SRAM bank, equipped with adder units, which can be conceptually considered an extension of the scratchpad memory space. The systolic array can store results to any address in the accumulator, and can also read new inputs from any address in the accumulator. The DMA engine can also tranfer data directly between the accumulator and main memory, which is often necessary to load in biases.

Gemmini also includes peripheral circuitry to optionally apply activation functions such as ReLU or ReLU6, scale results down by powers-of-2 to support quantized workloads, or to transpose matrices before feeding them into the systolic array to support the output-stationary dataflow.

Generator Parameters
--------------------------

Major parameters of interest include:

* Systolic array dimensions (``tileRows``, ``tileColumns``, ``meshRows``, ``meshColumns``): The systolic array is composed of a 2-level hierarchy, in which each tile is fully combinational, while a mesh of tiles has pipeline registers between each tile.

![Image description](./gemmini-systolic-array.png)

* Dataflow parameters (``dataflow``): Determine whether the systolic array in Gemmini is output-stationary or weight-stationary, or whether it supports both dataflows so that programmers may choose between them at runtime.

* Scratchpad and accumulator memory parameters (``sp_banks``, ``sp_capacity``, ``acc_capacity``): Determine the properties of the Gemmini scratchpad memory: overall capacity of the scratchpad or accumulators (in KiB), and the number of banks the scratchpad is divided into.

* Type parameters (``inputType``, ``outputType``, ``accType``):
Determine the data-types flowing through different parts of a Gemmini accelerator.
For example, ``inputType`` may be an 8-bit fixed-point number, while ``accType``, which determines the type of partial accumulations in a matrix multiplication, may be a 32-bit integer.
``outputType`` only determines the type of the data passed between two processing elements (PEs); for example, an 8-bit multiplication may produce a 16-bit result which must be shared between PEs in a systolic array.
    - Examples of possible datatypes are:
        - `SInt(8.W)` for a signed 8-bit integer
        - `UInt(32.W)` for an unsigned 32-bit integer
        - `Float(8, 24)` for a single-precision IEEE floating point number
    - If your datatype is a floating-point number, then you might also want to change the ``pe_latency`` parameter, which specifies how many shift registers to add inside the PEs.
This might be necessary if your datatype cannot complete a multiply-accumulate operation within a single cycle.

* Access-execute queue parameters (``ld_queue_length``, ``st_queue_length``, ``ex_queue_length``, ``rob_entries``): To implement access-execute decoupling, a Gemmini accelerator has a load instruction queue, a store instruction queue, and an execute instruction queue. The relative sizes of these queue determine the level of access-execute decoupling. Gemmini also implements a reorder buffer (ROB) - the number of entries in the ROB determines possible dependency management limitations.

* DMA parameters (``dma_maxbytes``, ``dma_buswidth``, ``mem_pipeline``): Gemmini implements a DMA to move data from main memory to the Gemmini scratchpad, and from the Gemmini accumulators to main memory. The size of these DMA transactions is determined by the DMA parameters. These DMA parameters are tightly coupled with Rocket Chip SoC system parameters: in particular ``dma_buswidth`` is associated with the ``SystemBusKey`` ``beatBytes`` parameter, and ``dma_maxbytes`` is associated with ``cacheblockbytes`` Rocket Chip parameters.

There are also optional features, which can be either enabled or left out of Gemmini at elaboration-time.
For example:

* Scaling during "move-in" operations (``mvin_scale_args``, ``mvin_scale_acc_args``):
When data is being moved in from DRAM or main memory into Gemmini's local scratchpad memory, it can optionally be multiplied by a scaling factor.
These parameters specify what the datatype of the scaling factor is, and how the scaling is actually done.
If these are set to ``None``, then this optional feature will be disabled at elaboration time.
If both the scratchpad inputs are accumulator inputs are to be scaled in the same say, then the ``mvin_scale_shared`` parameter can be set to ``true`` so that the multipliers and functional units are shared.

Software
==========

The Gemmini non-standard ISA extension is specified in the `ISA` section below.
The ISA includes configuration instructions, data movement instructions (from main memory to the Gemmini scratchpad, and from the Gemmini accumulators to main memory), and matrix multiplication execution instructions. 

Since Gemmini instructions are not exposed through the GNU binutils assembler, several C macros are provided in order to construct the instruction encodings to call these instructions.

The Gemmini generator includes a C matrix multiplication library which wraps the calls to the custom Gemmini instructions.
The ``software`` directory of the generator includes the aforementioned library and macros, as well as bare-metal tests, and some FireMarshal workloads to run the tests in a Linux environment. In particular, the matrix multiplication C library can be found in the ``software/gemmini-rocc-tests/include/gemmini.h`` file. 

The Gemmini generator generates a C header file based on the generator parameters. This header files gets compiled together with the matrix multiplication library to tune library performance. The generated header file can be found under ``software/gemmini-rocc-tests/include/gemmini_params.h``

Gemmini can also be used to run ONNX-specified neural-networks through a port of Microsoft's ONNX-Runtime framework. The port is included as the [onnxruntime-riscv](https://github.com/pranav-prakash/onnxruntime-riscv) repository submoduled in the `software` directory.
To start using ONNX-Runtime, run `git submodule update --init --recursive software/onnxruntime-riscv`, and read the documentation [here](https://github.com/pranav-prakash/onnxruntime-riscv/blob/systolic/systolic_runner/docs).

## Build and Run Gemmini Tests

To build the Gemmini tests:

```shell
cd software/gemmini-rocc-tests/
./build.sh
```

Afterwards, the test binaries will be found in `software/gemmini-rocc-tests/build`. Binaries whose names end in `-baremetal` are meant to be run in a bare-metal environment, while binaries whose names end in `-linux` are meant to run in a Linux environment. You can run the tests either on a cycle-accurate RTL simulator, or on a (much faster) functional ISA simulator called Spike.

We use a special fork of Spike, found [here](https://github.com/ucb-bar/esp-isa-sim), which has support for Gemmini instructions. If you are using Chipyard, you can easily build Spike by running `./scripts/build-toolchains.sh esp-tools` from Chipyard's root directory. Then, to run the `mvin_mvout` test, which simply moves a matrix into Gemmini's scratchpad before moving it back out into main memory, run the following commands:

```shell
cd build/bareMetalC
spike --extension=gemmini mvin_mvout-baremetal
```

## Writing Your Own Gemmini Tests
`software/gemmini-rocc-tests/bareMetalC/template.c` is a template Gemmini test that you can base your own Gemmini tests off of. To write your own Gemmini test, run:

```bash
cd software/gemmini-rocc-tests/
cp bareMetalC/template.c bareMetalC/my_test.c
```

Then, add `my_test` to the `tests` list at the top of `bareMetalC/Makefile`. Afterwards, running `./build.sh` will install `my_test-baremetal` in `build/bareMetalC`.

# ISA

This section describes Gemmini's assembly-level ISA which is made up of custom RISC-V instructions.

## Data Movement
### `mvin` Move Data From L2/DRAM to Scratchpad
**Format:** `mvin rs1, rs2`
- `rs1` = virtual DRAM address (byte addressed) to load into scratchpad
- `rs2[31:0]` = local scratchpad address (systolic array single-axis addressed; i.e. `tileColumns x meshColumns x dataBytes` bytes of data are captured in 1 address)
    - if the 32nd (Most Significant) bit is set to logical 1, `rs2[31:0]` refers to an address in the accumulator memory space. In this case, the bitwidth of the elements is `tileColumns x meshColumns x accumulated result bitwidth`.
- `rs2[47:32]` = number of columns to load in
- `rs2[63:48]` = number of rows to load in
- `funct` = 2

**Action:** Scratchpad[rs2] <= DRAM[Translate[rs1]]
- Loads a fixed amount of data (`tileColumns x meshColumns x tileRows x meshRows x dataBytes` bytes) into the scratchpad
- Load is sequential from the rs1/rs2 base addresses. Stride must be set by the `config_mvin` command

### `mvout` Move Data from Scratchpad to L2/DRAM
**Format:** `mvout rs1, rs2`
- `rs1` = virtual DRAM address (byte addressed) to write to from scratchpad
- `rs2[31:0]` = local scratchpad address (systolic array single-axis addressed; i.e. `tileColumns x meshColumns x dataBytes` bytes of data are captured in 1 address)
    - if the 32nd (Most Significant) bit is set to logical 1, `rs2[31:0]` refers to an address in the the accumulator memory space. In this case, the bitwidth of the elements is `tileColumns x meshColumns x accumulated result bitwidth`.
    - if the 30th bit is set to logical 1, the `mvout` command will store the full accumulator row in main memory, rather than scaling it down to the input type. Activation functions and accumulator scaling will not be applied in this case
- `rs2[47:32]` = number of columns to store
- `rs2[63:48]` = number of rows to store
- `funct` = 3

**Action:** DRAM[Translate[rs2]] <= Scratchpad[rs1]
- Stores a fixed amount of data (`tileColumns x meshColumns x tileRows x meshRows x dataBytes` bytes) from the scratchpad to L2/DRAM
- Store is sequential from the rs1/rs2 base addresses. Stride must be set by the `config_mvout` command

## Configuration
### `config_ex` configures the Execute pipeline
**Format:** `config_ex rs1 rs2`
- `rs1[0:1]` must be `00`
- `rs1[2]` determines if output (0) or weight (1) stationary
- `rs1[4:3]` = activation function: either relu (1), relu6 (2), or no activation function (0)
- `rs1[8]` = should A be transposed?
- `rs1[9]` = should B be transposed?
- `rs1[31:16]` = the stride (in scratchpad addresses) by which the rows of A are fed into the systolic array.
"A" in this context refers to the left-hand matrix A in the matmul represented by A * B = C.
If this stride is 1, then we feed consecutive rows in the scratchpad, starting from the starting address of A, into the systolic array as the A matrix.
If the stride is 2, then we feed every other row into the systolic array instead.
- `rs1[63:32]` = the number of bits by which the accumulated result of a matmul is right-shifted when leaving the accumulator
- `rs2[31:0]` = the number of bits by which the accumulated result of a matmul is right-shifted when leaving the systolic array
- `rs2[63:32]` = the number of bits by which 6 should be left-shifted before applying relu6
- `funct` = 0

**Action:** mode <= rs1(2); shift <= rs2; A_stride <= rs1[31:16]

**Notes:**
- As of now, certain combinations of transpose options cannot be performed unless the right dataflow is chosen.
This limitation may be lifted in the future.

| Dataflow | Transpose A | Transpose B | Permitted? |
| :---: | :---: | :---: | :---: | 
| OS | No | No | Yes |
| OS | No | Yes | No |
| OS | Yes | No | Yes |
| OS | Yes | Yes | Yes |
| WS | No | No | Yes |
| WS | No | Yes | Yes |
| WS | Yes | No | Yes |
| WS | Yes | Yes | No |

### `config_mvin` configures the Load pipeline
**Format:** `config_mvin rs1 rs2`
- `rs1[0:1]` must be `01`
- `rs1[2]` is 0 if `mvin`s to the accumulator have the same bitwidth as accumulator types, and 1 if they have the same bitwidth as inputs to the systolic array
- `rs1[4:3]` is 0 if the stride is being set for `mvin`, 1 if the stride is being set for `mvin2`, and 2 if the stride is being set for `mvin3`
- `rs1[63:32]` is the "scale" by which to multiply data as it's being moved in to the scratchpad. This is ignored if Gemmini isn't built with the capability to scale values during `mvin`s.
- `rs2` = the stride in bytes
- `funct` = 0

**Action:** stride <= rs2; scale <= rs1[63:32]

### `config_mvout` configures the Store pipeline
**Format:** `config_mvout rs1 rs2`
- `rs1[0:1]` must be `10`
- `rs2` = the stride in bytes 
- `funct` = 0

During `mvout` operations, Gemmini can also perform max-pooling.
**This is an experimental feature, and is subject to change.**
This feature assumes that data is stored in the scratchpad or accumulator in NHWC format.
The parameters controlling this feature are:

- `rs1[5:4]` = max-pooling stride. If this is 0, then max-pooling is deactivated.
- `rs1[7:6]` = max-pooling window size
- `rs1[9:8]` = upper zero-padding
- `rs1[11:10]` = left zero-padding
- `rs1[31:24]` = output dimension of image after pooling
- `rs1[39:32]` = number of pooled rows to output
- `rs1[47:40]` = number of pooled columns to output
- `rs1[55:48]` = number of unpooled rows to pool
- `rs1[63:56]` = number of unpooled columns to pool

**Action:** stride <= rs2; max-pooling parameters <= rs1

### `flush` flushes the TLB
**Format:** `flush rs1`
- `rs1` = If `rs1[0]` is 1, then the current TLB request is skipped (if its waiting for an interrupt). Otherwise, the current TLB request is repeated.

**Notes:**

- This instruction executes _as soon as it is received_ without waiting for other instructions which may be queued up. It is the programmer's responsibility to insert fences if necessary.

## Core Matmul Sequences
Every single matrix multiply operation is a combination of `matmul.preload` and `matmul.compute` (due to the length of a single instruction it was split into two instructions). `matmul.preload` should precede the `matmul.compute`.

Example:
```
//// first matmul ////
// rs1 = InputD
// rs2 = OutputC
// rs3 = InputA
// rs4 = InputB
//matmul InputA InputB OutputC InputD
1. matmul.preload $rs1 $rs2
2. matmul.compute $rs3 $rs4
```
**Action:** Scratchpad[rs2] <= Scratchpad[rs3] \* Scratchpad[rs4] + Scratchpad[rs1]

**Notes on addressing:**
- For B or D, the address can be replaced with all high bits to input a 0 matrix instead.
- If the 32nd bit of any address is high, it will point to the accumulator's memory space.

### Preloading
**Format:** `matmul.preload rs1, rs2`
- `rs1[31:0]` = local scratchpad address (systolic array single-axis addressed) of D matrix (when output-stationary), or B matrix (when weight-stationary)
- `rs1[47:32]` = number of columns of D/B matrix
- `rs1[63:48]` = number of rows of D/B matrix
- `rs2[31:0]` = local scratchpad address (systolic array single-axis addressed) of C matrix. If this is set to all high bits, then C will not be written to the scratchpad. If the 32nd _and_ 31st bits are high, the result will be accumulated on top of the previous result pointed to by this address in the accumulator
- `rs2[47:32]` = number of columns of C matrix
- `rs2[63:48]` = number of rows of C matrix
- `funct` = 6

**Commit Behavior:** This instruction commits on the cycle after the systolic array receives it. The systolic array remains idle until the subsequent OS/WS specific instructions are seen.

### Computing
#### Explicitly Preloaded
**Format:** `matmul.compute.preloaded rs1, rs2`
- `rs1[31:0]` = local scratchpad address (systolic array single-axis addressed) of A matrix
- `rs1[47:32]` = number of columns of A matrix
- `rs1[63:48]` = number of rows of A matrix
- `rs2[31:0]` = local scratchpad address (systolic array single-axis addressed) of B matrix (when output-stationary), or D matrix (when weight-stationary)
- `rs2[47:32]` = number of columns of B/D matrix
- `rs2[63:48]` = number of rows of B/D matrix
- `funct` = 4
- This instruction will compute on the value preloaded (D if output-stationary, or B if weight-stationary)

#### Accumulate on Previous Computation
**Format:** `matmul.compute.accumulated rs1, rs2`
- `funct` = 5
- `rs1` and `rs2` have the same encoding as the `matmul.compute.preloaded` encoding
- If output-stationary, this instruction will compute on the previously computed result (C) in the systolic array
- If weight-stationary, this instruction will compute on the previously preloaded weights (B) in the systolic array

# Hardware Tiler ISA

This section describes an additional set of RoCC instructions that configure and invoke the hardware tiler in Gemmini. From the software's perspective the original instructions and these new instructions should be seen as totally unrelated. It is easy for software to safely interleave original instructions with these hardware tiling instructions, but this comes at the cost of increased serialization latency; for example, if you issue an original `preload` and `matmul` from above, and then immediately issue a `compute_cisc`, the last RoCC instruction will not start to be processed until the `preload` and `matmul` are completely finished from the software's perspective (the same works in the opposite direction -- `compute_cisc` before a `preload` and `matmul`).

## Hardware Tiler Configuration
### `config_cisc` configures the Execute pipeline
**Format:** `config_ex rs1 rs2`
- `rs1[4:3]` = activation function: either relu (1), relu6 (2), or no activation function (0)
- `rs1[63:32]` = the number of bits by which the accumulated result of a matmul is right-shifted when leaving the accumulator
- `rs2[31:0]` = the number of bits by which the accumulated result of a matmul is right-shifted when leaving the systolic array
- `rs2[63:32]` = the number of bits by which 6 should be left-shifted before applying relu6
- `funct` = 10

**Action:** semantically equivalent subset of the `config_ex` instruction fields.

### `addr_ab` sets the virtual address of the A and B matrices
**Format:** `addr_ab rs1, rs2`
- `rs1` = the A matrix virtual address
- `rs2` = the B matrix virtual address
- `funct` = 11

### `addr_cd` sets the virtual address of the C and D matrices
**Format:** `addr_ab rs1, rs2`
- `rs1` = the C matrix virtual address
- `rs2` = the D matrix virtual address
- `funct` = 12

### `size_mn` sets the M and N dimensions in terms of elements
**Format:** `size_mn rs1, rs2`
- `rs1` = sets M, the number of rows in A, C, and D matrices 
- `rs2` = sets N, the number of columns in B, C, and D matrices
- `funct` = 13

### `size_k` sets the K dimension in terms of elements
**Format:** `size_k rs1`
- `rs1` = sets K, the number of columns in A, and rows in B
- `funct` = 14

### `RPT_BIAS` sets whether the D is a repeating row
**Format:** `rpt_bias rs1`
- `rs1[0]` = set to 1 if D is not a 2D matrix, but really a 1D row that should be duplicated by the hardware
- `funct` = 15

### `reset` resets the hardware tiler input processing engine
**Format:** `reset`
- `funct` = 16

**Action:** this is only used if Gemmini has entered an error state while in the hardware-tiler mode due to invalid RoCC commands. Using this command will reset Gemmini. However, right now, there is no way for the software to check for these errors. 


### `COMPUTE_CISC` runs a complete hardware tiling sequence with the configured A, B, C, D, M, N, K, RPT_BIAS values
**Format:** `compute_cisc`
- `funct` = 17

# Citing Gemmini
If Gemmini helps you in your academic research, you are encouraged to cite our paper. Here is an example bibtex:
```
@INPROCEEDINGS{gemmini-dac,
  author={Genc, Hasan and Kim, Seah and Amid, Alon and Haj-Ali, Ameer and Iyer, Vighnesh and Prakash, Pranav and Zhao, Jerry and Grubb, Daniel and Liew, Harrison and Mao, Howard and Ou, Albert and Schmidt, Colin and Steffl, Samuel and Wright, John and Stoica, Ion and Ragan-Kelley, Jonathan and Asanovic, Krste and Nikolic, Borivoje and Shao, Yakun Sophia},
  booktitle={Proceedings of the 58th Annual Design Automation Conference (DAC)}, 
  title={Gemmini: Enabling Systematic Deep-Learning Architecture Evaluation via Full-Stack Integration}, 
  year={2021},
  volume={},
  number={},
  pages={}
}
```
