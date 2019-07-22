Systolic Array Project
=======================
Generator for configurable systolic arrays. Supports configurable dimensions, precision, and input and weight stationary dataflows. Integrates with Rocket as an RoCC accelerator.

# Architecture
## Core Systolic Architecture (PE/Tile/Mesh)
## Systolic Array Driver + Controller
### Local Memory System
## Integration with Rocket
## Top-Level Generator Parameters
- `tileRows`, `tileColumns`
    - A Tile is a fully combinational 2D array of PEs with dimension (`tileRows` x `tileColumns`)
- `meshRows`, `meshColumns`
    - A Mesh is a pipelined 2D array of Tiles with dimension (`meshRows` x `meshColumns`)
    - A Mesh is the top-level 'core' systolic array structure
    - It can natively perform a `matmul` of square and equal dimension matrices
        - e.g. C = A (m x n) `matmul` B (n x k) where `m = n = k = tileRows x meshRows = tileColumns x meshColumns`
    - To perform matmul with non-square matrices, arbitrary inner dimension `n`, or matrices larger than the Mesh, requires software to break the computation down into the Mesh's primitive `matmul`
- `dataWidth` = `dataBytes` x 8
    - The native elaboration-time data width (in bits) of the PEs MAC unit and input/output wires
    - Nominally `dataWidth = 8, dataBytes = 1` as we are designing an INT8 accelerator
- For simplicity, we assume `meshRows = meshColumns` and `tileRows = tileColumns`

# RoCC ISA
## Data Movement
### `mvin` Move Data From L2/DRAM to Scratchpad
**Format:** `mvin rs1, rs2`
- `rs1` = virtual DRAM address (byte addressed) to load into scratchpad
- `rs2` = local scratchpad address (systolic array single-axis addressed; i.e. `tileColumns x meshColumns x dataBytes` bytes of data are captured in 1 address)
    - the highest bits of `rs2` determine the bank number and the lowest bits determine the entry in the scratchpad
- `funct` = 2

**Action:** Scratchpad[rs2] <= DRAM[Translate[rs1]]
- Loads a fixed amount of data (`tileColumns x meshColumns x tileRows x meshRows x dataBytes` bytes) into the scratchpad
- Load is sequential from the rs1/rs2 base addresses. Stride must be set by the `config_mvin` command

### `mvout` Move Data from Scratchpad to L2/DRAM
**Format:** `mvout rs1, rs2`
- `rs1` = virtual DRAM address (byte addressed) to write to from scratchpad
- `rs2` = local scratchpad address (systolic array single-axis addressed; i.e. `tileColumns x meshColumns x dataBytes` bytes of data are captured in 1 address)
    - the highest bits of `rs2` determine the bank number and the lowests bits determine the entry in the scratchpad
    - if the 32nd bit is 1, `rs2` refers the accumulator memory space. In this case, the bitwidth of the elements is the accumulated result bitwidth
- `funct` = 3

**Action:** DRAM[Translate[rs2]] <= Scratchpad[rs1]
- Stores a fixed amount of data (`tileColumns x meshColumns x tileRows x meshRows x dataBytes` bytes) from the scratchpad to L2/DRAM
- Store is sequential from the rs1/rs2 base addresses. Stride must be set by the `config_mvout` command

## Configuration
### `config_ex` configures the Execute pipeline
**Format:** `config_ex rs1 rs2`
- `rs1` = `rs1[0:1]` must be `00`. `rs1[2]` will determine if output (0) or weight (1) stationary. `rs1[4:3]` will determine the activation function: either relu (1), relu6 (2), or no activation function (0).
- `rs[63:32]` is the number of bits by which the accumulated result of a matmul is right-shifted when leaving the accumulator
- `rs2[31:0]` = the number of bits by which the accumulated result of a matmul is right-shifted when leaving the systolic array
- `rs2[63:32]` = the number of bits by which 6 should be left-shifted before applying relu6
- `funct` = 0

**Action:** mode <= rs1(2); shift <= rs2

### `config_mvin` configures the Load pipeline
**Format:** `config_mvin rs1 rs2`
- `rs1` = `rs1[0:1]` must be `01`
- `rs2` = the stride in bytes
- `funct` = 0

**Action:** stride <= rs2

### `config_mvout` configures the Store pipeline
**Format:** `config_mvout rs1 rs2`
- `rs1` = `rs1[0:1]` must be `10`
- `rs2` = the stride in bytes
- `funct` = 0

**Action:** stride <= rs2

### `flush` flushes the TLB
**Format:** `flush rs1`
- `rs1` = If `rs1[0]` is 1, then the current TLB request is skipped (if its waiting for an interrupt). Otherwise, the current TLB request is repeated.

**Notes:**

- This instruction executes _as soon as it is received_ without waiting for other instructions which may be queued up. It is the programmer's responsibility to insert fences if necessary.
- Dependency bits cannot be appended to this instruction.

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
- `rs1` = local scratchpad address (systolic array single-axis addressed) of D matrix (when output-stationary), or B matrix (when weight-stationary)
- `rs2` = local scratchpad address (systolic array single-axis addressed) of C matrix. If this is set to all high bits, then C will not be written to the scratchpad. If the 32nd _and_ 31st bits are high, the result will be accumulated on top of the previous result pointed to by this address in the accumulator
- `funct` = 6

**Commit Behavior:** This instruction commits on the cycle after the systolic array receives it. The systolic array remains idle until the subsequent OS/WS specific instructions are seen.

### Computing
#### Explicitly Preloaded
**Format:** `matmul.compute.preloaded rs1, rs2`
- `rs1` = local scratchpad address (systolic array single-axis addressed) of A matrix
- `rs2` = local scratchpad address (systolic array single-axis addressed) of B matrix (when output-stationary), or D matrix (when weight-stationary)
- `funct` = 4
- This instruction will compute on the value preloaded (D if output-stationary, or B if weight-stationary)

#### Accumulate on Previous Computation
**Format:** `matmul.compute.accumulated rs1, rs2`
- `funct` = 5
- `rs1` and `rs2` have the same encoding as the `matmul.compute.preloaded` encoding
- If output-stationary, this instruction will compute on the previously computed result (C) in the systolic array
- If weight-stationary, this instruction will compute on the previously preloaded weights (B) in the systolic array

# Semantics
## Instruction Dependency Management
### TODO: Include examples of all these dependencies
- mvin -> multseq (handled by SW and instruction stream ordering)
- multseq -> multseq (handled by HW, SW doesn't have to worry about polling for completion if there exists a mult -> mult RAW dependency)
- multseq -> mvout (handled by HW, blocking on all previous mults completing)
- mvout -> multseq (WAR dependency is handled by instruction ordering, mvout is blocking)

# Software Examples
## Basic Output Stationary Mapping
We want to calculate C = A x B + D.
- Dimensions: A (m x n), B (n x k), D (m x k), C (m x k)
- For simplicity assume m = n = k = `tileRows` x `meshRows` = `tileCols` x `meshCols`
    - In our typical systolic array parameterization, `tileRows = tileCols = 2`, and `meshRows = meshCols = 8`
- Assume A.T (transposed A), B, D are stored in DRAM row-major
- A needs to be fed into the systolic array column-wise (so it is stored transposed), while B needs to be fed in row-wise
- D is fed in row-wise from the top of the systolic array (this is done before A and B are fed simultaneously)

This sequence of instructions performs the matmul:
- Assume A, B, D are stored at addresses DaddrA, DaddrB, DaddrD in DRAM
- Assume there's space in DRAM at DaddrC for the C result matrix
- Assume the SW has reserved scratchpad space for A, B, C, D at addresses SaddrA, SaddrB, SaddrC, SaddrD

```
for (i = 0; i < n; ++i) {
    mvin (DaddrA + i*m*dataBytes) (SaddrA + i*m*dataBytes)
}
for (i = 0; i < n; ++i) {
    mvin (DaddrB + i*k*dataBytes) (SaddrB + i*k*dataBytes)
}
for (i = 0; i < m; ++i) {
    mvin (DaddrD + i*k*dataBytes) (SaddrD + i*k*dataBytes)
}
matmul.preload SaddrD
matmul.os1 SaddrA, SaddrB
matmul.os2 SaddrC
for (i = 0; i < m; ++i) {
    mvout (DaddrC + i*k*dataBytes) (SaddrD + i*k*dataBytes)
}
```

## Exploiting Parallelism
### Double Buffering Within PEs (Preloads)
### Loads During Matmul
### Scratchpad Banking For Max Throughput
