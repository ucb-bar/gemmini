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
    - A Tile is a fully combinational 2D array of PEs with dimension (`tileRows` X `tileColumns`)
- `meshRows`, `meshColumns`
    - A Mesh is a pipelined 2D array of Tiles with dimension (`meshRows` X `meshColumns`)
    - A Mesh is the top-level 'core' systolic array structure
    - It can natively perform a `matmul` of square and equal dimension matrices
        - e.g. C = A (m x n) `matmul` B (n x k) where `m = n = k = tileRows x meshRows = tileColumns x meshColumns`
    - To perform matmul with non-square matrices, arbitrary inner dimension `n`, or matrices larger than the Mesh, requires software to break the computation down into the Mesh's primitive `matmul`
- `dataWidth` = `dataBytes` x 8
    - The native elaboration-time data width (in bits) of the PEs MAC unit and input/output wires
    - Nominally `dataWidth = 8, dataBytes = 1` as we are designing an INT8 accelerator

# RoCC ISA
## Data Movement
### `mvin` Move Data From L2/DRAM to Scratchpad
**Format:** `mvin rs1, rs2`
- `rs1` = virtual DRAM address to load into scratchpad
- `rs2` = local scratchpad address

**Action:** Scratchpad[rs2] <= DRAM[Translate[rs1]]
- Loads a fixed amount of data into the scratchpad = `tileRows x meshRows x dataBytes` corresponding to the Mesh's parameterization
- Load is sequential from the rs1/rs2 base address. Any stride or skip operation is implemented in software
**TODO** Decide what to do when `tileRows != tileColumns` or `meshRows != meshColumns` (should these not be considered?)

**Commit Behavior:** This instruction is synchronous and will stall Rocket's pipeline until all the DRAM data is resident in the scratchpad

### `mvout` Move Data from Scratchpad to L2/DRAM
**Format:** `mvout rs1, rs2`
- rs1 = virtual DRAM address to write to
- rs2 = local scratchpad address

**Action:** DRAM[Translate[rs1]] <= Scratchpad[rs2]
- Stores a fixed amount of data from the scratchpad to L2/DRAM = `tileRows x meshRows x dataBytes`
- Store is sequential from the rs1/rs2 base address. Strides in software.

**Commit Behavior:** Identical to `mvin`, synchronous and will stall until all scratchpad data has been flushed into the L2

## Core Matmul Sequences
### Generic Instructions
- mult.preload rs1
    - rs1 = `pointer to tileRows X meshRows X dataWidth block of scratchpad` (D matrix)
    - if rs1 == 0x0, then preload all zeros

### Output Stationary
- mult.os1 rs1, rs2
    - rs1 = `pointer to tileColumns X meshColumns X dataWidth block of scratchpad` (A matrix)
    - rs1 = `pointer to tileRows X meshRows X dataWidth block of scratchpad` (B matrix)
- mult.os2 rs1
    - rs1 = `pointer to tileRows X meshRows X dataWidth block of scratchpad` (C matrix) **may be wrong (consider middle dimension)**
    - Fire off matmul into RoCC queue
- mult.os2.stay
    - Fire off matmul into RoCC queue
    - Don't move data out of accumulators into SRAM
### Weight Stationary
- mult.ws1/2
    - Identical encoding to mult.os1/2 but trigger a weight stationary computation
    - If rs2 == 0x0, then feed zeros in on the B-axis in the systolic array

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
mult.preload SaddrD
mult.os1 SaddrA, SaddrB
mult.os2 SaddrC
for (i = 0; i < m; ++i) {
    mvout (DaddrC + i*k*dataBytes) (SaddrD + i*k*dataBytes)
}
```

## Exploiting Parallelism
### Double Buffering Within PEs (Preloads)
### Loads During Matmul
### Scratchpad Banking For Max Throughput
