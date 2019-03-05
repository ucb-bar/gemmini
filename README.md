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

# RoCC ISA
## Data Movement
### `mvin` Move Data From L2/DRAM to Scratchpad
**Format:** `mvin rs1, rs2`
- `rs1` = virtual DRAM address to load into scratchpad
- `rs2` = local scratchpad address (the highest bits of rs2 determine the bank number and the lowests bits determine the entry)

 **-- funct field should be set to 2**
**Action:** Scratchpad[rs2] <= DRAM[Translate[rs1]]
- Loads a fixed amount of data into the scratchpad = `tileRows x meshRows x dataBytes` corresponding to the Mesh's parameterization
- Load is sequential from the rs1/rs2 base address. Any stride or skip operation is implemented in software

**TODO** Decide what to do when `tileRows != tileColumns` or `meshRows != meshColumns` (should these not be considered?)

**Commit Behavior:** This instruction is synchronous and will stall Rocket's pipeline until all the DRAM data is resident in the scratchpad

### `mvout` Move Data from Scratchpad to L2/DRAM
**Format:** `mvout rs1, rs2`
- rs1 = local scratchpad address (the highest bits of rs1 determine the bank number and the lowests bits determine the entry)
- rs2 = virtual DRAM address to write to 

 **-- funct field should be set to 3**

**Action:** DRAM[Translate[rs2]] <= Scratchpad[rs1]
- Stores a fixed amount of data from the scratchpad to L2/DRAM = `tileRows x meshRows x dataBytes`
- Store is sequential from the rs1/rs2 base address. Strides in software.

**Commit Behavior:** Identical to `mvin`, synchronous and will stall until all scratchpad data has been flushed into the L2

## Dataflow Mode
### `setmode` set the mode to weight/output stationary
**Format:** `setmode rs1`
- `rs1` = the lsb of rs1 will determine if output (0.U) or weight (1.U) stationary.

 **-- funct field should be set to 9**
 
**Action:** mode <= rs1(0)

## Core Matmul Sequences
Every single matrix multiply operation is a combination of matmul.preload and matmul.compute (due to the length of a single instruction it was split into two instructions). matmul.preload should preceed the matmul.compute.

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
**Action:** Scratchpad[rs2] <= Scratchpad[rs3]*Scratchpad[rs4]+Scratchpad[rs1]

**Action:** C <= A*B+D
<!---
//// second matmul ////
//matmul InputA2 InputB2 OutputC2 InputD2
3. matmul.preload $rs5 $rs6
4. matmul.compute $rs7 $rs8
-->

<!---

Note that as defined above the data preloaded in matmul.preload is for the matmul in the next instruction (i.e., the data preloaded in 1 is actually used in 4 because 4 is the next matmul instruction). OutputC is always for the CURRENT `matmul` instruction.
-->

The preload command is encoded to funct field #8.
- If you set the preload command's rd value to 1 it will automatically preload zeros.
- If you set the preload command's C value to 0xFFFFFFFF the systolic array will assume the output will remain in the systolic array and won't be read out.
The `matmul.compute` command is encoded to funct fields #4 and #5.
- `matmul.compute.preloaded` associated with funct field #4 will compute on the value preloaded (D) <!---
in the PREVIOUS `matmul` instruction.-->
- `matmul.compute.accumulate` associated with funct field #5 will accumulate on top of the previously computed results <!---of the PREVIOUS `matmul` instruction.-->

<!---
### Preloading
**Format:** `matmul.preload rs1`
- `rs1` = local scratchpad address of B matrix (weight stationary), D matrix (final biasing or output stationary), `0xAAAA_AAAA` (don't preload, use existing state) or `0xFFFF_FFFF` (preload zeros)

**Action:** Mesh[PE Double Buffer Accumulators] <= Scratchpad[rs1] OR zeros OR NOP

**Commit Behavior:** This instruction commits on the cycle after the systolic array receives it. The systolic array remains idle until the subsequent OS/WS specific instructions are seen.

### Output Stationary
Issue the `matmul.os1` instruction to set up A and B for the matmul operation.

**Format:** `matmul.os1 rs1, rs2`
- `rs1` = local scratchpad address of A matrix (stored as rows of A.T)
- `rs2` = local scratchpad address of B matrix (stored as rows of B)

Then issue `matmul.os2` OR `matmul.os2.stay` depending on whether you want to send the matmul result to the scratchpad or hold it inside the PEs' accumulators.

**Format:** `matmul.os2 rs1`
- `rs1` = local scratchpad address to store C matrix (stored as rows of C)

**Format:** `matmul.os2.stay` (no register arguments)

**Commit Behavior:** Upon issuing the sequence of instructions `matmul.preload, matmul.os1, matmul.os2` the sequence will be stored as a single matmul compute unit in a queue inside the RoCC accelerator. These instructions will commit immediately and won't stall Rocket's pipeline.

### Weight Stationary
Similar to the output stationary instructions, there is a similar set of weight stationary instructions `matmul.ws1, matmul.ws2` that trigger a WS dataflow matmul operation.

**Format:** `matmul.ws1 rs1, rs2`
- `rs1` = local scratchpad address of A matrix
- `rs2` = local scratchpad address of partial C matrix OR zeros
    - If `rs2 == 0xFFFF_FFFF`, then feed zeros in on the B-axis in the systolic array
    - Otherwise stream the partial `C` matrix from the scratchpad on the B-axis

**Format:** `matmul.ws2 rs1`
- `rs1` = local scratchpad address to store output C matrix (stored as rows of C)
- **TODO** do we need functionality to keep the output in-place for the WS dataflow?

# Semantics
## Instruction Dependency Management
### TODO: Include examples of all these dependencies
- mvin -> multseq (handled by SW and instruction stream ordering)
- multseq -> multseq (handled by HW, SW doesn't have to worry about polling for completion if there exists a mult -> mult RAW dependency)
- multseq -> mvout (handled by HW, blocking on all previous mults completing)
- mvout -> multseq (WAR dependency is handled by instruction ordering, mvout is blocking)
-->
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
