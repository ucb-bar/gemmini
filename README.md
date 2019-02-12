Systolic Array Project
=======================
Generator for configurable systolic arrays. Supports configurable dimensions, precision, and input and weight stationary dataflows. Integrates with Rocket as an RoCC accelerator.

# Architecture
## Core Systolic Architecture (PE/Tile/Mesh)
## Systolic Array Driver + Controller
### Local Memory System
## Integration with Rocket
## Top-Level Generator Parameters

# RoCC ISA
## Data Movement
### `mvin` Move Data From L2/DRAM to Scratchpad
Format: `mvin rs1, rs2`
- rs1 = virtual DRAM address to load into scratchpad
- rs2 = local scratchpad address
Action: Scratchpad[rs2] <= DRAM[Translate[rs1]]
Requirements:
- rs2 must be aligned to the 'systolic block size'
### `mvout` Move Data from Scratchpad to L2/DRAM
- mvout rs1, rs2
    - rs1 = `dram_addr`
    - rs2 = `scratchpad_addr`
## Core Matmul Sequences
- mult.os1 rs1, rs2
    - rs1 = `pointer to tileColumns X meshColumns X dataWidth block of scratchpad` (A matrix)
    - rs1 = `pointer to tileRows X meshRows X dataWidth block of scratchpad` (B matrix)
- mult.os2 rs1
    - rs1 = `pointer to tileRows X meshRows X dataWidth block of scratchpad` (C matrix) **may be wrong (consider middle dimension)**
    - Fire off matmul into RoCC queue
- mult.os2.stay
    - Fire off matmul into RoCC queue
    - Don't move data out of accumulators into SRAM
- mult.preload rs1
    - rs1 = `pointer to tileRows X meshRows X dataWidth block of scratchpad` (D matrix)
    - if rs1 == 0x0, then preload all zeros
- mult.ws1/2
    - Identical encoding to mult.os1/2 but trigger a weight stationary computation
    - If rs2 == 0x0, then feed zeros in on the B-axis in the systolic array

# Semantics
## Instruction Retirement
- mvin/mvout instructions are synchronous (will block Rocket's pipeline until the moves are fully completed and the data are resident in the scratchpad/L2)
## Instruction Dependency Management
### TODO: Include examples of all these dependencies
- mvin -> multseq (handled by SW and instruction stream ordering)
- multseq -> multseq (handled by HW, SW doesn't have to worry about polling for completion if there exists a mult -> mult RAW dependency)
- multseq -> mvout (handled by HW, blocking on all previous mults completing)
- mvout -> multseq (WAR dependency is handled by instruction ordering, mvout is blocking)

# Software Examples
## Basic Output Stationary Mapping
## Exploiting Parallelism
### Double Buffering Within PEs (Preloads)
### Loads During Matmul
### Scratchpad Banking For Max Throughput
