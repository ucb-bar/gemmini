#!/bin/bash

cd ../../sims/verilator/
./simulator-chipyard-CustomGemminiSoCConfig-debug -v waveform.vcd ../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/${1}-baremetal

