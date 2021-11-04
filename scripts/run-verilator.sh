#!/bin/bash

cd ../../sims/verilator/
./simulator-chipyard-CustomGemminiSoCConfig ../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/${1}-baremetal

