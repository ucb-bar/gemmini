#!/bin/bash

cd ../../sims/verilator/
./simv-chipyard-CustomGemminiSoCConfig ../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/${1}-baremetal

