#!/bin/bash

help () {
  echo "Build a functional simulator for RISCV Gemmini programs, matching"
  echo '`customConfig` in `configs/GemminiCustomConfigs.scala`.'
  echo
  echo "Usage: $0 [-h|--help]"
  echo
  echo "Note:    On Spike, cycle counts, SoC counter values, and performance"
  echo "         statistics are all meaningless. Use Spike only to check if your"
  echo "         programs are functionally correct. For meaningful metrics, you"
  echo "         must run your programs on VCS, Verilator, or Firesim instead."
  exit
}

if [ $# -gt 0 ]; then
   help
fi

export GEMMINI_ONLY_GENERATE_GEMMINI_H=1

cd ../../sims/verilator/
echo Generating new gemmini_params.h file...
make verilog CONFIG=CustomGemminiSoCConfig &> build.log

cd -
cp software/gemmini-rocc-tests/include/gemmini_params.h ../../toolchains/esp-tools/riscv-isa-sim/gemmini/gemmini_params.h
cd ../../toolchains/esp-tools/riscv-isa-sim/build
make && make install
