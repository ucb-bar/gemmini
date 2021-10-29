#!/bin/bash

if [ "$1" == "--help" ]; then
    echo usage: $0 DRAM_CONTROLLER_MODEL
    echo " " DRAM_CONTROLLER_MODEL: Either DDR3FRFCFS or DDR3FRFCFSLLC4MB or DDR3FCFS or DDR3FCFSLLC4MB
    echo "   " FRFCFS is "first ready, first come, first serve"
    echo "   " FCFS is "first come, first serve"
    exit
elif [ "$1" == "" ]; then
    echo DRAM model must be provided
fi

cd ../../sims/firesim/sim
make verilator TARGET_CONFIG=${1}_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig

