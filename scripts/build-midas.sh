#!/bin/bash

if [ "$1" == "--help" ]; then
    echo usage: $0 DRAM_CONTROLLER_MODEL
    echo " " DRAM_CONTROLLER_MODEL: Either DDR3FCFS or DDR3FRFCFS or DDR3FRFCFSLLC4MB
    echo "   " FCFS is "first come, first serve"
    echo "   " FRFCFS is "first ready, first come, first serve"
    exit
elif [ "$1" == "" ]; then
    echo DRAM model must be provided
    exit 1
fi

cd ../../sims/firesim/
source sourceme-f1-manager.sh &> build.log

cd sim/
make verilator TARGET_CONFIG=${1}_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig

