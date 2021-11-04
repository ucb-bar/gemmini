#!/bin/bash

checkpoint="$1"

if [ $checkpoint == "build-verilator" ]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Inference
    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
elif [ $checkpoint == "build-complex" ]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Complex
    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
else
    echo Unknown checkpoint
    exit 1
fi

echo Loaded checkpoint $checkpoint

