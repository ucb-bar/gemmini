#!/bin/bash

checkpoint="$1"

if [ $checkpoint == "build-verilator" ]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Inference
    rtl=generated-src-inference

    rm -rf /root/chipyard/sims/verilator/generated-src

    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src
elif [ $checkpoint == "build-complex" ]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Complex
    rtl=generated-src-complex

    rm -rf /root/chipyard/sims/verilator/generated-src

    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src
elif [ $checkpoint == "build-midas" ]; then
    build=/root/builds/DDR3FCFS-MIDAS
    target=/root/chipyard/sims/firesim/sim/f1/FireSim-DDR3FCFS_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig-BaseF1Config

    rm -rf $target
    cp -r $build $target
else
    echo Unknown checkpoint
    exit 1
fi

echo Loaded checkpoint $checkpoint

