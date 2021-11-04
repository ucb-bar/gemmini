#!/bin/bash

checkpoint="$1"

if [ $checkpoint == "build-verilator" ]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Inference
    rtl=generated-src-inference

    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src
elif [ $checkpoint == "build-complex" ]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Complex
    rtl=generated-src-complex

    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src
else
    echo Unknown checkpoint
    exit 1
fi

echo Loaded checkpoint $checkpoint

