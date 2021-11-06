#!/bin/bash

path=""
suffix=""

binary="$1"

if [ "$binary" == "" ]; then
    echo You must provide a binary to run
elif [ -f "../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/${binary}-baremetal" ]; then
    path="../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/"
    suffix="-baremetal"
elif [ -f "../../generators/gemmini/software/gemmini-rocc-tests/build/imagenet/${binary}-baremetal" ]; then
    path="../../generators/gemmini/software/gemmini-rocc-tests/build/imagenet/"
    suffix="-baremetal"
elif [ ! -f "$binary" ]; then
    echo Binary not found
    exit 1
fi

cd ../../sims/verilator/
./simulator-chipyard-CustomGemminiSoCConfig ${path}${binary}${suffix}

