#!/bin/bash

checkpoint="$1"

if [[ $checkpoint == "build-verilator" ]]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Inference
    rtl=generated-src-inference

    rm -rf /root/chipyard/sims/verilator/generated-src
    rm -rf /root/chipyard/generators/gemmini/software/gemmini-rocc-tests/build/

    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src
    cp -r /root/builds/baremetalc-integer /root/chipyard/generators/gemmini/software/gemmini-rocc-tests/build
elif [[ $checkpoint == "build-complex" ]]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Complex
    rtl=generated-src-complex

    rm -rf /root/chipyard/sims/verilator/generated-src
    rm -rf /root/chipyard/generators/gemmini/software/gemmini-rocc-tests/build/

    cp /root/builds/$build /root/chipyard/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src

    cp -r /root/builds/baremetalc-complex /root/chipyard/generators/gemmini/software/gemmini-rocc-tests/build
elif [[ $checkpoint == "build-midas" ]]; then
    build=/root/builds/DDR3FCFS-MIDAS
    target=/root/chipyard/sims/firesim/sim/generated-src/f1/FireSim-DDR3FCFS_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig-BaseF1Config

    rm -rf $target
    rm -rf /root/chipyard/generators/gemmini/software/gemmini-rocc-tests/build/
    cp -r $build $target
    cp -r /root/builds/$rtl /root/chipyard/sims/verilator/generated-src
    cp -r /root/builds/baremetalc-integer /root/chipyard/generators/gemmini/software/gemmini-rocc-tests/build
elif [[ $checkpoint == "build-onnx-inference" ]]; then
    cp /root/builds/ort_test /root/chipyard/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_runner/

    cd /root/chipyard/generators/gemmini/
    cp /root/builds/gemmini_params_int.h ../../toolchains/esp-tools/riscv-isa-sim/gemmini/gemmini_params.h
    cd ../../toolchains/esp-tools/riscv-isa-sim/build
    make && make install
elif [[ $checkpoint == "build-onnx-training" ]]; then
    cp /root/builds/resnet_train /root/chipyard/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_trainer/

    cd /root/chipyard/generators/gemmini/
    cp /root/builds/gemmini_params_fp.h ../../toolchains/esp-tools/riscv-isa-sim/gemmini/gemmini_params.h
    cd ../../toolchains/esp-tools/riscv-isa-sim/build
    make && make install
else
    echo Unknown checkpoint
    echo Supported checkpoints:
    echo "	build-verilator: 	verilator build wth baseline inference gemmini config"
    echo "	build-complex:   	verilator build wth complex number gemmini config"
    echo "	build-midas:   	 	MIDAS build wth basline inference config"
    echo "	build-onnx-inference:	int8 gemmini spike and inference onnxruntime binary"
    echo "	build-onnx-training:	fp32 gemmini spike and training onnxruntime binary"
    exit 1
fi

echo Loaded checkpoint $checkpoint

