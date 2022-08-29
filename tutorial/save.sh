#!/bin/bash

set -e

checkpoint="$1"
checkpoints_dir="$2"

if [ "$checkpoints_dir" == "" ]; then
    checkpoints_dir="/root/builds/"
fi

CHIP_TOP="$PWD/../../"

if [[ $checkpoint == "build-verilator" ]]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Inference
    rtl=generated-src-inference

    rm -rf $checkpoints_dir/$build
    rm -rf $checkpoints_dir/$rtl
    rm -rf $checkpoints_dir/baremetalc-integer

    cp -r $CHIP_TOP/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig $checkpoints_dir/$build
    cp -r $CHIP_TOP/sims/verilator/generated-src $checkpoints_dir/$rtl
    cp -r $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build $checkpoints_dir/baremetalc-integer
elif [[ $checkpoint == "build-midas" ]]; then
    build=$checkpoints_dir/DDR3FCFS-MIDAS
    rtl=generated-src-inference
    target=$CHIP_TOP/sims/firesim/sim/generated-src/f1/FireSim-DDR3FCFS_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig-BaseF1Config

    rm -rf $build
    rm -rf $checkpoints_dir/$rtl
    rm -rf $checkpoints_dir/baremetalc-integer

    cp -r $target $build
    cp -r $CHIP_TOP/sims/verilator/generated-src $checkpoints_dir/$rtl
    cp -r $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build $checkpoints_dir/baremetalc-integer
elif [[ $checkpoint == "build-complex" ]]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Complex
    rtl=generated-src-complex

    rm -rf $checkpoints_dir/$build
    rm -rf $checkpoints_dir/$rtl
    rm -rf $checkpoints_dir/baremetalc-complex

    cp -r $CHIP_TOP/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig $checkpoints_dir/$build
    cp -r $CHIP_TOP/sims/verilator/generated-src $checkpoints_dir/$rtl

    cp -r $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build $checkpoints_dir/baremetalc-complex
elif [[ $checkpoint == "build-onnx-inference" ]]; then
    cp $CHIP_TOP/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_runner/ort_test $checkpoints_dir/ort_test

    cd $checkpoints_dir/
    wget https://github.com/ucb-bar/onnxruntime-riscv/releases/download/v0.01/resnet50_opt_quant.onnx
elif [[ $checkpoint == "build-onnx-training" ]]; then
    cp $CHIP_TOP/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_trainer/resnet_train $checkpoints_dir/resnet_train

    cd $checkpoints_dir/
    rm -rf resnet50-training
    mkdir resnet50-training
    cd resnet50-training
    wget https://github.com/ucb-bar/onnxruntime-riscv/releases/download/v0.01/resnet50-training.tar.gz
    tar -xzvf resnet50-training.tar.gz && rm resnet50-training.tar.gz
else
    echo Unknown checkpoint: $checkpoint
fi

echo Saved checkpoint $checkpoint

