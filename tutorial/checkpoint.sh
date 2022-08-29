#!/bin/bash

checkpoint="$1"
checkpoints_dir="$2"

if [ "$checkpoints_dir" == "" ]; then
    checkpoints_dir="/root/builds/"
fi

CHIP_TOP="$PWD/../../"

if [[ $checkpoint == "build-verilator" ]]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Inference
    rtl=generated-src-inference

    rm -rf $CHIP_TOP/sims/verilator/generated-src
    rm -rf $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build/

    cp $checkpoints_dir/$build $CHIP_TOP/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r $checkpoints_dir/$rtl $CHIP_TOP/sims/verilator/generated-src
    cp -r $checkpoints_dir/baremetalc-integer $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build
elif [[ $checkpoint == "build-complex" ]]; then
    build=simulator-chipyard-CustomGemminiSoCConfig-Complex
    rtl=generated-src-complex

    rm -rf $CHIP_TOP/sims/verilator/generated-src
    rm -rf $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build/

    cp $checkpoints_dir/$build $CHIP_TOP/sims/verilator/simulator-chipyard-CustomGemminiSoCConfig
    cp -r $checkpoints_dir/$rtl $CHIP_TOP/sims/verilator/generated-src

    cp -r $checkpoints_dir/baremetalc-complex $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build
elif [[ $checkpoint == "build-midas" ]]; then
    build=$checkpoints_dir/DDR3FCFS-MIDAS
    target=$CHIP_TOP/sims/firesim/sim/generated-src/f1/FireSim-DDR3FCFS_WithDefaultFireSimBridges_WithFireSimConfigTweaks_chipyard.CustomGemminiSoCConfig-BaseF1Config

    rm -rf $target
    rm -rf $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build/
    cp -r $build $target
    cp -r $checkpoints_dir/$rtl $CHIP_TOP/sims/verilator/generated-src
    cp -r $checkpoints_dir/baremetalc-integer $CHIP_TOP/generators/gemmini/software/gemmini-rocc-tests/build
elif [[ $checkpoint == "build-onnx-inference" ]]; then
    cp $checkpoints_dir/ort_test $checkpoints_dir/resnet50_opt_quant.onnx $CHIP_TOP/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_runner/

    cd $CHIP_TOP/generators/gemmini/
    cp tutorial/gemmini_params_int.h ../../toolchains/esp-tools/riscv-isa-sim/gemmini/gemmini_params.h
    cd ../../toolchains/esp-tools/riscv-isa-sim/build
    make && make install
elif [[ $checkpoint == "build-onnx-training" ]]; then
    cp $checkpoints_dir/resnet_train $CHIP_TOP/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_trainer/
    cp -r $checkpoints_dir/resnet50-training/batch_out.txt $checkpoints_dir/resnet50-training/imagenet2012_cropped $checkpoints_dir/resnet50-training/resnet50.onnx $CHIP_TOP/generators/gemmini/software/onnxruntime-riscv/systolic_runner/imagenet_trainer/

    cd $CHIP_TOP/generators/gemmini/
    cp tutorial/gemmini_params_fp.h ../../toolchains/esp-tools/riscv-isa-sim/gemmini/gemmini_params.h
    cd ../../toolchains/esp-tools/riscv-isa-sim/build
    make && make install
elif [[ $checkpoint == "data-collection" ]]; then
    rm -rf data-collection-output-configs/
    cp -r tutorial/data-collection/* .
    mv clean.sh software/gemmini-rocc-tests/gemmini-data-collection/
else
    echo Unknown checkpoint
    echo Supported checkpoints:
    echo "	build-verilator: 	verilator build wth baseline inference gemmini config"
    echo "	build-complex:   	verilator build wth complex number gemmini config"
    echo "	build-midas:   	 	MIDAS build wth basline inference config"
    echo "	build-onnx-inference:	int8 gemmini spike and inference onnxruntime binary"
    echo "	build-onnx-training:	fp32 gemmini spike and training onnxruntime binary"
    echo "	data-collection:	parallelized data collection results"
    exit 1
fi

echo Loaded checkpoint $checkpoint

