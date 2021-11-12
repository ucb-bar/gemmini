#!/bin/bash

cd /root/chipyard/generators/gemmini/software/onnxruntime-riscv/
rm -rf ./build/
./build.sh --parallel --enable_training --config=Debug --cmake_extra_defines onnxruntime_USE_SYSTOLIC=ON onnxruntime_SYSTOLIC_INT8=ON onnxruntime_SYSTOLIC_FP32=OFF
cd ./systolic_runner/imagenet_runner/
./build.sh --parallel --enable_training --config=Debug
