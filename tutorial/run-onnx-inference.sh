#!/bin/bash

cd ./software/onnxruntime-riscv/
cd ./systolic_runner/imagenet_runner/

spike --extension=gemmini pk ort_test -m resnet50_opt_quant.onnx -i images/dog.jpg  -p caffe2 -x 2 -O 99

