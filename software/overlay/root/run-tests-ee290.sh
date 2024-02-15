#!/usr/bin/env bash

echo "*****************TEST RESULTS*************" > test_output.txt
echo "=========very_large_matmul========="
echo "=========very_large_matmul=========" >> test_output.txt
/root/ee290/very_large_matmul-linux >> test_output.txt
echo "=========CIFAR DNN========="
echo "=========CIFAR DNN=========" >> test_output.txt
/root/ee290/cifar_quant-linux >> test_output.txt
echo "========Multi-Level Perceptron 1========="
echo "========Multi-Level Perceptron 1=========" >> test_output.txt
/root/mlps/mlp1-linux >> test_output.txt
echo "========Multi-Level Perceptron 2========="
echo "========Multi-Level Perceptron 2=========" >> test_output.txt
/root/mlps/mlp2-linux >> test_output.txt
echo "========MobileNet========="
echo "========MobileNet=========" >> test_output.txt
/root/imagenet/mobilenet-linux >> test_output.txt
echo "========ResNet50========="
echo "========ResNet50=========" >> test_output.txt
/root/imagenet/resnet50-linux >> test_output.txt
cat test_output.txt
poweroff -f