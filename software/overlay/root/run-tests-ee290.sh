#!/usr/bin/env bash

#cat /proc/cpuinfo

echo "*****************TEST RESULTS*************"
#echo "=========very_large_matmul========="
#/root/ee290/very_large_matmul-linux
#echo "=========CIFAR DNN========="
#/root/ee290/cifar_quant-linux
echo "========Multi-Level Perceptron 1========="
/root/mlps/mlp1-linux
echo "========Multi-Level Perceptron 2========="
/root/mlps/mlp2-linux
echo "========MobileNet========="
/root/imagenet/mobilenet-linux
#echo "========ResNet50========="
#/root/imagenet/resnet50-linux

poweroff -f
