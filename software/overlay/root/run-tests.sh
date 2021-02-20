#!/usr/bin/env bash

cat /proc/cpuinfo

echo "*****************TEST RESULTS*************"
echo "=========tiled_matmul========="
echo "=========tiled_matmul_os-linux========="
/root/bareMetalC/tiled_matmul_os-linux
echo "=========tiled_matmul_ws-linux========="
/root/bareMetalC/tiled_matmul_ws-linux

echo "========mobilenet========="
echo "========mobilenet OS========="
/root/imagenet/mobilenet-linux os matmul
echo "========mobilenet WS========="
/root/imagenet/mobilenet-linux ws matmul
echo "========mobilenet conv WS========="
/root/imagenet/mobilenet-linux ws conv

echo "========ResNet50========="
echo "========ResNet50 OS========="
/root/imagenet/resnet50-linux os matmul
echo "========ResNet50 WS========="
/root/imagenet/resnet50-linux ws matmul
echo "========ResNet50 conv WS========="
/root/imagenet/resnet50-linux ws conv

echo "========MLP 1========="
echo "========MLP 1 OS========="
/root/mlps/mlp1-linux os
echo "========MLP 1 WS========="
/root/mlps/mlp1-linux ws
echo "========MLP 2========="
echo "========MLP 2 OS========="
/root/mlps/mlp2-linux os
echo "========MLP 2 WS========="
/root/mlps/mlp2-linux ws
echo "========MLP 3========="
echo "========MLP 3 OS========="
/root/mlps/mlp3-linux os
echo "========MLP 3 WS========="
/root/mlps/mlp3-linux ws
echo "========MLP 4========="
echo "========MLP 4 OS========="
/root/mlps/mlp4-linux os
echo "========MLP 4 WS========="
/root/mlps/mlp4-linux ws

poweroff -f
