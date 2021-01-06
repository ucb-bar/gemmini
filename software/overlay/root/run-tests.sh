#!/usr/bin/env bash

echo "*****************TEST RESULTS*************" > test_output.txt
echo "=========tiled_matmul========="
echo "=========tiled_matmul_os-linux=========" >> test_output.txt
/root/bareMetalC/tiled_matmul_os-linux >> test_output.txt
echo "=========tiled_matmul_ws-linux=========" >> test_output.txt
/root/bareMetalC/tiled_matmul_ws-linux >> test_output.txt

echo "========mobilenet========="
echo "========mobilenet OS=========" >> test_output.txt
/root/imagenet/mobilenet-linux os matmul >> test_output.txt
echo "========mobilenet WS=========" >> test_output.txt
/root/imagenet/mobilenet-linux ws matmul >> test_output.txt
echo "========mobilenet conv WS=========" >> test_output.txt
/root/imagenet/mobilenet-linux ws conv >> test_output.txt

echo "========ResNet50========="
echo "========ResNet50 OS=========" >> test_output.txt
/root/imagenet/resnet50-linux os matmul >> test_output.txt
echo "========ResNet50 WS=========" >> test_output.txt
/root/imagenet/resnet50-linux ws matmul >> test_output.txt
echo "========ResNet50 conv WS=========" >> test_output.txt
/root/imagenet/resnet50-linux ws conv >> test_output.txt

echo "========MLP 1========="
echo "========MLP 1 OS=========" >> test_output.txt
/root/mlps/mlp1-linux os >> test_output.txt
echo "========MLP 1 WS=========" >> test_output.txt
/root/mlps/mlp1-linux ws >> test_output.txt
echo "========MLP 2========="
echo "========MLP 2 OS=========" >> test_output.txt
/root/mlps/mlp2-linux os >> test_output.txt
echo "========MLP 2 WS=========" >> test_output.txt
/root/mlps/mlp2-linux ws >> test_output.txt
echo "========MLP 3========="
echo "========MLP 3 OS=========" >> test_output.txt
/root/mlps/mlp3-linux os >> test_output.txt
echo "========MLP 3 WS=========" >> test_output.txt
/root/mlps/mlp3-linux ws >> test_output.txt
echo "========MLP 4========="
echo "========MLP 4 OS=========" >> test_output.txt
/root/mlps/mlp4-linux os >> test_output.txt
echo "========MLP 4 WS=========" >> test_output.txt
/root/mlps/mlp4-linux ws >> test_output.txt

cat test_output.txt
poweroff -f
