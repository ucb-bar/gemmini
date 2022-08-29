#!/bin/bash

mkdir -p data-collection-output
./scripts/run-verilator.sh conv_first_layer_1_4 > data-collection-output/conv_first_layer_1_4-verilator.txt &
./scripts/run-verilator.sh conv_first_layer_4_64 > data-collection-output/conv_first_layer_4_64-verilator.txt &
wait
