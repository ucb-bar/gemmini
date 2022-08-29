#!/bin/bash

mkdir -p data-collection-output
./scripts/run-spike.sh conv_first_layer_1_4 > data-collection-output/conv_first_layer_1_4-spike.txt &
./scripts/run-spike.sh conv_first_layer_4_64 > data-collection-output/conv_first_layer_4_64-spike.txt &
wait
