#!/bin/bash

mkdir -p data-collection-output
./scripts/run-midas.sh $1 conv_first_layer_1_4 > data-collection-output/conv_first_layer_1_4-midas.txt &
./scripts/run-midas.sh $1 conv_first_layer_4_64 > data-collection-output/conv_first_layer_4_64-midas.txt &
wait
