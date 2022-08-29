#!/bin/bash

mkdir -p data-collection-output
./scripts/run-vcs.sh conv_first_layer_1_4 > data-collection-output/conv_first_layer_1_4-vcs.txt &
./scripts/run-vcs.sh conv_first_layer_4_64 > data-collection-output/conv_first_layer_4_64-vcs.txt &
wait
