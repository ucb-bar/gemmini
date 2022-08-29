#!/bin/bash

rm -rf ../../../data-collection-output
rm ../../../data-collection-vcs.sh
rm ../../../data-collection-verilator.sh
rm ../../../data-collection-midas.sh
rm ../../../data-collection-spike.sh
cp og_baremetal_Makefile ../bareMetalC/Makefile
cd ..
./build.sh clean
cd gemmini-data-collection
rm ../bareMetalC/conv_first_layer_1_4.c
rm ../bareMetalC/conv_first_layer_4_64.c
