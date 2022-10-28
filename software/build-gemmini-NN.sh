#!/bin/bash

# This script will run on the host from the workload directory
# (e.g. workloads/example-fed) every time the workload is built.
# It is recommended to call into something like a makefile because
# this script may be called multiple times.
echo "Building gemmini-rocc-tests benchmark"
RDIR=$(pwd)
FSIMSW=/home/centos/firesim/sw/firesim-software/
cd gemmini-rocc-tests
./build.sh
cp -r build/* ../overlay/root/
cd $FSIMSW
rm images/gemmini-tests-NN*
./marshal -v --workdir $RDIR build gemmini-tests-NN.json
./marshal -v --workdir $RDIR install gemmini-tests-NN.json
cd $RDIR

