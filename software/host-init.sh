#!/bin/bash

# This script will run on the host from the workload directory
# (e.g. workloads/example-fed) every time the workload is built.
# It is recommended to call into something like a makefile because
# this script may be called multiple times.
echo "Building gemmini-rocc-tests benchmark"
cd gemmini-rocc-tests
autoconf
mkdir -p build && cd build
../configure
make TARGET=riscv64-unknown-linux-gnu- -j
cp -r ./* ../../overlay/root/
