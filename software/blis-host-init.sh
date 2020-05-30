#!/bin/bash

# This script will run on the host from the workload directory
# (e.g. workloads/example-fed) every time the workload is built.
# It is recommended to call into something like a makefile because
# this script may be called multiple times.
echo "Building Blis"
cd blis
# configure to statically build for riscv64
#./configure CC=riscv64-unknown-linux-gnu-gcc CFLAGS=-static LDFLAGS=-static --disable-threading --disable-pba-pools --disable-sba-pools --disable-shared riscv64
./configure CC=riscv64-unknown-linux-gnu-gcc CFLAGS=-static LDFLAGS=-static --disable-threading --disable-pba-pools --disable-sba-pools --disable-shared gemmini
# build library
make clean
make -j8
# build tests
echo "Building Blis Tests"
make testsuite-bin
make blastest-bin
# copy to linux overlay
echo "Copying Blis and tests to overlay"
rm -rf ../overlay/root/blis/
mkdir -p ../overlay/root/blis
echo '#!/usr/bin/env bash' > ../overlay/root/blis/make-check.sh
echo `` >> ../overlay/root/blis/make-check.sh
make -n check >> ../overlay/root/blis/make-check.sh
chmod 775 ../overlay/root/blis/make-check.sh
cp test_libblis.x ../overlay/root/blis 
cp make-check.sh ../overlay/root/blis 
cp -r obj/ ../overlay/root/blis/obj
cp -r lib/ ../overlay/root/blis/lib
cp -r include/ ../overlay/root/blis/include
cp -r testsuite ../overlay/root/blis
cp -r blastest ../overlay/root/blis
