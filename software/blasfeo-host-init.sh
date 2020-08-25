#!/bin/bash

set -e

# This script will run on the host from the workload directory
# (e.g. workloads/example-fed) every time the workload is built.
# It is recommended to call into something like a makefile because
# this script may be called multiple times.
echo "Building blasfeo"
cd blasfeo
# clean up state
rm -rf ../overlay/root/blasfeo
make clean
# build library
make static_library -j8 TARGET=RISCV
# build tests
echo "Building BLASFEO Tests"
make benchmarks TARGET=RISCV GHZ_MAX=1
make examples
make tests_one
# copy to linux overlay
mkdir -p ../overlay/root/blasfeo
echo "Copying BLASFEO and tests to overlay"
echo '#!/usr/bin/env bash' > ../overlay/root/blasfeo/make-check.sh
echo `` >> ../overlay/root/blasfeo/make-check.sh
echo 'echo Running BLASFEO example' >> ../overlay/root/blasfeo/make-check.sh
echo './examples/example.out' >> ../overlay/root/blasfeo/make-check.sh
echo 'echo Running BLASFEO tests' >> ../overlay/root/blasfeo/make-check.sh
echo './tests/build/HIGH_PERFORMANCE/RISCV/test_s_blasfeo_api.o.out' >> ../overlay/root/blasfeo/make-check.sh
echo 'echo Running BLASFEO benchmarks' >> ../overlay/root/blasfeo/make-check.sh
echo './benchmarks/build/HIGH_PERFORMANCE/RISCV/benchmark_s_blasfeo_api.o.out' >> ../overlay/root/blasfeo/make-check.sh
chmod 775 ../overlay/root/blasfeo/make-check.sh
cp -r lib/ ../overlay/root/blasfeo/lib
cp -r include/ ../overlay/root/blasfeo/include
cp -r benchmarks/ ../overlay/root/blasfeo/benchmarks
cp -r examples/ ../overlay/root/blasfeo/examples
cp -r tests/ ../overlay/root/blasfeo/tests
