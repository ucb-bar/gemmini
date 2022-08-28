#!/bin/bash

grep -Ir DEADBEEF src/
grep -Ir DEADBEEF /root/builds/

./scripts/setup-paths.sh
./scripts/build-spike.sh
./tutorial/checkpoint.sh build-verilator
./tutorial/checkpoint.sh build-midas
./tutorial/checkpoint.sh data-collection

cd software/gemmini-rocc-tests
./build.sh

