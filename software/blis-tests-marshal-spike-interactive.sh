#!/bin/bash

# This script will rebuild BLIS, and then
# build the appropriate Linux Image for FireSim execution using
# FireMarshal and Spike
echo "Build and Test BLIS Tests Workload"

RDIR=$(pwd)
MARSHALDIR=$(pwd)/../../../software/firemarshal
rm -rf overlay/root/blis/
cd $MARSHALDIR
rm images/blis-tests*
./marshal -v --no-disk --workdir /scratch/alonamid/chipyard-gemmini-blas/generators/gemmini/software/ build blis-tests-interactive.json
./marshal -v --no-disk --workdir /scratch/alonamid/chipyard-gemmini-blas/generators/gemmini/software/ launch --spike blis-tests-interactive.json
cd $RDIR
