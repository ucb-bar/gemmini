#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

mkdir -p $JAVA_TMP_DIR

git clone --progress --verbose https://github.com/ucb-bar/chipyard.git $LOCAL_CHIPYARD_DIR
cd $LOCAL_CHIPYARD_DIR

git fetch
git checkout $CHIPYARD_BRANCH

git submodule update --init generators/gemmini
git -C generators/gemmini fetch origin
git -C generators/gemmini checkout $GEMMINI_SUBMODULE_BRANCH

# Radiance generator is needed by RadianceGemminiOnlyConfig; pin it to main
# so the verilator elaborate picks up the matching radiance side of MX work.
git submodule update --init generators/radiance
git -C generators/radiance fetch origin
git -C generators/radiance checkout $RADIANCE_SUBMODULE_BRANCH

export MAKEFLAGS="-j32"
./build-setup.sh riscv-tools -s 6 -s 7 -s 8 -s 9 -v

source env.sh

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git config --global --add safe.directory '*'

cd $LOCAL_CHECKOUT_DIR

git submodule update --init --recursive mxgen
git submodule update --init --recursive software/gemmini-rocc-tests
rm -rf $LOCAL_CHIPYARD_DIR/generators/gemmini/* $LOCAL_CHIPYARD_DIR/generators/gemmini/.git*
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHECKOUT_DIR/.git* $LOCAL_CHIPYARD_DIR/generators/gemmini/
