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
git checkout $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)

export MAKEFLAGS="-j32"
./build-setup.sh riscv-tools -f -s 6 -s 7 -s 8 -s 9 -v

source env.sh

cd $LOCAL_CHECKOUT_DIR
chown -R $(whoami) .
git config --global --add safe.directory $LOCAL_CHECKOUT_DIR
git config --global --add safe.directory '*'

cd $LOCAL_CHECKOUT_DIR
git submodule update --init --recursive software/gemmini-rocc-tests
rm -rf $LOCAL_CHIPYARD_DIR/generators/gemmini/* $LOCAL_CHIPYARD_DIR/generators/gemmini/.git*
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHECKOUT_DIR/.git* $LOCAL_CHIPYARD_DIR/generators/gemmini/
