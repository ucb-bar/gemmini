#!/bin/bash

set -ex

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

rm -rf $LOCAL_CHIPYARD_DIR
rm -rf $LOCAL_CONDA

