#!/bin/bash

#############
# SHARED VARS
#############

# make parallelism
CI_MAKE_NPROC=4
LOCAL_MAKE_NPROC=$CI_MAKE_NPROC

# local variables
LOCAL_CHECKOUT_DIR=$GITHUB_WORKSPACE

LOCAL_CHIPYARD_DIR=$REMOTE_WORK_DIR
LOCAL_SIM_DIR=$LOCAL_CHIPYARD_DIR/sims/verilator

CICONFIG=chipyard.config.WithNoDebug_GemminiRocketConfig
