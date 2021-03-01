#!/bin/bash

# shared variables between the different services
#
# CircleCI set values:
#   $SERVER - points to the millennium build server
#   $AWS_SERVER - points to the aws manager instance
#   $CI_DIR - home directory on build server
#   $CI_AWS_DIR - home directory on aws


#############
# SHARED VARS
#############

# make parallelism
CI_MAKE_NPROC=4
LOCAL_MAKE_NPROC=$CI_MAKE_NPROC

# verilator version
VERILATOR_VERSION=v4.034

# local variables (aka within the docker container)
LOCAL_WORK_DIR=$HOME
LOCAL_CHECKOUT_DIR=$HOME/project
LOCAL_RISCV_DIR=$HOME/riscv-tools-install
LOCAL_ESP_DIR=$HOME/esp-tools-install
LOCAL_CHIPYARD_DIR=$HOME/chipyard
LOCAL_SIM_DIR=$LOCAL_CHIPYARD_DIR/sims/verilator
LOCAL_VERILATOR_DIR=$HOME/verilator-install

# api url to do curls
API_URL=https://circleci.com/api/v2

