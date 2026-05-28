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
LOCAL_SIM_DIR=$LOCAL_CHIPYARD_DIR/sims/vcs

CICONFIG=chipyard.config.WithNoDebug_GemminiRocketConfig

# Branches used by install-gemmini.sh. The chipyard branch needs the
# RadianceGemminiOnlyConfig (graphics) and the gemmini submodule pinned to
# gemmini-mx-cleanup so chipyard's build-setup picks up the same RoCC funct
# layout the spike libgemmini and the rocc tests expect.
CHIPYARD_BRANCH=${CHIPYARD_BRANCH:-graphics}
GEMMINI_SUBMODULE_BRANCH=${GEMMINI_SUBMODULE_BRANCH:-gemmini-mx-cleanup}
RADIANCE_SUBMODULE_BRANCH=${RADIANCE_SUBMODULE_BRANCH:-main}

# RTL config used for the MX (FP4/FP6/FP8) functional-equivalence job.
MX_CICONFIG=RadianceGemminiOnlyConfig

# The 13 ported MX tiled-matmul tests (passing on spike; expected to pass on
# RTL). matmul_tiled_fp4_64x64_DRAMMvout is intentionally omitted: it uses the
# accumulator -> DRAM mvout path that the spike kernel does not model.
MX_TESTS=(
  matmul_tiled_fp8_64x64
  matmul_tiled_fp8_128x128
  matmul_tiled_fp8_128x128x256
  matmul_tiled_fp8_128x128_requant
  matmul_tiled_fp4_64x64
  matmul_tiled_fp4_64x64_requant
  matmul_tiled_fp4_128x128
  matmul_tiled_fp4_128x128_requant
  matmul_tiled_fp4_128x128x512
  matmul_tiled_fp4_128x128x512_requant
  matmul_tiled_fp6_128x128
  matmul_tiled_fp6_128x128x512
  matmul_tiled_fp6_128x128x512_requant
)
