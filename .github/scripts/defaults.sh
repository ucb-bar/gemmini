#!/bin/bash

#############
# SHARED VARS
#############

# make parallelism
# BUILD_NPROC: threads for the RTL elaboration/compilation step (kept low to
# avoid Scala/Chisel/VCS parallel-build races that break the RadianceGemminiOnlyConfig build).
# MAKE_NPROC: threads used when running tests in parallel (tests are independent).
CI_MAKE_NPROC=4
LOCAL_MAKE_NPROC=$CI_MAKE_NPROC
CI_BUILD_NPROC=1
LOCAL_BUILD_NPROC=$CI_BUILD_NPROC

# local variables
LOCAL_CHECKOUT_DIR=$GITHUB_WORKSPACE

LOCAL_CHIPYARD_DIR=$REMOTE_WORK_DIR
LOCAL_SIM_DIR=$LOCAL_CHIPYARD_DIR/sims/vcs

export CARGO_HOME=$HOME/.cargo
export RUSTUP_HOME=$HOME/.rustup
export PATH=$HOME/.cargo/bin:$PATH

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

# The ported MX (FP4/FP6/FP8) tests (passing on spike; expected to pass on
# RTL). Each entry is the test binary's path relative to the build directory
# (i.e. <category>/<name>), so tests outside bareMetalC (e.g. the mlps MNIST
# end-to-end test) can be listed alongside the tiled-matmul kernels.
# matmul_tiled_fp4_64x64_DRAMMvout is intentionally omitted: it uses the
# accumulator -> DRAM mvout path that the spike kernel does not model.
MX_TESTS=(
  bareMetalC/matmul_tiled_fp8_128x128x256
  bareMetalC/matmul_tiled_fp8_64x64_requant
  bareMetalC/matmul_tiled_fp8_128x128_requant
  bareMetalC/matmul_tiled_fp8_64x96x64_requant
  bareMetalC/matmul_tiled_fp8_96x96x64_requant
  bareMetalC/matmul_tiled_fp4_64x64
  bareMetalC/matmul_tiled_fp4_64x64_requant
  bareMetalC/matmul_tiled_fp4_128x128
  bareMetalC/matmul_tiled_fp4_128x128_requant
  bareMetalC/matmul_tiled_fp4_128x128x512
  bareMetalC/matmul_tiled_fp4_128x128x512_requant
  bareMetalC/matmul_tiled_fp6_128x128
  bareMetalC/matmul_tiled_fp6_128x128x512
  bareMetalC/matmul_tiled_fp6_128x128x512_requant
  mlps/mnist_mxgemmini
)

# RTL config for the int8 (non-MX) GemminiRocketConfig sanity job. Built and
# tested just like the MX track, but exercising the legacy systolic-array path.
ROCKET_CICONFIG=GemminiRocketConfig

# The int8 sanity tests. Built by the regular ./build.sh (the binaries run on
# both spike and VCS, so no build_spike step is needed). Each entry is the test
# binary's path relative to the build directory (<category>/<name>).
ROCKET_TESTS=(
  bareMetalC/matmul_ws
  bareMetalC/matmul_os
  bareMetalC/mvin_mvout
  bareMetalC/mvin_mvout_spad
  bareMetalC/conv
)

# Tests the spike gemmini extension does not model, so they are run on RTL only.
ROCKET_SPIKE_SKIP=(
  bareMetalC/conv
)
