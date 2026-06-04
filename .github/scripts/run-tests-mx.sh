#!/bin/bash

# Run the ported MX (FP4/FP6/FP8) tiled-matmul tests on both spike and
# verilator. Each test self-checks against the Python golden header and
# prints "PASSED" or "FAILED"; we accept the test iff both simulators see
# PASSED. No byte-level diff is performed (verilator is slow).
#
# Tests are launched in parallel (up to LOCAL_MAKE_NPROC at a time) to
# reduce wall-clock time. Results are collected via a per-test temp file.

set -e
set -o pipefail

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

eval "$(conda shell.bash hook)"

cd $LOCAL_CHIPYARD_DIR
source env.sh

# Put Synopsys VCS (and its license server) on PATH for the sims/vcs run.
source /ecad/tools/vlsi.bashrc

TESTS_DIR=$LOCAL_CHIPYARD_DIR/generators/gemmini/software/gemmini-rocc-tests

RESULT_DIR=$(mktemp -d)
trap 'rm -rf "$RESULT_DIR"' EXIT

# Run a single test in a subshell; write PASS/FAIL/MISSING to a result file.
# Usage: run_test_bg <result_file> <log_file> <cmd...>
# The test is considered passing if its log contains "PASSED".
run_test_bg() {
  local result_file=$1 log_file=$2
  shift 2
  "$@" > "$log_file" 2>&1 || true
  if grep -q "PASSED" "$log_file"; then
    echo "PASS" > "$result_file"
  else
    echo "FAIL" > "$result_file"
  fi
}

# Throttle: keep at most LOCAL_MAKE_NPROC background jobs running at once.
throttle() {
  while [ "$(jobs -rp | wc -l)" -ge "$LOCAL_MAKE_NPROC" ]; do
    sleep 0.5
  done
}

echo "================================================================"
echo "  MX tests on spike (parallel, up to $LOCAL_MAKE_NPROC at a time)"
echo "================================================================"
for t in "${MX_TESTS[@]}"; do
  bin=$TESTS_DIR/build_spike/${t}-baremetal
  log=/tmp/mx-spike-${t//\//-}.log
  result=$RESULT_DIR/spike_${t//\//_}
  if [ ! -f "$bin" ]; then
    echo "MISSING" > "$result"
    continue
  fi
  throttle
  run_test_bg "$result" "$log" \
    spike --extension=gemmini "$bin" &
done

echo "================================================================"
echo "  MX tests on VCS (parallel, up to $LOCAL_MAKE_NPROC at a time)"
echo "================================================================"
cd $LOCAL_SIM_DIR
for t in "${MX_TESTS[@]}"; do
  bin=$TESTS_DIR/build/${t}-baremetal
  log=/tmp/mx-rtl-${t//\//-}.log
  result=$RESULT_DIR/rtl_${t//\//_}
  if [ ! -f "$bin" ]; then
    echo "MISSING" > "$result"
    continue
  fi
  throttle
  run_test_bg "$result" "$log" \
    make -C $LOCAL_SIM_DIR run-binary CONFIG=$MX_CICONFIG LOADMEM=1 BINARY="$bin" &
done

wait

echo "================================================================"
echo "  Summary"
echo "================================================================"
PASS=()
FAIL=()
for t in "${MX_TESTS[@]}"; do
  for sim in spike rtl; do
    result_file=$RESULT_DIR/${sim}_${t//\//_}
    result=$(cat "$result_file" 2>/dev/null || echo "MISSING")
    case "$result" in
      PASS)
        PASS+=("$sim:$t")
        ;;
      MISSING)
        FAIL+=("$sim:$t (binary missing)")
        ;;
      *)
        log=/tmp/mx-${sim}-${t//\//-}.log
        echo "--- $sim:$t FAILED (last 20 lines): ---"
        tail -n 20 "$log" || true
        FAIL+=("$sim:$t")
        ;;
    esac
  done
done

echo "Passed (${#PASS[@]}/$((2*${#MX_TESTS[@]}))):"
for p in "${PASS[@]}"; do echo "  PASS  $p"; done
echo "Failed (${#FAIL[@]}/$((2*${#MX_TESTS[@]}))):"
for f in "${FAIL[@]}"; do echo "  FAIL  $f"; done

exit ${#FAIL[@]}
