#!/bin/bash

# Array of binaries to run in parallel
binaries=(
    "mvin_mvout"
    "mvin_mvout_zeros"
    "mvin_mvout_block_stride"
    "mvin_mvout_acc_zero_stride"
    "mvin_mvout_stride"
    "mvin_mvout_acc"
    "mvin_mvout_acc_full"
    "mvin_mvout_acc_stride"
    "mvin_mvout_acc_full_stride"
    "mvin_scale"
    "matmul_os"
    "matmul_ws"
    "matmul"
    "raw_hazard"
    "aligned"
    "padded"
    "conv"
    "conv_stride"
    "conv_rect"
    "conv_rect_pool"
    "conv_with_pool"
    "conv_with_rot180"
    "conv_with_kernel_dilation"
    "conv_with_input_dilation"
    "conv_with_input_dilation_and_rot180"
    "conv_with_input_dilation_and_neg_padding"
    "conv_trans_output_1203"
    "conv_trans_weight_1203"
    "conv_trans_weight_0132"
    "conv_trans_input_3120"
    "conv_trans_input_3120_with_kernel_dilation"
    "conv_first_layer"
    "conv_dw"
    "conv_perf"
    "conv_dw_perf"
    "tiled_matmul_os"
    "tiled_matmul_ws"
    "tiled_matmul_ws_At"
    "tiled_matmul_ws_Bt"
    "tiled_matmul_ws_full_C"
    "tiled_matmul_ws_low_D"
    "tiled_matmul_ws_igelu"
    "tiled_matmul_ws_layernorm"
    "tiled_matmul_ws_softmax"
    "tiled_matmul_ws_perf"
    "tiled_matmul_cpu"
    "tiled_matmul_option"
    "transpose"
    "matrix_add"
    "resadd"
    "resadd_stride"
    "global_average"
    "gemmini_counter"
    "identity"
    "template"
    "lut_template"
)

# Function to run each binary in parallel
run_binary() {
    binary="$1"
    ./scripts/run-vcs_1.sh "$binary" &
    pid=$!
    echo "Running $binary (PID: $pid)"
}

# Iterate through the array and run each binary in parallel
for binary in "${binaries[@]}"; do
    run_binary "$binary"
done

# Wait for all background jobs to finish
wait

echo "All binaries have been launched."
