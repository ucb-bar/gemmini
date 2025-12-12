# Muon Integration Baremetal C Tests

Test MxGemmini with Radiance Kernels: 
1. Clone [radiance-kernels](https://github.com/ucb-bar/radiance-kernels.git).
2. `cd soc/` and replace `main.c` with one of the tests in this directory. 
3. run `make` to build the binary. 
4. run on chipyard `sims/vcs`: `make run-binary-debug CONFIG=RadianceTapeoutSimConfig LOADMEM=1 BINARY=../../generators/radiance/radiance-kernels/soc/fused.elf`


Test example MxGemmini with a RocketConfig (Isolated, without Radiance cluster Integration):
1. Go to `sims/vcs` on chipyard root and run `make CONFIG=TestMxGemminiRocketConfig`. This will build a config that replaces scaling factor memory to avoid having to write to it trough MMIO with the gpu. It assumes that a scale factor of 0.5 is preloaded.
2. To generate the test binary, `cd software/gemmini-rocc-tests`.
3. You can generate testing data using a pytorch based "golden model" to simulate quantization and low precision float arithmetic, and generate the test inputs/outputs:
    ```
    python golden_model.py   --input fp8:e4m3 --input-rounding zero --prod-mant-bits 7 --acc   bf16 --acc-rounding q_bf16_rne  --scaled-spec bf16 --scale-spec fpe8m0 --scale-exp -2 --M 16 --K 16 --N 16 --header-path include/matmul_data.h
    ```
4. In this directory, then run `./build.sh`
5. This binary can be run in `sims/vcs` with: `make run-binary CONFIG=MxGemminiRocketConfig LOADMEM=1 BINARY=../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/matmul_ws-baremetal`.
