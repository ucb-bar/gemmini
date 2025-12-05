# Muon Integration Baremetal C Tests

Tests MxGemmini with Radiance Kernels: 

1. Clone [radiance-kernels](https://github.com/ucb-bar/radiance-kernels.git).
2. `cd soc/` and replace `main.c` with one of the tests in this directory. 
3. run `make` to build the binary. 
4. run on chipyard `sims/vcs`: `make run-binary-debug CONFIG=RadianceTapeoutSimConfig LOADMEM=1 BINARY=../../generators/radiance/radiance-kernels/soc/fused.elf`


Test example MxGemmini with a RocketConfig (Isolated, without Radiance cluster Integration):
1. in `sims/vcs` run `make CONFIG=TestMxGemminiRocketConfig`
2. `cd software/gemmini-rocc-tests`
3. `./build.sh`
4. then go back to `sims/vcs` and run: `make run-binary CONFIG=MxGemminiRocketConfig LOADMEM=1 BINARY=../../generators/gemmini/software/gemmini-rocc-tests/build/bareMetalC/matmul_ws-baremetal`, for example
