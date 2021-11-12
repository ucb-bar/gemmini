#!/bin/bash

if [ ! -d configs ]; then
    mkdir configs/
fi

if [ ! -d generated-src ]; then
    mkdir generated-src/
fi

if [ ! -f configs/GemminiDefaultConfigs.scala ]; then
    ln -s $PWD/src/main/scala/gemmini/Configs.scala configs/GemminiDefaultConfigs.scala
fi

if [ ! -f configs/GemminiCustomConfigs.scala ]; then
    ln -s $PWD/src/main/scala/gemmini/CustomConfigs.scala configs/GemminiCustomConfigs.scala
fi

if [ ! -f configs/CPUConfigs.scala ]; then
    sed '1,1d; $d' $PWD/src/main/scala/gemmini/CustomCPUConfigs.scala > ../chipyard/src/main/scala/config/GemminiCPUConfigs.scala
    ln -s $PWD/../chipyard/src/main/scala/config/GemminiCPUConfigs.scala configs/CPUConfigs.scala
fi

if [ ! -f configs/SoCConfigs.scala ]; then
    sed '1,1d; $d' $PWD/src/main/scala/gemmini/CustomSoCConfigs.scala > ../chipyard/src/main/scala/config/GemminiSoCConfigs.scala
    ln -s $PWD/../chipyard/src/main/scala/config/GemminiSoCConfigs.scala configs/SoCConfigs.scala
fi

if [ ! -f generated-src/verilator ] && [ ! -d generated-src/verilator ]; then
    ln -s $PWD/../../sims/verilator/generated-src/ generated-src/verilator 2>/dev/null
fi

if [ ! -f generated-src/vcs ] && [ ! -d generated-src/vcs ]; then
    ln -s $PWD/../../sims/vcs/generated-src/ generated-src/vcs 2>/dev/null
fi

