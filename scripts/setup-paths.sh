#!/bin/bash

if [ ! -d configs ]; then
    mkdir configs/
fi

if [ ! -f configs/GemminiDefaultConfigs.scala ]; then
    ln -s $PWD/src/main/scala/gemmini/Configs.scala configs/GemminiDefaultConfigs.scala
fi

if [ ! -f configs/GemminiCustomConfigs.scala ]; then
    ln -s $PWD/src/main/scala/gemmini/CustomConfigs.scala configs/GemminiCustomConfigs.scala
fi

if [ ! -f configs/CPUConfigs.scala ]; then
    sed "1,1d; $d" $PWD/src/main/scala/gemmini/CustomCPUConfigs.scala > ../chipyard/src/main/scala/config/GemminiCPUConfigs.scala
    ln -s $PWD/../chipyard/src/main/scala/config/GemminiCPUConfigs.scala configs/CPUConfigs.scala
fi

# if [ ! -f configs/L2Configs.scala ]; then
# fi

