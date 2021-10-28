#!/bin/bash

if [ ! -d configs ]; then
    mkdir configs/
fi

if [ ! -f configs/GemminiCustomConfigs.scala ]; then
    ln -s $PWD/src/main/scala/gemmini/GemminiCustomConfigs.scala configs/GemminiCustomConfigs.scala
fi

