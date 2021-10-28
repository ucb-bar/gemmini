#!/bin/bash

mkdir configs/ 2>/dev/null
ln -s $PWD/src/main/scala/gemmini/GemminiCustomConfigs.scala configs/GemminiCustomConfigs.scala

