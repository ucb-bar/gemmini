#!/bin/bash

export PATH="$LOCAL_CONDA/bin:$PATH"
conda init
source ~/.bashrc
conda activate base
if ! { conda env list | grep 'chipyard'; } >/dev/null 2>&1; then
    conda create -n chipyard
    conda activate chipyard
    conda install -c conda-forge conda-lock
fi
conda activate chipyard

