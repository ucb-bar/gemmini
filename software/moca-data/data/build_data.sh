#!/bin/bash

python3 ../make_csv_from_uartlog.py uartlog &&

python3 ../make_valid.py ../pre_compile
