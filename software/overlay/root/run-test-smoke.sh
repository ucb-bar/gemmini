#!/usr/bin/env bash

echo "*****************TEST RESULTS*************" > test_output.txt

echo "========mobilenet========="
/root/imagenet/mobilenet-linux >> test_output.txt

cat test_output.txt
poweroff -f
