#!/usr/bin/env bash
echo "Test planaria_mp8b"
/root/imagenet/planaria_mp8b-linux
echo "Test priority_mp8_staticb"
/root/imagenet/priority_mp8_staticb-linux
echo "Test priority_mp8_dynamicb"
/root/imagenet/priority_mp8_dynamicb-linux
#echo "Test priority_mp8_staticb"
#/root/imagenet/priority_mp8_staticb-linux
#echo "Test prema_mp8"
#/root/imagenet/prema_mp8-linux

poweroff -f
