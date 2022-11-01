#!/usr/bin/env bash
#echo "Test planaria_mp8b"
#/root/imagenet/planaria_mp8b-linux
echo "Test priority_mp8_staticc_m"
/root/imagenet/priority_mp8_static-linux
echo "Test priority_mp8_dynamicc_m"
/root/imagenet/priority_mp8_dynamic-linux
echo "Test priority_mp8_staticc_h"
/root/imagenet/priority_mp8_static_h-linux
echo "Test priority_mp8_dynamicc_h"
/root/imagenet/priority_mp8_dynamic_h-linux
echo "Test priority_mp8_staticc_l"
/root/imagenet/priority_mp8_static_l-linux
echo "Test priority_mp8_dynamicc_l"
/root/imagenet/priority_mp8_dynamic_l-linux
poweroff -f
