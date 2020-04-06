cd /

mkdir output
touch output/cpu_trace.json
touch output/os_trace.json
touch output/ws_trace.json

./ort_test -t trace -d 0 -m model_quantized.onnx -i images/dog.jpg -p caffe -x 0
mv *.json output/cpu_trace.json


./ort_test -t trace -d 0 -m model_quantized.onnx -i images/dog.jpg -p caffe -x 1
mv *.json output/os_trace.json


./ort_test -t trace -d 0 -m model_quantized.onnx -i images/dog.jpg -p caffe -x 2
mv *.json output/ws_trace.json

poweroff
