cd /

mkdir -p output/

touch /output/googlenet_cpu_trace.json
touch /output/googlenet_os_trace.json
touch /output/googlenet_ws_trace.json
touch /output/googlenet_cpu_out.txt
touch /output/googlenet_os_out.txt
touch /output/googlenet_ws_out.txt
touch /output/googlenet_cpu_trace_nhwc.json
touch /output/googlenet_os_trace_nhwc.json
touch /output/googlenet_ws_trace_nhwc.json
touch /output/googlenet_cpu_nhwc_out.txt
touch /output/googlenet_os_nhwc_out.txt
touch /output/googlenet_ws_nhwc_out.txt

touch /output/mobilenet_cpu_trace.json
touch /output/mobilenet_os_trace.json
touch /output/mobilenet_ws_trace.json
touch /output/mobilenet_cpu_out.txt
touch /output/mobilenet_os_out.txt
touch /output/mobilenet_ws_out.txt
touch /output/mobilenet_cpu_trace_nhwc.json
touch /output/mobilenet_os_trace_nhwc.json
touch /output/mobilenet_ws_trace_nhwc.json
touch /output/mobilenet_cpu_nhwc_out.txt
touch /output/mobilenet_os_nhwc_out.txt
touch /output/mobilenet_ws_nhwc_out.txt

touch /output/mobilenet_optimized_cpu_trace.json
touch /output/mobilenet_optimized_os_trace.json
touch /output/mobilenet_optimized_ws_trace.json
touch /output/mobilenet_optimized_cpu_out.txt
touch /output/mobilenet_optimized_os_out.txt
touch /output/mobilenet_optimized_ws_out.txt
touch /output/mobilenet_optimized_cpu_trace_nhwc.json
touch /output/mobilenet_optimized_os_trace_nhwc.json
touch /output/mobilenet_optimized_ws_trace_nhwc.json
touch /output/mobilenet_optimized_cpu_nhwc_out.txt
touch /output/mobilenet_optimized_os_nhwc_out.txt
touch /output/mobilenet_optimized_ws_nhwc_out.txt

touch /output/resnet50_cpu_trace.json
touch /output/resnet50_os_trace.json
touch /output/resnet50_ws_trace.json
touch /output/resnet50_cpu_out.txt
touch /output/resnet50_os_out.txt
touch /output/resnet50_ws_out.txt
touch /output/resnet50_cpu_trace_nhwc.json
touch /output/resnet50_os_trace_nhwc.json
touch /output/resnet50_ws_trace_nhwc.json
touch /output/resnet50_cpu_nhwc_out.txt
touch /output/resnet50_os_nhwc_out.txt
touch /output/resnet50_ws_nhwc_out.txt

# --- RESNET50 ---

echo "Resnet50"

./ort_test -t trace -d 0 -m resnet50_quantized.onnx -i images/dog.jpg -p mxnet -x 0  2>&1 | tee output/resnet50_cpu_out.txt
mv *.json output/resnet50_cpu_trace.json


./ort_test -t trace -d 0 -m resnet50_quantized.onnx -i images/dog.jpg -p mxnet -x 1  2>&1 | tee output/resnet50_os_out.txt
mv *.json output/resnet50_os_trace.json


./ort_test -t trace -d 0 -m resnet50_quantized.onnx -i images/dog.jpg -p mxnet -x 2  2>&1 | tee output/resnet50_ws_out.txt
mv *.json output/resnet50_ws_trace.json


./ort_test -t trace -d 0 -m resnet50_quantized.onnx -i images/dog.jpg -p mxnet -x 0 -O 99  2>&1 | tee output/resnet50_cpu_nhwc_out.txt
mv *.json output/resnet50_cpu_trace_nhwc.json


./ort_test -t trace -d 0 -m resnet50_quantized.onnx -i images/dog.jpg -p mxnet -x 1 -O 99  2>&1 | tee output/resnet50_os_nhwc_out.txt
mv *.json output/resnet50_os_trace_nhwc.json


./ort_test -t trace -d 0 -m resnet50_quantized.onnx -i images/dog.jpg -p mxnet -x 2 -O 99  2>&1 | tee output/resnet50_ws_nhwc_out.txt
mv *.json output/resnet50_ws_trace_nhwc.json

# --- GOOGLENET ---

echo "Googlenet"

./ort_test -t trace -d 0 -m googlenet_quantized.onnx -i images/dog.jpg -p caffe2 -x 0 2>&1 | tee output/googlenet_cpu_out.txt
mv *.json output/googlenet_cpu_trace.json


./ort_test -t trace -d 0 -m googlenet_quantized.onnx -i images/dog.jpg -p caffe2 -x 1  2>&1 | tee output/googlenet_os_out.txt
mv *.json output/googlenet_os_trace.json


./ort_test -t trace -d 0 -m googlenet_quantized.onnx -i images/dog.jpg -p caffe2 -x 2 2>&1 | tee output/googlenet_ws_out.txt
mv *.json output/googlenet_ws_trace.json


./ort_test -t trace -d 0 -m googlenet_quantized.onnx -i images/dog.jpg -p caffe2 -x 0 -O 99 2>&1 | tee output/googlenet_cpu_nhwc_out.txt
mv *.json output/googlenet_cpu_trace_nhwc.json


./ort_test -t trace -d 0 -m googlenet_quantized.onnx -i images/dog.jpg -p caffe2 -x 1 -O 99  2>&1 | tee output/googlenet_os_nhwc_out.txt
mv *.json output/googlenet_os_trace_nhwc.json


./ort_test -t trace -d 0 -m googlenet_quantized.onnx -i images/dog.jpg -p caffe2 -x 2 -O 99  2>&1 | tee output/googlenet_ws_nhwc_out.txt
mv *.json output/googlenet_ws_trace_nhwc.json

# --- MOBILENET ---

echo "Mobilenet"

./ort_test -t trace -d 0 -m mobilenet_quantized.onnx -i images/dog.jpg -p caffe -x 0  2>&1 | tee output/mobilenet_cpu_out.txt
mv *.json output/mobilenet_cpu_trace.json


./ort_test -t trace -d 0 -m mobilenet_quantized.onnx -i images/dog.jpg -p caffe -x 1  2>&1 | tee output/mobilenet_os_out.txt
mv *.json output/mobilenet_os_trace.json


./ort_test -t trace -d 0 -m mobilenet_quantized.onnx -i images/dog.jpg -p caffe -x 2  2>&1 | tee output/mobilenet_ws_out.txt
mv *.json output/mobilenet_ws_trace.json


./ort_test -t trace -d 0 -m mobilenet_quantized.onnx -i images/dog.jpg -p caffe -x 0 -O 99  2>&1 | tee output/mobilenet_cpu_nhwc_out.txt
mv *.json output/mobilenet_cpu_trace_nhwc.json


./ort_test -t trace -d 0 -m mobilenet_quantized.onnx -i images/dog.jpg -p caffe -x 1 -O 99  2>&1 | tee output/mobilenet_os_nhwc_out.txt
mv *.json output/mobilenet_os_trace_nhwc.json


./ort_test -t trace -d 0 -m mobilenet_quantized.onnx -i images/dog.jpg -p caffe -x 2 -O 99  2>&1 | tee output/mobilenet_ws_nhwc_out.txt
mv *.json output/mobilenet_ws_trace_nhwc.json

# --- MOBILENET OPTIMIZED ---

echo "Mobilenet Optimized"

./ort_test -t trace -d 0 -m mobilenet_quantized_optimized.onnx -i images/dog.jpg -p caffe -x 0  2>&1 | tee output/mobilenet_optimized_cpu_out.txt
mv *.json output/mobilenet_optimized_cpu_trace.json


./ort_test -t trace -d 0 -m mobilenet_quantized_optimized.onnx -i images/dog.jpg -p caffe -x 1  2>&1 | tee output/mobilenet_optimized_os_out.txt
mv *.json output/mobilenet_optimized_os_trace.json


./ort_test -t trace -d 0 -m mobilenet_quantized_optimized.onnx -i images/dog.jpg -p caffe -x 2  2>&1 | tee output/mobilenet_optimized_ws_out.txt
mv *.json output/mobilenet_optimized_ws_trace.json


./ort_test -t trace -d 0 -m mobilenet_quantized_optimized.onnx -i images/dog.jpg -p caffe -x 0 -O 99  2>&1 | tee output/mobilenet_optimized_cpu_nhwc_out.txt
mv *.json output/mobilenet_optimized_cpu_trace_nhwc.json


./ort_test -t trace -d 0 -m mobilenet_quantized_optimized.onnx -i images/dog.jpg -p caffe -x 1 -O 99  2>&1 | tee output/mobilenet_optimized_os_nhwc_out.txt
mv *.json output/mobilenet_optimized_os_trace_nhwc.json


./ort_test -t trace -d 0 -m mobilenet_quantized_optimized.onnx -i images/dog.jpg -p caffe -x 2 -O 99  2>&1 | tee output/mobilenet_optimized_ws_nhwc_out.txt
mv *.json output/mobilenet_optimized_ws_trace_nhwc.json

poweroff

