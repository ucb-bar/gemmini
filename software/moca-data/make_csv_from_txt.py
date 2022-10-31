import sys
import re
import csv

DIM = 16
app_name = ['resnet', 'alexnet', 'googlenet', 'squeezenet', 'kwsnet', 'yolonet', 'yololitenet']
#app_name = ['resnet', 'alexnet', 'kwsnet', 'fcnnet', 'squeezenet', 'googlenet', 'yolonet', 'yololitenet']
#app_name = ['resnet']

# for runtime prediction
# first assuming image remains in L2: ideal computation time + dram bandwidth overhead(B, C, D) + L2 bandwidth overhead if necessary
l2_bw = 8 # L2 banks
dram_bw = 1 # 16 bytes / cycles
#scaling_factor_dram = 0.7 # utilizable dram bw
cache_size = (2e6/DIM)
max_conv = 0

def conv_layer_runtime_predict(num_core, compute_ideal, l2_bw_util, dram_bw_util, total_mem, full_dram, a_reuse, b_reuse):
    core_compute_ideal = compute_ideal / num_core
    effective_l2_bw = min(l2_bw / num_core, 1) # ToDo: scale?
    # memory does not scale
    l2_mem = total_mem / num_core #(total_mem - full_dram) / num_core
    dram_cycle = full_dram / dram_bw
    l2_cycle = l2_mem / effective_l2_bw
    mem_ideal = max(l2_cycle, dram_cycle)# + min(l2_cycle, dram_cycle) * 0.2
    #if b_reuse == 1 and a_reuse > 1:
    #    mem_ideal += min(l2_cycle, dram_cycle) * 0.7
    #elif b_reuse == 1:
    #    mem_ideal += min(l2_cycle, dram_cycle) * 0.2
    prediction = max(mem_ideal, core_compute_ideal)# + min(mem_ideal, core_compute_ideal) * 0.4
    if b_reuse == 1 and a_reuse > 1:
        prediction += min(mem_ideal, core_compute_ideal) * 0.7
    elif b_reuse == 1:
        prediction += min(mem_ideal, core_compute_ideal) * 0.5
    elif (b_reuse + a_reuse) > 100:
        prediction += min(mem_ideal, core_compute_ideal)
    else:
        prediction += min(mem_ideal, core_compute_ideal) * 0.3
    # ToDo: add overhead?
    return (core_compute_ideal, mem_ideal, prediction)

def resadd_layer_runtime_predict(num_core, A_size, B_size, C_size):
    total_l2 = A_size + B_size + C_size
    from_dram = total_l2
    '''
    if A_size + B_size > cache_size:
        from_dram = total_l2
    else:
        from_dram = B_size + C_size
    '''
    effective_l2_bw = min(l2_bw / num_core, 1)
    l2_mem = total_l2 / num_core / effective_l2_bw
    prediction = from_dram / dram_bw + l2_mem
    return (from_dram, prediction)

def pool_layer_runtime_predict(num_core, A_size, C_size):
    effective_l2_bw = min(l2_bw / num_core, 1)
    total_mem = A_size + C_size # just treat everything from dram
    total_mem = total_mem / dram_bw + total_mem / effective_l2_bw / num_core
    pool_op = C_size * 3 * 3 / num_core
    prediction = pool_op + total_mem
    return prediction

conv_regex_1 = r"total A load: (\d*), total B load: (\d*), total D load: (\d*), raw D: (\d*)\b"
conv_regex_2 = r"A size: (\d*), B size: (\d*), C size: (\d*)\b"
conv_regex_3 = r"inner tile A: (\d*), inner tile B: (\d*), outer loop iteration A: (\d*), outer loop iteration B: (\d*)\b"
conv_regex_4 = r"number of tile: (\d*), target load per tile: (\d*), ideal runtime: (\d*)\b"

resadd_regex = r"resadd A size: (\d*), B size: (\d*), C size: (\d*), number of tile: (\d*), target load per tile: (\d*)\b"


pool_regex = r"pool total load: (\d*), C size: (\d*), number of tile: (\d*), target load per tile: (\d*)\b"

total_cycle_print1 = []
total_cycle_print2 = []

from_dram_str = "{"
conv_print_str = "{"
num_load_str = "{"

for app in range(len(app_name)):
    conv_storage_1 = [['conv number', 'total A load', 'total B load', 'total D load', 'D from dram']]
    conv_storage_2 = [['stream A load', 'stream B load', 'stream C store']]
    conv_storage_4 = [['number of tiles', 'load per tile', 'ideal runtime']]
    conv_storage_3 = [['inner tile A size', 'inner tile B size', 'tile A outer loop factor', 'tile B outer loop factor']]
    conv_process_1 = [['A size (MB)', 'B size (MB)']]
    conv_process_2 = [['reuse A', 'reuse B']]
    conv_process_4 = [['ideal L2 BW usage', 'ideal DRAM BW usage', 'total memory transaction', 'from dram']]
    conv_process_cycle = [['1 core compute', '1 core memory', '1 core prediction',' ', '2 core compute', '2 core memory', '2 core prediction', ' ', '4 core compute', '4 core memory','4 core prediction', ' ', '8 core compute', '8 core memory','8 core prediction', ' ']]
    conv_process_compare = [['1 core mem/compute', '2 core mem/compute', '4 core mem/compute', '8 core mem/compute', ' ', '1 core dram bw predict', '2 core dram bw predict', '4 core dram bw predict', '8 core dram bw predict']]

    resadd_storage = [['resadd number', 'stream (total) A load', 'stream (total) B load', 'stream (total) C store']]
    resadd_process_1 = [['1 core prediction', '2 core prediction', '4 core prediction', '8 core prediction',  'from dram']]
    resadd_process_2 = [['1 core dram bw predict', '2 core dram bw predict', '4 core dram bw predict', '8 core dram bw predict']]
    pool_storage = [['pool number', 'stream (total) load', 'stream (total) store']]
    pool_process = [['1 core prediction', '2 core prediction', '4 core prediction', '8 core prediction']]

    total_predict = [0, 0, 0, 0]
    if app != 0:
        from_dram_str += "},"
        conv_print_str += "},"
        num_load_str += "},"
    from_dram_str += "{"
    conv_print_str += "{"
    num_load_str += "{"

    num_conv = 1
    num_resadd = 1
    num_pool = 1
    read_file_name = app_name[app] + '.txt'
    write_file_name = app_name[app] + '.csv'
    #name_lock = False
    from_dram = 0 # assume nothing gets kicked out
    total_mem = 0 # total memory transaction
    ideal_compute_runtime = 0
    added_A = 0
    with open(read_file_name, "r") as f:
        for line in f.readlines():
            conv_search_1 = re.search(conv_regex_1, line)
            conv_search_2 = re.search(conv_regex_2, line)
            conv_search_3 = re.search(conv_regex_3, line)
            conv_search_4 = re.search(conv_regex_4, line)
            resadd_search = re.search(resadd_regex, line)
            pool_search = re.search(pool_regex, line)

            if resadd_search:
                resadd_4 = [num_resadd]
                for i in range(1, len(resadd_storage[0])):
                    resadd_4.append(int(resadd_search.group(i)))
                resadd_storage.append(resadd_4)
                resadd_predict = [0, 0, 0, 0]
                from_dram, resadd_predict[0] = resadd_layer_runtime_predict(1, resadd_4[1], resadd_4[2], resadd_4[3])
                _, resadd_predict[1] = resadd_layer_runtime_predict(2, resadd_4[1], resadd_4[2], resadd_4[3])
                _, resadd_predict[2] = resadd_layer_runtime_predict(4, resadd_4[1], resadd_4[2], resadd_4[3])
                _, resadd_predict[3] = resadd_layer_runtime_predict(8, resadd_4[1], resadd_4[2], resadd_4[3])
                total_predict = [x + y for x, y in zip(resadd_predict, total_predict)]
                resadd_bw_predict = [from_dram / t for t in resadd_predict]
                resadd_predict.append(int(from_dram))
                resadd_process_1.append(resadd_predict)
                resadd_process_2.append(resadd_bw_predict)
                num_resadd += 1
            elif conv_search_1:
                conv_1 = [num_conv]
                for i in range(1, len(conv_storage_1[0])):
                    conv_1.append(int(conv_search_1.group(i)))
                from_dram = conv_1[-1] # D
                conv_storage_1.append(conv_1)
            elif conv_search_2:
                conv_2 = []
                for i in range(len(conv_storage_2[0])):
                    conv_2.append(int(conv_search_2.group(i+1)))
                conv_process_1.append([x * DIM / 1e6 for x in conv_2[:-1]])
                from_dram += (conv_2[1]) # B size
                from_dram += (conv_2[2]) # C size (for store)
                if from_dram*2 + conv_2[0] > cache_size:
                    from_dram += conv_2[0]
                    added_A = 1
                elif num_conv == 1:
                    added_A = 1
                    from_dram += conv_2[0] # first layer, image from DRAM
                elif conv_storage_2[num_conv-1][-1] != conv_2[0]: # A from somewhere else
                    from_dram += conv_2[0]
                    added_A = 1
                A_reuse = conv_storage_1[num_conv][1]/conv_2[0]
                B_reuse = conv_storage_1[num_conv][2]/conv_2[1]
                conv_storage_2.append(conv_2)
                conv_process_2.append([A_reuse, B_reuse])
            elif conv_search_3:
                conv_3 = []
                for i in range(len(conv_storage_3[0])):
                    conv_3.append(int(conv_search_3.group(i+1)))
                if (conv_3[0] + conv_3[1] + (int(conv_storage_2[num_conv][2]) / max(conv_3[2], conv_3[3]))) > cache_size/1.2 or num_conv == 1: # sum of inner loop A and B tile size
                    #print(num_conv, conv_3[2], conv_3[3], conv_storage_2[num_conv][2])
                    if conv_3[2] > 1: # A from dram multiple times
                        from_dram += (conv_3[2] - added_A) * (conv_3[0])
                    elif conv_3[3] > 1: # matmul
                        from_dram += (int)((conv_3[3] - 1) / 2) * (conv_3[1])

                conv_storage_3.append(conv_3)
            elif conv_search_4:
                conv_4 = []
                for i in range(len(conv_storage_4[0])):
                    conv_4.append(int(conv_search_4.group(i+1)))
                #if num_conv == 1:
                #    conv_4[-1] = conv_4[-1] * DIM / 3 # for 3 channels
                ideal_compute_runtime = conv_4[-1]
                total_mem = conv_4[1] * conv_4[0] + conv_storage_2[num_conv][2] # load per tile * number of tile
                ideal_dram_bw = from_dram / ideal_compute_runtime
                total_l2_bw = total_mem / ideal_compute_runtime
                conv_process_4.append([total_l2_bw, ideal_dram_bw, total_mem, int(from_dram)])
                conv_storage_4.append(conv_4)
                prediction = []
                conv_predict = []
                mem_compute = []
                for i in [1, 2, 4, 8]:
                    [c, m, p] = conv_layer_runtime_predict(i, ideal_compute_runtime, total_l2_bw, ideal_dram_bw, total_mem, from_dram, conv_process_2[-1][0], conv_process_2[-1][1])
                    if i == 2:
                        if num_conv != 1:
                            from_dram_str += ','
                            conv_print_str += ','
                            num_load_str += ','
                        from_dram_str += str(int(from_dram))
                        conv_print_str += str(int(p))
                        num_load_str += str(int(total_mem / i))
                    prediction.append(c)
                    prediction.append(m)
                    prediction.append(p)
                    conv_predict.append(p)
                    mc = m / c
                    mem_compute.append(mc)
                    prediction.append(' ')
                mem_compute.append(' ')
                for i in range(len([1, 2, 4, 8])):
                    mem_compute.append(from_dram / conv_predict[i])
                total_predict = [x + y for x, y in zip(conv_predict, total_predict)]
                conv_process_cycle.append(prediction)
                conv_process_compare.append(mem_compute)
                num_conv += 1
                max_conv = max(max_conv, num_conv)

    # finished reading
    # start writing
    with open(write_file_name, 'w') as f:
        write = csv.writer(f)
        for i in range(len(conv_storage_1)):
            conv_row = conv_storage_1[i]
            conv_row.append(' ')
            for j in range(len(conv_process_1[i])):
                conv_row.append(conv_process_1[i][j])
            conv_row.append(' ')
            for j in range(len(conv_process_2[i])):
                conv_row.append(conv_process_2[i][j])
            conv_row.append(' ')
            #for j in range(len(conv_storage_2[i])):
            #    conv_row.append(conv_storage_2[i][j])
            #conv_row.append(' ')
            for j in range(len(conv_storage_3[i])):
                conv_row.append(conv_storage_3[i][j])
            conv_row.append(' ')
            for j in range(len(conv_storage_4[i])):
                conv_row.append(conv_storage_4[i][j])
            for j in range(len(conv_process_4[i])):
                conv_row.append(conv_process_4[i][j])
            conv_row.append(' ')
            for j in range(len(conv_process_cycle[i])):
                conv_row.append(conv_process_cycle[i][j])
            conv_row.append(' ')
            for j in range(len(conv_process_compare[i])):
                conv_row.append(conv_process_compare[i][j])
            write.writerow(conv_row)
        write.writerow([' '])

        for i in range(len(resadd_storage)):
            resadd_row = (resadd_storage[i])
            resadd_row.append(' ')
            for j in range(len(resadd_process_1[i])):
                resadd_row.append(resadd_process_1[i][j])
            resadd_row.append(' ')
            for j in range(len(resadd_process_2[i])):
                resadd_row.append(resadd_process_2[i][j])
            write.writerow(resadd_row)
        write.writerow([' '])

        total_row = ['total cycle prediction']
        for i in range(len(total_predict)):
            total_row.append(total_predict[i])
        print(total_row)
        total_cycle_print1.append(int(total_row[1]))
        total_cycle_print2.append(int(total_row[2]))
        write.writerow(total_row)

print(total_cycle_print1)
print(total_cycle_print2)
print1_str = "{"
for i in range(len(total_cycle_print1)):
    if i != 0:
        print1_str += ','
    print1_str += str(total_cycle_print1[i])
print1_str += '};'
print2_str = "{"
for i in range(len(total_cycle_print2)):
    if i != 0:
        print2_str += ','
    print2_str += str(total_cycle_print2[i])
print2_str += '};'
from_dram_str += "}};"
conv_print_str += "}};"
num_load_str += "}};"

f=open("precompile.h", "w")
f.write("#define MAX_CONV " + str(max_conv))
f.write('\n')
f.write("#define NUM_WORKLOAD " + str(len(total_cycle_print1)))
f.write('\n')
f.write("uint64_t sp_prediction_cycles[NUM_WORKLOAD] = " + print1_str)
f.write('\n')
f.write("uint64_t tp_prediction_cycles[NUM_WORKLOAD] = " + print2_str)
f.write('\n')
f.write("uint64_t conv_prediction_cycles[NUM_WORKLOAD][MAX_CONV] = " + conv_print_str)
f.write('\n')
f.write("int total_from_dram[NUM_WORKLOAD][MAX_CONV] = " + from_dram_str)
f.write('\n')
f.write("int total_num_load[NUM_WORKLOAD][MAX_CONV] = " + num_load_str)
f.write('\n')
