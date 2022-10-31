import sys
import re
import csv
from matplotlib import pyplot as plt

app_name = ['resnet', 'alexnet', 'googlenet', 'squeezenet', 'kwsnet', 'yolonet', 'yololitenet']
#app_name = ['resnet']
num_app = len(app_name)
core_count = [1, 2, 4, 8]

thread_cycle_storage = [[0 for i in range(len(core_count))] for j in range(num_app)]
print(num_app, thread_cycle_storage)
total_cycle_storage = [[0 for i in range(len(core_count))] for j in range(num_app)]

data = dict()
name_regex = r"Test (\S*) batch\d (\d*)\b"
end_regex = r"Script done \S*\b"
thread_cycle_regex = r"\S* repeat (\d) total thread cycles: (\d*)\b"
total_cycle_regex = r"\S* repeat (\d) total cycles: (\d*)\b"
conv_cycle_regex = r"\S* repeat (\d) Conv layer (\d*) worst cycles: (\d*)\b"
matmul_cycle_regex = r"\S* repeat (\d) Matmul layer (\d*) worst cycles: (\d*)\b"
resadd_cycle_regex = r"\S* repeat (\d) Resadd layer (\d*) worst cycles: (\d*)\b"
pool_cycle_regex = r"\S* repeat (\d) Pool layer (\d*) worst cycles: (\d*)\b"

#cycle_regex = r"^turn (\d*) total thread cycles: (\d*)\b"
#file_regex = r"\W*(\w*)_(\w*)_orow\S*\b"
#cycle_regex = r"^Cycle\s*\b(\d*)\b"
#data_regex = r"^PerfCounter (\S*)_FireSim\S*:\s*(\d*)\b"

name = None
num_core = 0
app_index = None
core_index = None
conv_cycle = []
matmul_cycle = []
resadd_cycle = []
pool_cycle = []

total_cycle = [] # only the execution cycle
thread_cycle = [] # including synchronization overhead
#name_lock = False
with open(sys.argv[1], "r") as f:
    for line in f.readlines():
        name_search = re.search(name_regex, line)
        thread_search = re.search(thread_cycle_regex, line)
        total_search = re.search(total_cycle_regex, line)
        conv_search = re.search(conv_cycle_regex, line)
        matmul_search = re.search(matmul_cycle_regex, line)
        resadd_search = re.search(resadd_cycle_regex, line)
        pool_search = re.search(pool_cycle_regex, line)
        end_search = re.search(end_regex, line)

        if name_search or end_search:
            # write previous name
            if name is not None:
                file = name+"_"+str(num_core)+".csv"
                print(file)
                with open(file, 'w') as f:
                    write = csv.writer(f)
                    thread_max = max(thread_cycle)
                    thread_min = min(thread_cycle)
                    thread_avg = int(sum(thread_cycle) / len(thread_cycle))
                    thread_cycle.insert(0, 'total')
                    thread_cycle.insert(1, ' ')
                    thread_cycle.append(' ')
                    thread_cycle.append(thread_min)
                    thread_cycle.append(thread_max)
                    thread_cycle.append(thread_avg)
                    write.writerow(thread_cycle)
                    thread_cycle_storage[app_index][core_index] = thread_avg
                    #print(app_index, core_index, thread_avg, thread_cycle_storage)

                    app_index = None
                    core_index = None

                    for i in range(len(conv_cycle)):
                        average = int(sum(conv_cycle[i]) / len(conv_cycle[i]))
                        conv_max = max(conv_cycle[i])
                        conv_min = min(conv_cycle[i])
                        conv_cycle[i].insert(0, 'conv')
                        conv_cycle[i].insert(1, i)
                        conv_cycle[i].append(' ')
                        conv_cycle[i].append(conv_min)
                        conv_cycle[i].append(conv_max)
                        conv_cycle[i].append(average)
                        write.writerow(conv_cycle[i])
                    for i in range(len(matmul_cycle)):
                        average = int(sum(matmul_cycle[i]) / len(matmul_cycle[i]))
                        matmul_max = max(matmul_cycle[i])
                        matmul_min = min(matmul_cycle[i])
                        matmul_cycle[i].insert(0, 'matmul')
                        matmul_cycle[i].insert(1, i)
                        matmul_cycle[i].append(' ')
                        matmul_cycle[i].append(matmul_min)
                        matmul_cycle[i].append(matmul_max)
                        matmul_cycle[i].append(average)
                        write.writerow(matmul_cycle[i])
                    for i in range(len(resadd_cycle)):
                        average = int(sum(resadd_cycle[i]) / len(resadd_cycle[i]))
                        resadd_max = max(resadd_cycle[i])
                        resadd_min = min(resadd_cycle[i])
                        resadd_cycle[i].insert(0, 'resadd')
                        resadd_cycle[i].insert(1, i)
                        resadd_cycle[i].append(' ')
                        resadd_cycle[i].append(resadd_min)
                        resadd_cycle[i].append(resadd_max)
                        resadd_cycle[i].append(average)
                        write.writerow(resadd_cycle[i])
                    for i in range(len(pool_cycle)):
                        average = int(sum(pool_cycle[i]) / len(pool_cycle[i]))
                        pool_max = max(pool_cycle[i])
                        pool_min = min(pool_cycle[i])
                        pool_cycle[i].insert(0, 'pool')
                        pool_cycle[i].insert(1, i)
                        pool_cycle[i].append(' ')
                        pool_cycle[i].append(pool_min)
                        pool_cycle[i].append(pool_max)
                        pool_cycle[i].append(average)
                        write.writerow(pool_cycle[i])
                conv_cycle = []
                matmul_cycle = []
                resadd_cycle = []
                pool_cycle = []
                total_cycle = []
                thread_cycle = []

            #update name
            if name_search:
                name = name_search.group(1)
                app_index = app_name.index(name) # search for index of the workload
                num_core = int(name_search.group(2))
                core_index = core_count.index(num_core)
        elif thread_search:
            cycle = int(thread_search.group(2))
            thread_cycle.append(cycle)
        elif total_search:
            cycle = int(total_search.group(2))
            total_cycle.append(cycle)
        elif conv_search:
            layer = int(conv_search.group(2))
            cycle = int(conv_search.group(3))
            if len(conv_cycle) <= layer:
                conv_cycle.append([cycle])
            else:
                conv_cycle[layer].append(cycle)
        elif matmul_search:
            layer = int(matmul_search.group(2))
            cycle = int(matmul_search.group(3))
            if len(matmul_cycle) <= layer:
                matmul_cycle.append([cycle])
            else:
                matmul_cycle[layer].append(cycle)
        elif resadd_search:
            layer = int(resadd_search.group(2))
            cycle = int(resadd_search.group(3))
            if len(resadd_cycle) <= layer:
                resadd_cycle.append([cycle])
            else:
                resadd_cycle[layer].append(cycle)
        '''
        elif pool_search:
            layer = int(pool_search.group(2))
            cycle = int(pool_search.group(3))
            if len(pool_cycle) <= layer:
                pool_cycle.append([cycle])
            else:
                pool_cycle[layer].append(cycle)
        '''


