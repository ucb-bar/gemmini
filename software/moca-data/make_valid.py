import sys
import re
import csv

ideal_runtime_index = 0
from_dram_index = 0

app_name_list = ['resnet', 'alexnet', 'kwsnet', 'squeezenet', 'googlenet', 'yolonet', 'yololitenet']
copy_file_path = sys.argv[1]


for app in range(len(app_name_list)):
    app_name = app_name_list[app] #str(sys.argv[2])
    read_file_name = app_name + '.txt'
    write_file_name = app_name + '.csv'

    copy_file = copy_file_path + "/" + write_file_name
    core = [1]

    conv = []
    pool = []
    resadd = []
    conv = [[] for j in range(len(core))]
    pool = [[] for j in range(len(core))]
    resadd = [[] for j in range(len(core))]

    num_conv = 1
    num_resadd = 1
    num_pool = 1

    thread_cycle = []
    total_cycle = []

    for i in range(len(core)):
        c = core[i]
        read_file_name = app_name+'_'+str(c)+'.csv'
        print(read_file_name)
        with open(read_file_name, 'r') as read_obj:
            # pass the file object to reader() to get the reader object
            csv_reader = csv.reader(read_obj)
            # Iterate over each row in the csv using reader object
            for row in csv_reader:
                if(row[0] == "thread overhead"):
                    thread_cycle.append(int(row[-1]))
                elif(row[0] == "total"):
                    total_cycle.append(int(row[-1]))
                if(row[0] == "conv" or row[0] == "matmul"):
                    conv[i].append(int(row[-1]))
                elif(row[0] == "resadd"):
                    resadd[i].append(int(row[-1]))
                elif(row[0] == "pool"):
                    pool[i].append(int(row[-1]))


    num_conv = -1
    num_resadd = -1
    num_pool = -1

    new_storage = []

    with open(copy_file, 'r') as f:
        csv_reader = csv.reader(f)
        for row in csv_reader:
            if row[0].isnumeric():
                if num_conv >= 0:
                    new_row = row
                    new_row.append(' ')
                    scale = []
                    ideal_runtime = float(new_row[ideal_runtime_index])
                    for i in range(len(core)):
                        new_row.append(conv[i][num_conv])
                    #    scale.append(conv[0][num_conv] / conv[i][num_conv])
                    new_row.append(' ')
                    from_dram = int(new_row[from_dram_index])
                    for i in range(len(core)):
                        new_row.append(from_dram / conv[i][num_conv])
                    for i in range(len(core)):
                        scale.append(ideal_runtime / conv[i][num_conv])
                    new_row.append(' ')
                    for i in scale:
                        new_row.append(i)
                    new_storage.append(new_row)
                    num_conv += 1

                elif num_resadd >= 0:
                    new_row = row
                    new_row.append(' ')
                    scale = []
                    ideal_runtime = float(new_row[ideal_runtime_index])
                    for i in range(len(core)):
                        new_row.append(resadd[i][num_resadd])
                    #    scale.append(resadd[0][num_resadd] / resadd[i][num_resadd])
                    new_row.append(' ')
                    from_dram = int(float(new_row[from_dram_index]))
                    for i in range(len(core)):
                        new_row.append(from_dram / resadd[i][num_resadd])
                    for i in range(len(core)):
                        scale.append(ideal_runtime / resadd[i][num_resadd])
                    new_row.append(' ')
                    for i in scale:
                        new_row.append(i)
                    new_storage.append(new_row)
                    num_resadd += 1

                elif num_pool >= 0:
                    new_row = row
                    new_row.append(' ')
                    scale = []
                    ideal_runtime = float(new_row[ideal_runtime_index])
                    for i in range(len(core)):
                        new_row.append(pool[i][num_pool])
                    #    scale.append(pool[0][num_pool] / pool[i][num_pool])
                    for i in range(len(core)):
                        scale.append(ideal_runtime / pool[i][num_pool])
                    new_row.append(' ')
                    for i in scale:
                        new_row.append(i)
                    new_storage.append(new_row)
                    num_pool += 1

            else:
                new_row = row
                if(row[0] == "conv number"):
                    new_row.append(' ')
                    new_row.append('1 core runtime')
                    new_row.append(' ')
                    new_row.append('dram bw usage')
                    new_row.append(' ')
                    new_row.append('accuracy')
                if(row[0] == "resadd number" or row[0] == "pool number"):
                    new_row.append(' ')
                    new_row.append('1 core runtime')
                    new_row.append(' ')
                    new_row.append('dram bw usage')
                    new_row.append(' ')
                    new_row.append('accuracy')
                new_storage.append(new_row)
                if(row[0] == "conv number" or row[0] == "resadd number" or row[0] == "pool number"):
                    temp = 0
                    for contents in row:
                        if str(contents) == "1 core prediction":
                            ideal_runtime_index = temp
                            break
                        else:
                            temp += 1
                if(row[0] == "conv number" or row[0] == "resadd number"):
                    temp = 0
                    for contents in row:
                        if str(contents) == "from dram":
                            from_dram_index = temp
                            break
                        else:
                            temp += 1
                if(row[0] == "total cycle prediction"):
                    thread_row = ['real thread cycle']
                    for x in range(len(thread_cycle)):
                        thread_row.append(thread_cycle[x])
                    new_storage.append(thread_row)
                    total_row = ['real total cycle']
                    for x in range(len(total_cycle)):
                        total_row.append(total_cycle[x])
                    new_storage.append(total_row)


            if(row[0] == "conv number"):
                num_conv = 0
                num_resadd = -1
                num_pool = -1

            if(row[0] == "resadd number"):
                num_conv = -1
                num_resadd = 0
                num_pool = -1

            if(row[0] == "pool number"):
                num_conv = -1
                num_resadd = -1
                num_pool = 0



# finished reading
# start writing

    with open(write_file_name, 'w') as f:
        write = csv.writer(f)
        for i in range(len(new_storage)):
            write.writerow(new_storage[i])


