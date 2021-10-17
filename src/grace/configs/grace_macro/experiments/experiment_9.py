#    python ~/repo/candescence/src/grace/configs/grace_macro/experiments/experiment_0.py 
#from subprocess import Popen
import os
import sys

exp = "9" # experiment number
gpu = "9"
lr = "0.001"
momentum = "0.9"
decay = "0.001"
total_epochs=3000
freeze = 3 
load=True
pretrained=False

if freeze==-1:
    freeze="m1"

PATH_REPO="/home/hallett/repo/candescence/"
PATH_EXPERIMENTS=PATH_REPO+"src/grace/configs/grace_macro/experiments"
PATH_CONFIGS=PATH_REPO+"src/grace/configs/grace_macro/"
LOAD_FROM="'/home/data/refined/candescence/production/models/candescence_version_1.0/model.pth'"
MMDETECTION="python /home/data/analysis-tools/mmdetection/tools/train.py "
OUTPUT_FOLDER="/home/data/refined/candescence/output/grace_macro/exp_" + exp 

cfg_file = PATH_CONFIGS + "parameters_" + exp + ".py"
f = open(cfg_file, "w+")
if (pretrained==True):
    f.write("\n_base_=[ 'config_" + str(freeze) + "_pre.py' ]\n")
else:
    f.write("\n_base_=[ 'config_" + str(freeze) + "_nopre.py' ]\n")
f.write("\ntotal_epochs = " + str(total_epochs) + "\n")
f.write("\noptimizer = dict(lr=" + lr + ", momentum=" + momentum + ", weight_decay=" + decay + ", paramwise_cfg=dict(bias_lr_mult=2., bias_decay_mult=0.))\n")
if (load==False):
    f.write("\nload_from = None\n")
else:
    f.write("\nload_from = " + LOAD_FROM + "\n")
f.close()

full_command = MMDETECTION + cfg_file + " --work-dir=" + OUTPUT_FOLDER 
full_command = full_command + " --gpu-ids=" + gpu
try:
    os.system(full_command)
    try:
        os.mkdir(output_folder + "/" + j)
    except OSError:
        print ("Creation of the directory %s failed" % j)
    os.system("cp "+ output_folder + "/*" + " " +  output_folder + "/" + j)
except:
    print("\n\nDid not execute.")


