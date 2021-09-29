from evaluator import *
from glob import glob
import pandas as pd
from pathlib import Path
import numpy as np
import os

numba = "_exp27_val_"
target = "val"   # can be {val | train}
exp = 'exp27' 
thresh = 0.25   #  , 0.3, 0.33, 0.35, 0.4, 0.5, 0.6]

parent_dir = CANDESCENCE + "output/" 

target_file = VARASANA + "/train-validation/" + val + "_hyphae.pkl"   


output_dir = CANDESCENCE + "performance/" + exp + "_results/visual_comparison/" + str(thresh) +"/"
config_file = parent_dir + exp + "/" + exp + ".py"
network =  parent_dir + exp + "/latest.pth"

eval = Evaluator(config_file, network, target_file)

for ind in range(0,len(eval.filenames)):
    print("\nCurrent filename: %s" % eval.filenames[ind])
    annotations = eval.get_gts(eval.filenames[ind])
    new_image = mmcv.imshow_det_bboxes(å
            eval.filenames[ind],
            annotations["ann"]["bboxes"],
            annotations["ann"]["labels"],
            class_names=eval.model.CLASSES,
            score_thr=0,
            wait_time=0,
            show=False,
            out_file=output_dir + "gt_" + str(ind) + ".png")
    eval.draw_silent_dts(eval.filenames[ind],thresh, output_dir + "pred_" + str(ind) + ".png")
    os.system( "montage " + output_dir + "gt_" + str(ind) + ".png " + output_dir + "pred_" + str(ind) + ".png -tile 2x1 -geometry +0+0 " + output_dir + "join" + str(ind) + ".png" )
    os.system( "rm " + output_dir + "gt_" + str(ind) + ".png" )
    os.system( "rm " + output_dir + "pred_" + str(ind) + ".png" )


