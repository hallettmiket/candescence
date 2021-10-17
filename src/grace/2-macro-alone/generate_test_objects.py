import numpy as np
from skimage.draw import rectangle_perimeter
from skimage import data, io, filters

from matplotlib.pyplot import figure
import matplotlib.image as mpimg
import matplotlib.pyplot as plt

import mmcv
from mmcv.parallel import collate, scatter
from mmdet.datasets.pipelines import Compose
from mmcv.runner import load_checkpoint
from mmdet.apis import inference_detector, init_detector
from mmdet.apis import show_result_pyplot
from mmdet.models.detectors import BaseDetector

import pickle

from evaluator import *
from glob import glob
import pandas as pd
from pathlib import Path
import os
from os import listdir

numba = "results_slow_lax"
exp = "slow_lax" 
thresh=0.3
cls = 9

CANDESCENCE=   "/home/data/refined/candescence/"
TEST_SET=      CANDESCENCE + "train-data/grace_macro/test/"
MODEL=         CANDESCENCE + "output/grace_macro/exp_" + exp +"/"
OUTPUT=        CANDESCENCE + "performance/grace_macro/"
OUTPUT_IMAGES= OUTPUT + "full_validation_images/"

# # this saves individual subimpages for each predicted bounding box. this is used by the VAE in src/5-vae.
# image_dir = CANDESCENCE + vaes/" + "test_object_" + numba + "_thresh_" + str(thresh) +"/"

classes = {"0": 0, "1": 1, "2": 2, "time":3,"unknown": 4, "artifact": 5, "3":6, "macrophage":7, "UFO":8}

model = init_detector(MODEL + "parameters_" + exp + ".py",  MODEL + "latest.pth")
model.__dict__
model.CLASSES = [i for i in classes]

target_file_names = listdir(TEST_SET)
print(target_file_names)

all_events = pd.DataFrame( index=np.arange(0, len(target_file_names)),
        columns = ['event', 'filename', 'index', 'experiment', 'threshold', 'bbox_1', 'bbox_2','bbox_3','bbox_4', 'gt_class', 'dt_class' ] ) 

tot = 0
gns = 0
for f in target_file_names[0:100]:
    gns=gns+1
    print("genes so far\n")
    print(gns)
    img_orig = TEST_SET + f
    actual_img = mpimg.imread(img_orig)
    res = inference_detector(model, img_orig)
    img = BaseDetector.show_result(img=img_orig,
                    result=res,
                    self=model,
                    score_thr=thresh,
                    wait_time=0,
                    show=False,
                    out_file= OUTPUT_IMAGES + f)
    for i in range(0, len(model.CLASSES)):
        current = res[i]
        for j in range(0, len(current)):
            if (current[j][4] > thresh):
                #new_image = resize(actual_img[int(current[j][1]):int(current[j][3]),int(current[j][0]):int(current[j][2])],(128,128,3))
                # mpimg.imsave(image_dir + str(tot) + "_" + model.CLASSES[i] + "_" + f, new_image )
                all_events.loc[tot] =  [ 'predict',   f,  tot,  exp,   thresh,
                                 int(current[j][0]),
                                 int(current[j][1]),
                                 int(current[j][2]),
                                 int(current[j][3]),
                                 np.nan,
                                 model.CLASSES[i]]
                print("events so far\n")
                print(tot)
                tot += 1

all_events.to_csv(OUTPUT + "test_events_" + numba + "_thresh_" + str(thresh) + ".csv")
#io.imshow(new_image)

