import matplotlib.pyplot as plt

import mmcv
from mmcv.parallel import collate, scatter

import numpy as np
from skimage.draw import rectangle_perimeter
from matplotlib.pyplot import figure
import matplotlib.image as mpimg

import mmcv
from mmcv.parallel import collate, scatter
from mmdet.datasets.pipelines import Compose
from mmcv.runner import load_checkpoint
from mmdet.apis import inference_detector, init_detector
from mmdet.models.detectors import BaseDetector
from skimage import data, io, filters
import pickle
from mmdet.datasets.pipelines import Compose
from mmdet.apis import show_result_pyplot
from evaluator import *
from glob import glob
import pandas as pd
from pathlib import Path
import numpy as np
import matplotlib.pyplot as plt
from skimage import data, io, filters
import os

numba = "_exp27_test_"
exp = "exp27" 
thresh = 0.25

VARASANA = "/home/data/candescence/varasana/"
target_file_location = VARASANA + "/test/"


# this saves individual subimpages for each predicted bounding box. this is used by the VAE in src/5-vae.
image_dir = CANDESCENCE + vaes/" + "test_object_" + numba + "_thresh_" + str(thresh) +"/"

# the directory to save all output
performance_save = CANDESCENCE + "/performance/" + exp + "_results/" 
full_image_dir = performance_save + "full_validation_images/"


classes = {"Yeast White": 0, "Budding White": 1, "Yeast Opaque": 2,
                           "Budding Opaque":3,"Yeast Gray": 4, "Budding Gray": 5,
                            "Shmoo":6,"Artifact": 7, "Unknown ": 8, 
                            "Pseudohyphae": 9, "Hyphae": 10, 
                            "H-junction": 11, "P-junction":12,
                            "P-Start":13,"H-Start":14}

model = init_detector(OUTPUT + exp + "/" + exp + ".py", 
                      OUTPUT + exp + "/" + "latest.pth")
model.__dict__

model.CLASSES = [i for i in classes]

from os import listdir
target_file_names = listdir(target_file_location)
print(target_file_names)

tot = 0
all_events = pd.DataFrame( index=np.arange(0, len(target_file_names)),
        columns = ['event', 'filename', 'index', 'experiment', 'grade', 'threshold', 'bbox_1', 'bbox_2','bbox_3','bbox_4', 'gt_class', 'dt_class' ] ) 
 
for f in target_file_names:
    img_orig = target_file_location + f
    actual_img = mpimg.imread(img_orig)
    res = inference_detector(model, img_orig)
    img = BaseDetector.show_result(img=img_orig,
                    result=res,
                    self=model,
                    score_thr=thresh,
                    wait_time=0,
                    show=False,
                    out_file= full_image_dir + f)
    for i in range(0, len(model.CLASSES)):
        current = res[i]
        for j in range(0, len(current)):
            if (current[j][4] > thresh):
                new_image = resize(actual_img[int(current[j][1]):int(current[j][3]),int(current[j][0]):int(current[j][2])],(128,128,3))
                mpimg.imsave(image_dir + str(tot) + "_" + model.CLASSES[i] + "_" + f, new_image )
                all_events.loc[tot] =  [ 'predict',   f,  tot,  exp,    np.nan,   thresh,
                                 int(current[j][0]),
                                 int(current[j][1]),
                                 int(current[j][2]),
                                 int(current[j][3]),
                                 np.nan,
                                 model.CLASSES[i]]
                print(tot)
                tot += 1
                
all_events.to_csv(performance_save + "test_events_" + numba + "_thresh_" + str(thresh) + ".csv")
#io.imshow(new_image)

