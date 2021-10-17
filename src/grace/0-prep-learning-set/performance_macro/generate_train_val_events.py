from evaluator import *
from glob import glob
import pandas as pd
from pathlib import Path
import numpy as np
from functools import reduce
from collections import defaultdict
import json

numba = "results_slow_lax"
exp = "slow_lax" 
target = "train"   # can be {val | train}
thresholds = [0.2, 0.3, 0.4, 0.5, 0.6]
cls = 9

CANDESCENCE="/home/data/refined/candescence/"
PARENT_RAW_OUTPUT=CANDESCENCE+"output/grace_macro/"
SAVE_RESULTS=CANDESCENCE+"performance/grace_macro/"
IMAGE_PATH=CANDESCENCE+"train-data/grace_macro/" + target + "/"

DATASET=CANDESCENCE+"train-data/grace_macro"

for threshold in thresholds:
    config_file = PARENT_RAW_OUTPUT + "exp_" + exp + "/parameters_" + exp + ".py"
    if not(Path(config_file).is_file()):
        print("config problem")
    network=PARENT_RAW_OUTPUT + "exp_" + exp + "/latest.pth"
    if not(Path(network).is_file()):
        print("network problem")
    dataset = DATASET + "/" +target + "_grace_macro.pkl"
    try:
        evals=Evaluator(config_file, network, dataset, IMAGE_PATH)
        evals.get_blindspots(threshold = threshold)  # sets self.blindspots
        evals.get_mirages(threshold = threshold)     # sets self.mirages
        evals.get_classification_errors(threshold = threshold)   # sets self.classification_errors
        evals.get_classification_good(threshold = threshold)     # sets self.classification_good
    except:
        print("\nFailed on " + exp + " " + str(threshold))
    all_events = pd.DataFrame( columns = ['event', 'filename', 'experiment', 'threshold', 'type', 'bbox_1', 'bbox_2','bbox_3','bbox_4', 'gt_class', 'dt_class' ] ) 
    # blindspots
    for b in evals.blindspots['all']:
        if len(b['bboxes']) > 0:
            fname = b['filename']
            clss = b['classes']
            bb = b['bboxes']['fuck']
            ttt = bb.tolist()
            bb1 = [item[0] for item in ttt]
            bb2 = [item[1] for item in ttt]
            bb3 = [item[2] for item in ttt]
            bb4 = [item[3] for item in ttt]
            tmp_events = pd.DataFrame.from_dict(dict( [
                                ('event', ['blindspot']*len(clss)), 
                                ('filename', [fname]*len(clss)), 
                                ('experiment', [exp]*len(clss)), 
                                ('threshold', [threshold]*len(clss)),
                                ('type', [target]*len(clss)),
                                ('bbox_1', bb1), 
                                ('bbox_2', bb2), 
                                ('bbox_3', bb3), 
                                ('bbox_4', bb4), 
                                ('gt_class', [evals.model.CLASSES[i] for i in clss]), 
                                ('dt_class', [np.nan]*len(clss) ) ] ))
            all_events = all_events.append( tmp_events )
    # hallucinations (previously known as mirages)
    for b in evals.mirages['all']:
        if len(b['bboxes']) > 0:
            fname = b['filename']
            clss = b['classes']
            bb = b['bboxes']['hallucination']
            ttt = bb.tolist()
            bb1 = [item[0] for item in ttt]
            bb2 = [item[1] for item in ttt]
            bb3 = [item[2] for item in ttt]
            bb4 = [item[3] for item in ttt]
            tmp_events = pd.DataFrame.from_dict(dict( [
                                ('event', ['hallucination']*len(clss)), 
                                ('filename', [fname]*len(clss)), 
                                ('experiment', [exp]*len(clss)), 
                                ('threshold', [threshold]*len(clss)),
                                ('type', [target]*len(clss)),
                                ('bbox_1', bb1), 
                                ('bbox_2', bb2), 
                                ('bbox_3', bb3), 
                                ('bbox_4', bb4), 
                                ('gt_class', [np.nan]*len(clss)), 
                                ('dt_class', [evals.model.CLASSES[i] for i in clss] ) ]))
            all_events = all_events.append( tmp_events )
    # classification errors
    for b in range(0, len(evals.model.CLASSES)):
        target_cls = evals.classification_errors[evals.model.CLASSES[b]]
        for c in target_cls:
            if len(c['bboxes']) > 0:
                fname = c['filename']
                kys= [*c['bboxes']]
                for k in kys:
                    current = c['bboxes'][k]
                    ttt = current.tolist()
                    bb1 = [item[0] for item in ttt]
                    bb2 = [item[1] for item in ttt]
                    bb3 = [item[2] for item in ttt]
                    bb4 = [item[3] for item in ttt]
                    tmp_events = pd.DataFrame.from_dict(dict( [
                                ('event', ['class_error']*len(current)), 
                                ('filename', [fname]*len(current)), 
                                ('experiment', [exp]*len(current)), 
                                ('threshold', [threshold]*len(current)),
                                ('type', [target]*len(current)),
                                ('bbox_1', bb1), 
                                ('bbox_2', bb2), 
                                ('bbox_3', bb3), 
                                ('bbox_4', bb4), 
                                ('gt_class', [k]*len(current)), 
                                ('dt_class', evals.model.CLASSES[b] ) ]))
                    all_events = all_events.append( tmp_events )
    # true positives  
    for b in range(0, len(evals.model.CLASSES)):
        target_cls = evals.classification_good[evals.model.CLASSES[b]]
        for c in target_cls:
            if len(c['bboxes']) > 0:
                fname = c['filename']
                kys= [*c['bboxes']]
                for k in kys:
                    current = c['bboxes'][k]
                    ttt = current.tolist()
                    bb1 = [item[0] for item in ttt]
                    bb2 = [item[1] for item in ttt]
                    bb3 = [item[2] for item in ttt]
                    bb4 = [item[3] for item in ttt]
                    tmp_events = pd.DataFrame.from_dict(dict( [
                        ('event', ['class_good']*len(current)), 
                        ('filename', [fname]*len(current)), 
                        ('experiment', [exp]*len(current)), 
                        ('threshold', [threshold]*len(current)),
                        ('type', [target]*len(current)),
                        ('bbox_1', bb1), 
                        ('bbox_2', bb2), 
                        ('bbox_3', bb3), 
                        ('bbox_4', bb4), 
                        ('gt_class', [k]*len(current)), 
                        ('dt_class', evals.model.CLASSES[b] ) ]))
                    all_events = all_events.append( tmp_events )
    # save to file so that it can be picked up in R.
    all_events.to_csv(SAVE_RESULTS + "all_events_" + numba + "_type_" + target + "_thresh_" + str(threshold) + ".csv")
        
            
            



