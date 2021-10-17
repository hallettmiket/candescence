import torch
import glob
from mmcv import Config, DictAction
from mmdet.models import build_detector
from mmcv.runner import load_checkpoint
import mmcv
from mmdet.datasets.pipelines import Compose  # TO LOOK AT
from mmcv.parallel import collate, scatter
from mmdet.core import bbox2result
import pickle
import numpy as np
from utils import*

num_classes=9
class_to_number = {"0": 0, "1": 1, "2": 2, "time":3,"unknown": 4, "artifact": 5, 
            "3":6, "macrophage":7, "UFO":8}
number_to_class = {y:x for x,y in class_to_number.items()}

class Evaluator():
    def __init__(self,config,checkpoint_file,annotation_path,img_path):
        self.cfg = Config.fromfile(config)
        self.cfg["gpu-ids"] = 6
        self.model = build_detector(
        self.cfg.model, train_cfg=self.cfg.train_cfg, test_cfg=self.cfg.test_cfg)
        checkpoint_dict = load_checkpoint(self.model,checkpoint_file)
        state_dict = checkpoint_dict["state_dict"]
        self.model.CLASSES = checkpoint_dict['meta']['CLASSES']
        self.model.load_state_dict(state_dict)
        self.model.eval()
        self.confusion_matrix_list = []
        self.counts = {'all':[]}
        self.image_path=img_path

        self.classification_errors = {i:[] for i in self.model.CLASSES}
        self.classification_good = {i:[] for i in self.model.CLASSES}

        self.blindspots = {'all':[]}
        self.mirages = {'all': []}
        
        
        self.data = []
        with (open(annotation_path, "rb")) as openfile:
            while True:
                try:
                    self.data.append(pickle.load(openfile))
                except EOFError:
                    break
        self.data = self.data[0]
        self.filenames = [self.image_path + i["filename"] for i in self.data]
        
    def draw_dts(self,fname,threshold):
        data = dict(img_info=dict(filename=fname), img_prefix=None)
        test_pipeline = Compose(self.cfg.data.val.pipeline)
        data = test_pipeline(data)
        data = collate([data], samples_per_gpu=1)
        x = self.model.extract_feat(data["img"][0]) 
        outs = self.model.bbox_head(x)  # What does this output look like? Is it per pixel? (Note: review the FCOS paper)
        bbox_list = self.model.bbox_head.get_bboxes(
            *outs, img_metas = data["img_metas"][0].data[0], rescale=False)
        
        new_image = mmcv.imshow_det_bboxes(
            fname,
            bbox_list[0][0].numpy(),
            bbox_list[0][1].numpy(),
            class_names=self.model.CLASSES,
            score_thr=threshold,
            wait_time=0,
            show=False,
            out_file=None)
        figure(num=None, figsize=(20, 20))
        io.imshow(new_image)
        
    def draw_silent_dts(self,fname,threshold, out_name):
        data = dict(img_info=dict(filename=fname), img_prefix=None)
        test_pipeline = Compose(self.cfg.data.val.pipeline)
        data = test_pipeline(data)
        data = collate([data], samples_per_gpu=1)
        x = self.model.extract_feat(data["img"][0]) 
        outs = self.model.bbox_head(x)  # What does this output look like? Is it per pixel? (Note: review the FCOS paper)
        bbox_list = self.model.bbox_head.get_bboxes(
            *outs, img_metas = data["img_metas"][0].data[0], rescale=False)
        
        new_image = mmcv.imshow_det_bboxes(
            fname,
            bbox_list[0][0].numpy(),
            bbox_list[0][1].numpy(),
            class_names=self.model.CLASSES,
            score_thr=threshold,
            wait_time=0,
            show=False,
            out_file=out_name)
        #figure(num=None, figsize=(20, 20))

        
    def get_dts(self,filename):
        #cls = class_to_number[cls]
        data = dict(img_info=dict(filename=filename), img_prefix=None)
        test_pipeline = Compose(self.cfg.data.val.pipeline)
        data = test_pipeline(data)
        data = collate([data], samples_per_gpu=1)

        x = self.model.extract_feat(data["img"][0])  # Feature maps

        outs = self.model.bbox_head(x)  # What does this output look like? Is it per pixel? (Note: review the FCOS paper)
        bbox_list = self.model.bbox_head.get_bboxes(
                    *outs, img_metas=data["img_metas"][0].data[0], rescale=False)

        #return np.array([bbox_list[0][0][i].numpy() for i in range(len(bbox_list[0][0].numpy())) if bbox_list[0][1][i] == cls])
        return bbox_list
    
    @staticmethod
    def filter_dts(bbox_list,cls,threshold=0.4):
        cls = class_to_number[cls]
        bbs = np.array([bbox_list[0][0][i].numpy() for i in range(len(bbox_list[0][0].numpy())) if bbox_list[0][1][i] == cls])
        bbs = np.array([i for i in bbs if i[4] > threshold])
        return bbs
    
    def get_threshold_dts(self, bbox_list, threshold=0.4):
        bbs = np.array([bbox_list[0][0][i].numpy() for i in range(len(bbox_list[0][0].numpy())) if True])
        bbs = np.array([i for i in bbs if i[4] > threshold])
        return bbs
   

    def get_gts(self,filename):
        return [i for i in self.data if i['filename'] in filename][0]
        
    @staticmethod
    def filter_gts(tmp,cls):
        #if bool(tmp):
        bboxes = tmp["ann"]["bboxes"]
        labels = [number_to_class[i] for i in tmp["ann"]["labels"]]
        return np.array([bboxes[i] for i in range(len(bboxes)) if labels[i] == cls])
        #else:
          #  print("Class " + cls + " not found.")
          
        
    def get_confusion(self,filename,threshold,ids = None,confusion_matrix=None,tmp = True):
        dts = self.get_dts(filename)
        gts = self.get_gts(filename)
        if tmp:
            ids = [class_to_number[i] for i in self.model.CLASSES]
        confusion_matrix = np.zeros((len(ids),len(ids)))
        for i in ids:
            dts_filtered = self.filter_dts(dts,number_to_class[i],threshold)
            gts_filtered = self.filter_gts(gts,number_to_class[i])
            if len(gts_filtered) > 0 and len(dts_filtered) > 0:
                tp,fp = tpfp_default(dts_filtered,gts_filtered)
                tps = sum(tp[0])
                confusion_matrix[i][i] += tps
            for j in ids:
                if j != i:
                    gts_filtered = self.filter_gts(gts,number_to_class[j])
                    if len(gts_filtered) > 0 and len(dts_filtered) > 0:
                        tp,fp = tpfp_default(dts_filtered,gts_filtered)
                        fps = sum(tp[0])
                        confusion_matrix[i][j] += fps
        return confusion_matrix
    
    
    def get_fps_bboxes(self,filename,threshold):
        ids = [class_to_number[i] for i in self.model.CLASSES]
        dts = self.get_dts(filename)
        gts = self.get_gts(filename)
        for i in ids:
            bboxes_dict = {}
            dts_filtered = self.filter_dts(dts,number_to_class[i],threshold)
            for j in ids:
                if j != i:
                    gts_filtered = self.filter_gts(gts,number_to_class[j])
                    if len(gts_filtered) > 0 and len(dts_filtered) > 0:
                        tp,fp = tpfp_default(dts_filtered,gts_filtered)
                        if sum(tp[0]) > 0:
                            bboxes_dict[number_to_class[j]] = dts_filtered[list(map(bool,tp[0]))]
            self.classification_errors[number_to_class[i]].append({"filename":filename,"bboxes":bboxes_dict})

    def get_good_bboxes(self,filename,threshold):
        ids = [class_to_number[i] for i in self.model.CLASSES]
        dts = self.get_dts(filename)
        gts = self.get_gts(filename)
        for i in ids:
            bboxes_dict = {}
            dts_filtered = self.filter_dts(dts,number_to_class[i],threshold)
            # only interested in diagnonal
            gts_filtered = self.filter_gts(gts,number_to_class[i])
            if len(gts_filtered) > 0 and len(dts_filtered) > 0:
                tp,fp = tpfp_default(dts_filtered,gts_filtered)
                if sum(tp[0]) > 0:
                  bboxes_dict[number_to_class[i]] = dts_filtered[list(map(bool,tp[0]))]
            self.classification_good[number_to_class[i]].append({"filename":filename,"bboxes":bboxes_dict})


    @staticmethod
    def my_filter_dts(bbox_list, threshold=0.4):
        bbs = np.array([bbox_list[0][0][i].numpy() for i in range(len(bbox_list[0][0].numpy())) if True])
        bbs = np.array([i for i in bbs if i[4] > threshold])
        #lbls= np.array([bbox_list[0][1][i].numpy() for i in range(len(bbox_list[0][1].numpy())) if True])
        #lbls = np.array([i for i in lbs if i[4] > threshold])

        return bbs #, lbls]

    @staticmethod
    def my_filter_gts(tmp):
        #if bool(tmp):
        bboxes = tmp["ann"]["bboxes"]
        labels = tmp["ann"]["labels"]
        return(bboxes, labels)
          
                 
    def get_fns_bboxes(self,filename,threshold):
        ids = [class_to_number[i] for i in self.model.CLASSES]
        dts = self.get_dts(filename)
        dts_filtered = self.my_filter_dts(dts, threshold)
        gts = self.get_gts(filename)
        gts_filtered, labels = self.my_filter_gts(gts)
        bboxes_dict = {}
        tmp =[]
        if len(gts) > 0 and len(dts) > 0:
            tp,fp = tpfp_default( gts_filtered, dts_filtered )
            if sum(fp[0]) > 0:
                bboxes_dict['fuck'] = gts_filtered[list(map(bool,fp[0]))]
                tmp = labels[list(map(bool,fp[0]))]
            self.blindspots['all'].append({"filename":filename,"bboxes":bboxes_dict, "classes":tmp})

    @staticmethod
    def my_mirage_filter_dts(bbox_list, threshold=0.4):
        bbs = np.array([bbox_list[0][0][i].numpy() for i in range(len(bbox_list[0][0].numpy())) if True])
        lbls= np.array([bbox_list[0][1][i].numpy() for i in range(len(bbox_list[0][1].numpy())) if True])
        bbs2 = np.array([i for i in bbs if i[4] > threshold])
        lbls= np.array([lbls[i] for i in range(0, len(lbls)) if bbs[i][4]>threshold])
        return bbs2, lbls


    def get_mirage_bboxes(self,filename,threshold):
        ids = [class_to_number[i] for i in self.model.CLASSES]
        dts = self.get_dts(filename)
        dts_filtered, dts_labels = self.my_mirage_filter_dts(dts, threshold)
        gts = self.get_gts(filename)
        gts_filtered, labels = self.my_filter_gts(gts)

        bboxes_dict = {}
        tmp =[]
        if len(gts)==0 and len(dts_filtered) > 0:
            # all bboxes are mirages
            bboxes_dict['hallucination'] = dts_filtered
            tmp = dts_labels
            self.mirages['all'].append({"filename":filename,"bboxes":bboxes_dict, "classes":tmp})    
        elif len(gts_filtered) > 0 and len(dts_filtered) > 0:
            print("\nIn Mirage:")
            print(dts_filtered)
            print("\n\n")
            print(gts_filtered)
            tp,fp = tpfp_default(dts_filtered,gts_filtered)
            if sum(fp[0]) > 0:
                bboxes_dict['hallucination'] = dts_filtered[list(map(bool,fp[0]))]
                tmp = dts_labels[list(map(bool,fp[0]))]
            self.mirages['all'].append({"filename":filename,"bboxes":bboxes_dict, "classes":tmp})

            
    def get_blindspots(self, threshold):
        count = 0
        for i in self.filenames:
            self.get_fns_bboxes(i,threshold)
            count +=1 
            #print(count)
        fn_classes = [0]*num_classes
        for i in self.blindspots['all']:
            for j in i['classes']:
                fn_classes[j] += 1
        return(fn_classes)
      
    
    def get_classification_errors(self,threshold):
        for i in self.filenames:
            self.get_fps_bboxes(i,threshold)
 
    def get_classification_good(self,threshold):
        for i in self.filenames:
            self.get_good_bboxes(i,threshold)
            
    
    def get_mirages(self, threshold):
        count = 0
        for i in self.filenames:
            self.get_mirage_bboxes(i, threshold)
            count +=1
        mirage_classes = [0]*num_classes
        for i in self.mirages['all']:
            for j in i['classes']:
                mirage_classes[j] += 1
        return(mirage_classes)
            # print(count)

    
    def get_confusion_all(self,threshold=0):
        ids = [class_to_number[i] for i in self.model.CLASSES]
        confusion_matrix = np.zeros((len(ids),len(ids)))
        for i in self.filenames:
            confusion_matrix = self.get_confusion(i,threshold,ids,confusion_matrix,tmp=False)
            self.confusion_matrix_list.append(confusion_matrix)

        
    def get_gt_stats(self):
        for i in self.filenames:
            tmp = [0]*len(self.model.CLASSES)
            gts = self.get_gts(i)
            gts_filtered, labels = self.my_filter_gts(gts)
            for j in labels:
                tmp[j] += 1
            self.counts['all'].append({"filename":i,"counts":tmp})
    
    def get_threshold_dts(self, filename, threshold=0.4):
        dts = self.get_dts(filename)
        return( self.my_mirage_filter_dts(dts, threshold) )
    

