import torch

import glob

from mmcv import Config, DictAction
from mmdet.models import build_detector
from mmcv.runner import load_checkpoint
import mmcv

from mmdet.datasets.pipelines import Compose  # TO LOOK AT
from mmcv.parallel import collate, scatter



from mmdet.core import bbox2result

from skimage import data, io, filters
from matplotlib.pyplot import figure

import pickle

from skimage.transform import resize

import matplotlib.pyplot as plt
import numpy as np

import numpy as np
from skimage.draw import rectangle_perimeter
from skimage.transform import resize
from matplotlib.pyplot import figure


def save_fp_panel(evaluator,cls,output_path):
    fig = plt.figure(figsize=(50,50))
    c = 1
    for cc in evaluator.model.CLASSES:
        if cc != cls:
            for i in evaluator.fp_dict[cls]:
                image = io.imread(i["filename"])
            #if len(image.shape) == 3:
             #   image = image[:,:,0]     
                if cc in list(i["bboxes"].keys()):
                    for j in i["bboxes"][cc]:
                        new_image = resize(image[int(j[1]):int(j[3]),int(j[0]):int(j[2])],(128,128,3))
                        ax1 = fig.add_subplot(15,15,c)
                        ax1.imshow(new_image)
                        c += 1
                        
    plt.savefig("fp_panel_" + cls + ".png")  


def save_fn_panel(evaluator,cls,output_path):
    fig = plt.figure(figsize=(50,50))
    c = 1
    for cc in evaluator.model.CLASSES:
        if cc != cls:
            for i in evaluator.fp_dict[cls]:
                image = io.imread(i["filename"])
            #if len(image.shape) == 3:
             #   image = image[:,:,0]     
                if cc in list(i["bboxes"].keys()):
                    for j in i["bboxes"][cc]:
                        new_image = resize(image[int(j[1]):int(j[3]),int(j[0]):int(j[2])],(128,128,3))
                        ax1 = fig.add_subplot(15,15,c)
                        ax1.imshow(new_image)
                        c += 1
                        
    plt.savefig(output_path + "fp_panel_" + evaluator.name + "_" + cls + ".png")  

    
def get_precision_recall(model,cfg,image_fname,cls):
    dts = get_bbs(model,cfg,image_fname,class_to_number[cls])
    gts = get_gt_boxes(image_fname,train_data,cls)
    tp,fp = tpfp_default(dts,gts)
    recall = sum(tp[0])/len(gts)
    precision = sum(tp[0])/(sum(tp[0])+sum(fp[0]))
    return recall, precision


def get_bbs(model,cfg,image_fname,cls):
    data = dict(img_info=dict(filename=image_fname), img_prefix=None)
    test_pipeline = Compose(cfg.data.val.pipeline)
    data = test_pipeline(data)
    data = collate([data], samples_per_gpu=1)
    
    x = model.extract_feat(data["img"][0])  # Feature maps
    
    outs = model.bbox_head(x)  # What does this output look like? Is it per pixel? (Note: review the FCOS paper)
    bbox_list = model.bbox_head.get_bboxes(
                *outs, data["img_metas"][0].data[0], rescale=False)
    
    return np.array([bbox_list[0][0][i].numpy() for i in range(len(bbox_list[0][0].numpy())) if bbox_list[0][1][i] == cls])
    
    
   

def get_gt_boxes(filename,data,cls):
    tmp = [i for i in data if i['filename'] in filename][0]
    if bool(tmp):
        bboxes = tmp["bbox"]
        labels = [number_to_class[i] for i in tmp["labels"]]
        return np.array([bboxes[i] for i in range(len(bboxes)) if labels[i] == cls])
    else:
        print("Class " + cls + " not found.")




def bbox_overlaps(bboxes1, bboxes2, mode='iou', eps=1e-6):
    """Calculate the ious between each bbox of bboxes1 and bboxes2.

    Args:
        bboxes1(ndarray): shape (n, 4)
        bboxes2(ndarray): shape (k, 4)
        mode(str): iou (intersection over union) or iof (intersection
            over foreground)

    Returns:
        ious(ndarray): shape (n, k)
    """

    assert mode in ['iou', 'iof']

    bboxes1 = bboxes1.astype(np.float32)
    bboxes2 = bboxes2.astype(np.float32)
    rows = bboxes1.shape[0]
    cols = bboxes2.shape[0]
    ious = np.zeros((rows, cols), dtype=np.float32)
    if rows * cols == 0:
        return ious
    exchange = False
    if bboxes1.shape[0] > bboxes2.shape[0]:
        bboxes1, bboxes2 = bboxes2, bboxes1
        ious = np.zeros((cols, rows), dtype=np.float32)
        exchange = True
    area1 = (bboxes1[:, 2] - bboxes1[:, 0]) * (bboxes1[:, 3] - bboxes1[:, 1])
    area2 = (bboxes2[:, 2] - bboxes2[:, 0]) * (bboxes2[:, 3] - bboxes2[:, 1])
    for i in range(bboxes1.shape[0]):
        x_start = np.maximum(bboxes1[i, 0], bboxes2[:, 0])
        y_start = np.maximum(bboxes1[i, 1], bboxes2[:, 1])
        x_end = np.minimum(bboxes1[i, 2], bboxes2[:, 2])
        y_end = np.minimum(bboxes1[i, 3], bboxes2[:, 3])
        overlap = np.maximum(x_end - x_start, 0) * np.maximum(
            y_end - y_start, 0)
        if mode == 'iou':
            union = area1[i] + area2 - overlap
        else:
            union = area1[i] if not exchange else area2
        union = np.maximum(union, eps)
        ious[i, :] = overlap / union
    if exchange:
        ious = ious.T
    return ious





def tpfp_default(det_bboxes,
                 gt_bboxes,
                 gt_bboxes_ignore=None,
                 iou_thr=0.5,
                 area_ranges=None):
    """Check if detected bboxes are true positive or false positive.

    Args:
        det_bbox (ndarray): Detected bboxes of this image, of shape (m, 5).
        gt_bboxes (ndarray): GT bboxes of this image, of shape (n, 4).
        gt_bboxes_ignore (ndarray): Ignored gt bboxes of this image,
            of shape (k, 4). Default: None
        iou_thr (float): IoU threshold to be considered as matched.
            Default: 0.5.
        area_ranges (list[tuple] | None): Range of bbox areas to be evaluated,
            in the format [(min1, max1), (min2, max2), ...]. Default: None.

    Returns:
        tuple[np.ndarray]: (tp, fp) whose elements are 0 and 1. The shape of
            each array is (num_scales, m).
    """
    # an indicator of ignored gts
   # gt_ignore_inds = np.concatenate(
    #    (np.zeros(gt_bboxes.shape[0], dtype=np.bool),
     #    np.ones(gt_bboxes_ignore.shape[0], dtype=np.bool)))
    # stack gt_bboxes and gt_bboxes_ignore for convenience
    #gt_bboxes = np.vstack((gt_bboxes, gt_bboxes_ignore))

    num_dets = det_bboxes.shape[0]
    num_gts = gt_bboxes.shape[0]
    if area_ranges is None:
        area_ranges = [(None, None)]
    num_scales = len(area_ranges)
    # tp and fp are of shape (num_scales, num_gts), each row is tp or fp of
    # a certain scale
    tp = np.zeros((num_scales, num_dets), dtype=np.float32)
    fp = np.zeros((num_scales, num_dets), dtype=np.float32)

    # if there is no gt bboxes in this image, then all det bboxes
    # within area range are false positives
    if gt_bboxes.shape[0] == 0:
        if area_ranges == [(None, None)]:
            fp[...] = 1
        else:
            det_areas = (det_bboxes[:, 2] - det_bboxes[:, 0]) * (
                det_bboxes[:, 3] - det_bboxes[:, 1])
            for i, (min_area, max_area) in enumerate(area_ranges):
                fp[i, (det_areas >= min_area) & (det_areas < max_area)] = 1
        return tp, fp

    ious = bbox_overlaps(det_bboxes, gt_bboxes)
    # for each det, the max iou with all gts
    ious_max = ious.max(axis=1)
    # for each det, which gt overlaps most with it
    ious_argmax = ious.argmax(axis=1)
    # sort all dets in descending order by scores
    sort_inds = np.argsort(-det_bboxes[:, -1])
    for k, (min_area, max_area) in enumerate(area_ranges):
        gt_covered = np.zeros(num_gts, dtype=bool)
        # if no area range is specified, gt_area_ignore is all False
        #if min_area is None:
            #gt_area_ignore = np.zeros_like(gt_ignore_inds, dtype=bool)
        if True:
            gt_areas = (gt_bboxes[:, 2] - gt_bboxes[:, 0]) * (
                gt_bboxes[:, 3] - gt_bboxes[:, 1])
            #gt_area_ignore = (gt_areas < min_area) | (gt_areas >= max_area)
        for i in sort_inds:
            if ious_max[i] >= iou_thr:
                matched_gt = ious_argmax[i]
                #if not gt_area_ignore[matched_gt]:
                if True:
                    if not gt_covered[matched_gt]:
                        gt_covered[matched_gt] = True
                        tp[k, i] = 1
                    else:
                        fp[k, i] = 1
                # otherwise ignore this detected bbox, tp = 0, fp = 0
            elif min_area is None:
                fp[k, i] = 1
            else:
                bbox = det_bboxes[i, :4]
                area = (bbox[2] - bbox[0]) * (bbox[3] - bbox[1])
                if area >= min_area and area < max_area:
                    fp[k, i] = 1
    return tp, fp