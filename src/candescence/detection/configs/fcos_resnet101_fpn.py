"""
Purpose: FCOS detector configuration for Candida morphology detection
Author: Hallett Lab
Date: 2026-01-28
Input: Configuration parameters
Output: MMDETECTION config dict

FCOS (Fully Convolutional One-Stage) detector with ResNet-101 backbone
and Feature Pyramid Network (FPN) neck. Configured for 15 morphology classes.
"""

# Model configuration
model = dict(
    type='FCOS',
    pretrained='open-mmlab://detectron/resnet101_caffe',
    backbone=dict(
        type='ResNet',
        depth=101,
        num_stages=4,
        out_indices=(0, 1, 2, 3),
        frozen_stages=1,
        norm_cfg=dict(type='BN', requires_grad=False),
        norm_eval=True,
        style='caffe'
    ),
    neck=dict(
        type='FPN',
        in_channels=[256, 512, 1024, 2048],
        out_channels=256,
        start_level=1,
        add_extra_convs=True,
        extra_convs_on_inputs=False,
        num_outs=5,
        relu_before_extra_convs=True
    ),
    bbox_head=dict(
        type='FCOSHead',
        num_classes=15,
        in_channels=256,
        stacked_convs=4,
        feat_channels=256,
        strides=[8, 16, 32, 64, 128],
        loss_cls=dict(
            type='FocalLoss',
            use_sigmoid=True,
            gamma=2.0,
            alpha=0.25,
            loss_weight=1.0
        ),
        loss_bbox=dict(type='IoULoss', loss_weight=1.0),
        loss_centerness=dict(
            type='CrossEntropyLoss',
            use_sigmoid=True,
            loss_weight=1.0
        )
    )
)

# Training configuration
train_cfg = dict(
    assigner=dict(
        type='MaxIoUAssigner',
        pos_iou_thr=0.4,
        neg_iou_thr=0.1,
        min_pos_iou=0,
        ignore_iof_thr=-1
    ),
    allowed_border=-1,
    pos_weight=-1,
    debug=False
)

# Test configuration
test_cfg = dict(
    nms_pre=1000,
    min_bbox_size=0,
    score_thr=0.05,
    nms=dict(type='nms', iou_threshold=0.5),
    max_per_img=275
)

# Image normalization (ImageNet mean, no std normalization)
img_norm_cfg = dict(
    mean=[102.9801, 115.9465, 122.7717],
    std=[1.0, 1.0, 1.0],
    to_rgb=False
)

# Training pipeline
train_pipeline = [
    dict(type='LoadImageFromFile'),
    dict(type='LoadAnnotations', with_bbox=True),
    dict(type='Resize', img_scale=(800, 800), keep_ratio=True),
    dict(type='RandomFlip', flip_ratio=0.5),
    dict(type='Normalize', **img_norm_cfg),
    dict(type='Pad', size_divisor=32),
    dict(type='DefaultFormatBundle'),
    dict(type='Collect', keys=['img', 'gt_bboxes', 'gt_labels'])
]

# Test pipeline
test_pipeline = [
    dict(type='LoadImageFromFile'),
    dict(
        type='MultiScaleFlipAug',
        img_scale=(800, 800),
        flip=False,
        transforms=[
            dict(type='Resize', keep_ratio=True),
            dict(type='RandomFlip'),
            dict(type='Normalize', **img_norm_cfg),
            dict(type='Pad', size_divisor=32),
            dict(type='ImageToTensor', keys=['img']),
            dict(type='Collect', keys=['img'])
        ]
    )
]

# Optimizer
optimizer = dict(
    type='SGD',
    lr=0.01,
    momentum=0.9,
    weight_decay=0.001,
    paramwise_cfg=dict(bias_lr_mult=2.0, bias_decay_mult=0.0)
)
optimizer_config = dict(grad_clip=dict(max_norm=35, norm_type=2))

# Learning rate schedule
lr_config = dict(
    policy='step',
    warmup='constant',
    warmup_iters=1000,
    warmup_ratio=1.0 / 3.0,
    step=[8, 11]
)

# Runtime settings
total_epochs = 4800
checkpoint_config = dict(interval=300)
log_config = dict(
    interval=15,
    hooks=[
        dict(type='TextLoggerHook'),
        dict(type='TensorboardLoggerHook')
    ]
)
dist_params = dict(backend='nccl')
log_level = 'INFO'
workflow = [('train', 1)]
evaluation = dict(interval=1, metric='mAP')
