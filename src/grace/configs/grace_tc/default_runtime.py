checkpoint_config = dict(interval=100)
# yapf:disable
log_config = dict(
    interval=5,
    hooks=[
        dict(type='TextLoggerHook'),
        dict(type='TensorboardLoggerHook')
    ])
# yapf:enable
dist_params = dict(backend='nccl')
log_level = 'INFO'
load_from = '/home/data/refined/candescence/production/models/candescence_version_1.0/model.pth'
resume_from = None
workflow = [('train', 1)]
