
_base_=[ 'config_1_nopre.py' ]

total_epochs = 1000

optimizer = dict(lr=0.001, momentum=0.9, weight_decay=0.001, paramwise_cfg=dict(bias_lr_mult=2., bias_decay_mult=0.))

load_from = None
