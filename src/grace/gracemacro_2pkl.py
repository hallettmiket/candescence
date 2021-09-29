import pandas as pd
import numpy as np
# pd <- import("pandas")
# pickle_data <- pd$read_pickle("/home/data/refined/candescence/train-data/final/train_white.pkl")

# orig = pd.read_pickle("/home/data/refined/candescence/train-data/final/train_white.pkl")
# o=orig[0]

train=pd.read_pickle("/home/data/refined/candescence/train-data/gracetc/train_gracetc.pkl")
val=pd.read_pickle("/home/data/refined/candescence/train-data/gracetc/val_gracetc.pkl")

for i in range(0,len(train)):
  tmp=np.zeros( (len(train[i]['ann']['bboxes']), 4), dtype='float32')
  tmp[:,:]= train[i]['ann']['bboxes'][:,:]
  train[i]['ann']['bboxes']=tmp
  train[i]['ann']['labels']=train[i]['ann']['labels'].astype(np.int64)


for i in range(0,len(val)):
  tmp=np.zeros( (len(val[i]['ann']['bboxes']), 4), dtype='float32')
  tmp[:,:]= val[i]['ann']['bboxes'][:,:]
  val[i]['ann']['bboxes']=tmp
  val[i]['ann']['labels']=val[i]['ann']['labels'].astype(np.int64)


import pickle
pickle.dump(train, open("/home/data/refined/candescence/train-data/grace_macro/train_grace_macro.pkl", "wb"))  # save it into a file named save.p
pickle.dump(val, open("/home/data/refined/candescence/train-data/grace_macro/val_grace_macro.pkl", "wb"))  # save it into a file named save.p
