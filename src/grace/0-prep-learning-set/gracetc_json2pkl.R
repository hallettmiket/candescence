library(rjson)
library(reticulate)
library(tidyverse)
library(imager)

options(warn = -1)
set.seed(42)
root <- rprojroot::find_root(".git/index"); 
source(file.path(root, "src/grace/init.R"))

TARGET_FILE <- "gracetc-2021-09-24.json"
TARGET <- file.path("/home/data/refined/candescence/grace/", TARGET_FILE)
OUTPUT <- "/home/data/refined/candescence/train-data/grace_tc"
RAW_IMAGES <- "/home/data/refined/candescence/grace/grace_tc_all_images/bmp_orig_dim"
target_image_size <- 800


orig_width <- 1408
orig_height <- 1040

code <- 0:6
names(code) <- c("c0", "c1", "c2",   "time",   "unknown", "artifact", "c3" )

train_frac <- 0.8

res <- fromJSON(file=TARGET)
pkl <- convert_to_pickle_format(res, code) # default 800 x 800 conversion

num_cells_per_image <- unlist(lapply(pkl, FUN=function(x) nrow(x$ann$bboxes)))
num_train <- floor(train_frac*sum(num_cells_per_image))

tot <- 0; avail <- 1:length(pkl); train_idx<-c()
while (tot < num_train) {
  choice <- sample(avail, 1)
  avail <- setdiff(avail, choice); tot<-tot+num_cells_per_image[choice]
  train_idx <- c(train_idx, choice)
}
validation_idx <- setdiff(1:length(pkl), train_idx)
  
train <- pkl[train_idx]; validation <- pkl[validation_idx]
py_save_object(train, filename = file.path(OUTPUT, "train_gracetc.pkl"), pickle="pickle")
py_save_object(validation, filename = file.path(OUTPUT, "val_gracetc.pkl"), pickle="pickle")


# pd <- import("pandas")
# val2<- pd$read_pickle(file.path(OUTPUT, "val_gracetc.pkl"))


### Now put the images into $OUTPUT/train and $OUTPUT/val

file.remove(file.path(OUTPUT, "train", list.files(file.path(OUTPUT, "train"))))
file.remove(file.path(OUTPUT, "val", list.files(file.path(OUTPUT, "val"))))
#file.remove(file.path(OUTPUT, "test", list.files(file.path(OUTPUT, "test"))))

train_filenames <- c()
for (i in 1:length(train)) {
  im <- load.image(file.path(RAW_IMAGES, train[[i]]$filename))
  thmb <- resize(im,target_image_size, target_image_size)
  save.image(thmb, file.path(OUTPUT, "train", train[[i]]$filename))
  train_filenames <- c(train_filenames, train[[i]]$filename)
}

validation_filenames <- c()
for (i in 1:length(validation)) {
  im <- load.image(file.path(RAW_IMAGES, validation[[i]]$filename))
  thmb <- resize(im,target_image_size, target_image_size)
  save.image(thmb, file.path(OUTPUT, "val", validation[[i]]$filename))
  validation_filenames <- c(validation_filenames, validation[[i]]$filename)
}

# ---- The following code should be moved elsewhere and run everytime the learning set changes. 



all_test_files <- list.files(file.path(BASE_OUTPUT, "grace_tc_all_images", "bmp_orig_dim"))
all_test_files <- str_split(all_test_files, pattern="TC_Phase_", simplify=TRUE)[,2]
all_test_files <- str_split(all_test_files, pattern="\\.", simplify=TRUE)[,1]

train_filenames <- str_split(train_filenames, pattern="TC_Phase_", simplify=TRUE)[,2]
train_filenames <- str_split(train_filenames, pattern="\\.", simplify=TRUE)[,1]

validation_filenames <- str_split(validation_filenames, pattern="TC_Phase_", simplify=TRUE)[,2]
validation_filenames <- str_split(validation_filenames, pattern="\\.", simplify=TRUE)[,1]


#final_test_files <- setdiff(all_test_files, union(train_filenames, validation_filenames))

# for now put everything in the test for convenience
final_test_files <- all_test_files

for (i in 1:length(final_test_files)) {
  im <- load.image(file.path(RAW_IMAGES, paste0("TC_Phase_", final_test_files[i], ".bmp"))) %>% plot
  thmb <- resize(im,target_image_size, target_image_size)
  save.image(thmb, file.path(OUTPUT, "test", paste0("TC_Phase_", final_test_files[i], ".bmp")))
}



#### Here's an example of a pkl file from Candescene 1.0

# pd <- import("pandas")
# pickle_data <- pd$read_pickle("/home/data/refined/candescence/train-data/final/train_white.pkl")



