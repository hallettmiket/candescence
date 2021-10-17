library(rjson)
library(reticulate)
library(tidyverse)
library(imager)

options(warn = -1)
set.seed(42)
root <- rprojroot::find_root(".git/index"); 
source(file.path(root, "src/grace/init.R"))

TARGET_FILE <- "grace_macro-2021-10-06.json"
TARGET <- file.path("/home/data/refined/candescence/grace/", TARGET_FILE)
OUTPUT <- "/home/data/refined/candescence/train-data/grace_macro"
RAW_IMAGES <- "/home/data/refined/candescence/grace/grace_macro_all_images/merge_bmp_orig_dim"
target_image_size <- 800

code <- 0:8
names(code) <- rev(c("ufo", "macrophage", "3",   "artifact",   "unknown", "time", "2", "1", "0" ))

train_frac <- 0.7

res <- fromJSON(file=TARGET)
pkl <- convert_to_pickle_format_macro(res, code) # default 800 x 800 conversion

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
py_save_object(train, filename = file.path(OUTPUT, "train_grace_macro.pkl"), pickle="pickle")
py_save_object(validation, filename = file.path(OUTPUT, "val_grace_macro.pkl"), pickle="pickle")


orig_width <- 1408
orig_height <- 1040

file.remove(file.path(OUTPUT, "train", list.files(file.path(OUTPUT, "train"))))
file.remove(file.path(OUTPUT, "val", list.files(file.path(OUTPUT, "val"))))
file.remove(file.path(OUTPUT, "test", list.files(file.path(OUTPUT, "test"))))

train_filenames <- c()
for (i in 1:length(train)) {
  im <- load.image(file.path(RAW_IMAGES, train[[i]]$filename))
  im2 <- imsub(im,x > orig_width) %>% plot
  thmb <- resize(im2,target_image_size, target_image_size)
  save.image(thmb, file.path(OUTPUT, "train", train[[i]]$filename))
  train_filenames <- c(train_filenames, train[[i]]$filename)
}

validation_filenames <- c()
for (i in 1:length(validation)) {
  im <- load.image(file.path(RAW_IMAGES, validation[[i]]$filename))
  im2 <- imsub(im,x > orig_width) %>% plot
  thmb <- resize(im2,target_image_size, target_image_size)
  save.image(thmb, file.path(OUTPUT, "val", validation[[i]]$filename))
  validation_filenames <- c(validation_filenames, validation[[i]]$filename)
}

# ---- The following code should be moved elsewhere and run everytime the learning set changes. 



all_test_files <- list.files(file.path(BASE_OUTPUT, "grace_macro_all_images", "merge_bmp_orig_dim"))
all_test_files <- str_split(all_test_files, pattern="both_", simplify=TRUE)[,2]
all_test_files <- str_split(all_test_files, pattern="\\.", simplify=TRUE)[,1]

train_filenames <- str_split(train_filenames, pattern="both_", simplify=TRUE)[,2]
train_filenames <- str_split(train_filenames, pattern="\\.", simplify=TRUE)[,1]

validation_filenames <- str_split(validation_filenames, pattern="both_", simplify=TRUE)[,2]
validation_filenames <- str_split(validation_filenames, pattern="\\.", simplify=TRUE)[,1]


#final_test_files <- setdiff(all_test_files, union(train_filenames, validation_filenames))

# no put all of the files in the test set for now.
final_test_files <- all_test_files

for (i in 1:length(final_test_files)) {
  im <- load.image(file.path(RAW_IMAGES, paste0("both_", final_test_files[i], ".bmp")))
  im2 <- imsub(im,x > orig_width) %>% plot
  thmb <- resize(im2,target_image_size, target_image_size)
  save.image(thmb, file.path(OUTPUT, "test", paste0("both_", final_test_files[i], ".bmp")))
}



#### Here's an example of a pkl file from Candescene 1.0

# pd <- import("pandas")
# pickle_data <- pd$read_pickle("/home/data/refined/candescence/train-data/final/train_white.pkl")




# img <-  load.image( "/home/data/refined/candescence/train-data/grace_macro/train/both_R1_P1_A1_1_00d00h00m.bmp")


