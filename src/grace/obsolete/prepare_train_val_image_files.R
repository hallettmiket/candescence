library(tidyverse)

# train and validation

all_images <- "/home/data/raw/candescence"
output <- file.path(all_images, "together")

fluor_files <- list.files(file.path(all_images, "Macrophage_FITC"), pattern=".tif", full.name=TRUE)

macro_files <- list.files(file.path(all_images, "Macrophage_Merge"), pattern=".tif", full.names=TRUE)


f_files <- str_split(fluor_files, "_")
m_files <- str_split(macro_files, "_")


for (i in 1:length(fluor_files)) {
  current <- fluor_files[i]

  macro <- file.path(all_images, "Macrophage_Merge", 
                     paste("Macrophage_Merge", f_files[[i]][4], f_files[[i]][5], 
                           f_files[[i]][6], f_files[[i]][7], f_files[[i]][8], sep="_"  ) )
  out <- file.path(output, 
                   paste("both", f_files[[i]][4], f_files[[i]][5], 
                         f_files[[i]][6], f_files[[i]][7], f_files[[i]][8], sep="_"  )
                   )
  cat("\n montage ", current, " ", macro, " -tile 2x1 -geometry +0+0 ", out)
}



all_files <- list.files(file.path(all_images, "together"), pattern=".tif", full.name=TRUE)
tmp <- str_split(all_files, pattern=".tif")

for (i in 1:length(all_files)) {
  cat("\n convert ", all_files[i], " ", paste0(tmp[[i]][1], ".bmp" ))
}
  
