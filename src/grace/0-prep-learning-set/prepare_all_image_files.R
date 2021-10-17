library(tidyverse)

# --- This prepares the raw image files that are located in /home/data/raw/candeecence/full_set_raw for use with the FCOS. 
#     The processed files are moved to $BASE_OUTPUT defined below.

# ------  First we prepare the Macrophage related files. 
# ------      This involves montaging the flourescnet with the macrophage together and converting to bmp.

INPUT <- "/home/data/raw/candescence/full_set_raw"
BASE_OUTPUT <- "/home/data/refined/candescence/grace"

fluor_files <- list.files(file.path(INPUT, "Macrophage_FITC"), pattern=".tif", full.name=TRUE)
macro_files <- list.files(file.path(INPUT, "Macrophage_Merge"), pattern=".tif", full.names=TRUE)

f_files <- str_split(fluor_files, "_")
m_files <- str_split(macro_files, "_")

sink(file="montage.bash")
for (i in 1:length(fluor_files)) {
  current <- fluor_files[i]
  
  macro <- file.path(INPUT, "Macrophage_Merge", 
                     paste("Macrophage_Merge", f_files[[i]][6], f_files[[i]][7], 
                           f_files[[i]][8], f_files[[i]][9], f_files[[i]][10], sep="_"  ) )
  out <- file.path(BASE_OUTPUT,  "grace_macro_all_images", "merge_tif_orig_dim",
                   paste("both", f_files[[i]][6], f_files[[i]][7], 
                         f_files[[i]][8], f_files[[i]][9], f_files[[i]][10], sep="_"  )
  )
  cat("\n montage ", current, " ", macro, " -tile 2x1 -geometry +0+0 ", out)
}
sink()


all_files <- list.files(file.path(BASE_OUTPUT,  "grace_macro_all_images", "merge_tif_orig_dim"), pattern=".tif", full.name=FALSE)
tmp <- str_split(all_files, pattern=".tif")

sink(file="convert_macro.bash")
for (i in 1:length(all_files)) {
  cat("\n convert ", file.path(BASE_OUTPUT, "merge_tif_orig_dim", all_files[i]), " ", file.path(BASE_OUTPUT, "test", "merge_bmp_orig_dim", paste0(tmp[[i]][1], ".bmp" )))
}
sink()

# ------  Second we prepare the TC related files which are just DIC images of C. albicans without macrophages. 


tc_files_raw <- list.files(file.path(INPUT, "TC_Phase"), pattern=".tif", full.name=TRUE)
tc_files <- str_split(tc_files_raw, "/", simplify=TRUE)[,8]
sans_tif_tc_files <- str_split(tc_files, "\\.", simplify=TRUE)[,1]

sink(file="convert_tc.bash")
for (i in 1:length(tc_files_raw)) {
  cat("\n convert ", tc_files_raw[i], " ", file.path(BASE_OUTPUT, "grace_tc_all_images", "bmp_orig_dim", paste0(sans_tif_tc_files[i], ".bmp" )))
}
sink()



