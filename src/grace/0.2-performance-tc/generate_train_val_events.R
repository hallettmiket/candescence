options(warn = -1)
root <- rprojroot::find_root(".git/index"); source(file.path(root, "src", "grace", "init.R"))

numba = "results_big_momentum"   # this was our best one
exp = "big_momentum" 
target = "val"   # can be {val | train}
threshold = 0.3   #  , 0.3, 0.33, 0.35, 0.4, 0.5, 0.6]

CANDESCENCE="/home/data/refined/candescence"
PARENT_RAW_OUTPUT=  file.path(CANDESCENCE, "output/grace_tc")
SAVE_RESULTS=       file.path(CANDESCENCE, "performance/grace_tc", paste0("exp_", exp))
IMAGE_PATH=         file.path(CANDESCENCE, "train-data/gracetc", target)
DATASET=            file.path(CANDESCENCE, "train-data/gracetc", target)

code_name <- paste0(exp, "_type_", target, "_thresh_", threshold)

# # which folder to apply tool to
# output_dir <- file.path(CANDESCENCE, "output", exp )
# performance_save = file.path(CANDESCENCE, "performance", paste0(exp, "_results" ))


# Read in 
all_events_fname<-file.path(SAVE_RESULTS, paste0("all_events_results_", code_name, ".csv"))
events_ <- read_csv( all_events_fname ); events_ <- events_[,-1]

# clean up
events_[['short_filename']] <- short_filename( events_[['filename']])
events_ <- events_ %>% relocate( short_filename ) %>% 
  arrange( short_filename, bbox_1, bbox_2, bbox_3, bbox_4 ) %>% 
  distinct( short_filename, threshold, bbox_1, bbox_2, bbox_3, bbox_4, .keep_all = TRUE)

# final
events <- events_ %>% filter( threshold == threshold )

# ----> Get the confusion matrices <-------


cat("\n\nConfusion matrix: ")

e_confusion <- events %>% filter( event=="class_good" | event =="class_error") %>%
    select( gt_class, dt_class ) %>% rename( target = gt_class, prediction = dt_class ) 

c_o <- rev(c("c0", "c1", "c2", "time", "unknown", "artifact", "c3"))
c_idx <- c_o %in% unique(union(e_confusion[['target']], e_confusion[['prediction']]))
c_o_p <- c_o[c_idx]

confusion <- confusion_matrix(targets = e_confusion$target,  predictions = e_confusion$prediction)

out <- plot_confusion_matrix(confusion, 
                               add_sums = TRUE, 
                               rotate_y_text = FALSE,
                               place_x_axis_above = TRUE,
                               # target_col = "Ground Truth",
                               class_order = c_o_p,
                               font_counts = font(
                                 size = 3,  color = "black"
                               ),
                               font_normalized = font(
                                 size = 2,  color = "black"),
                               font_row_percentages = font(
                                 size = 1,  color = "black"),
                               font_col_percentages = font(
                                 size = 1,  color = "black"),
                               add_counts = TRUE,
                               add_arrows=FALSE,
                               add_normalized = FALSE,
                               add_row_percentages = FALSE,
                               add_col_percentages = FALSE,
                               diag_percentages_only = FALSE,
                               tile_border_color = "gray",
                               sums_settings = sum_tile_settings(
                                 palette = "Oranges",
                                 label = "Total",
                                 tc_tile_border_color = "black"
                               ))
  out + ggplot2::labs(x="Ground Truth", y = "Prediction")
  
  ggsave( out, file = file.path(SAVE_RESULTS, paste0("confusion_", code_name, ".png" )))
  

# <-------- isolate all hallucinations.

  upper_bound <- threshold
  
  v_blank <- grayscale(readRDS(file.path(root, "src", "grace", "performance", "data/v_blank.png")))

  e_hallucinations <- events %>% filter( event == "hallucination" ) %>% arrange( dt_class ) %>%
           make_unique_by_iou( upper_bound) 
  
  cat("\nThe total number of unique hallucinations: ", nrow(e_hallucinations))
  cat("\nNumber of hallcuinations per morphology: ")
  e4 <- e_hallucinations %>% group_by( dt_class ) %>% summarise(n = n())
  print(e4 )
  N_hallucinations <- nrow(e_hallucinations)
  
  e_hallucinations <- e_hallucinations %>% arrange( dt_class ) %>% relocate( dt_class)
  klasses <- unique(e_hallucinations[['dt_class']])
  for (i in 1:length(klasses)) {
    
    current <- e_hallucinations %>% filter( dt_class == klasses[i])
    clip_r <- list()
    
    for (j in 1:nrow(current)) {
      print(current[["short_filename"]][j])
      try(
        img <-  load.image( file.path( DATASET, current[["short_filename"]][j]) ) 
      )
    
      # 1,3,0,2 -> 2, 4, 1, 3
      bbox_1 <- current[['bbox_1']][j] 
      bbox_2 <- current[['bbox_2']][j] 
      bbox_3 <- current[['bbox_3']][j]  
      bbox_4 <- current[['bbox_4']][j]  
      clip <- imsub( img, x %inr% c(bbox_1, bbox_3), y %inr% c(bbox_2, bbox_4)  )
      clippy <- resize(clip, size_x = 50, size_y = 50)
      clip_r <- list.append(clip_r, clippy )
      clip_r <- list.append(clip_r, v_blank )
      
    }
    
    l <- length(clip_r); per_line <- 26
    num_rows <- ceiling(l / per_line)
    
    for (k in 1:num_rows) {
      lower <- ((k-1)*per_line +1);  upper <- ifelse( k < num_rows, (k*per_line), l )
      fin <- imager::imappend(clip_r[lower:upper], "x")
      fin <- grayscale(fin)
      # imager::save.image(fin, file.path(performance_save, paste0("hallucinations_", numba, "_thresh_", thresh, "_class_", klasses[i], "_row_", k, ".png" )))
      imager::save.image(fin, 
              file.path(SAVE_RESULTS,  paste0("hallucinations_", code_name, "_row_name_", k, ".png" )))
      
          }      
    plot(fin)

  }
  
  
  
  # <-------- isolate all blindspots
  
  upper_bound <- threshold
  
  v_blank <- grayscale(readRDS(file.path(root, "src", "grace", "performance", "data/v_blank.png")))

  e_blindspots <- events %>% filter( event == "blindspot" ) %>% arrange( gt_class ) %>%
    make_unique_by_iou( upper_bound) 
  
  cat("\nThe total number of unique blindspots: ", nrow(e_blindspots))
  cat("\nNumber of blindspots per morphology: ")
  e4 <- e_blindspots %>% group_by( gt_class ) %>% summarise(n = n())
  print(e4 )
  N_blindspots <- nrow(e_blindspots)
  
  e_blindspots <- e_blindspots %>% arrange( gt_class ) %>% relocate( gt_class)
  klasses <- unique(e_blindspots[['gt_class']])
  for (i in 1:length(klasses)) {
    
    current <- e_blindspots %>% filter( gt_class == klasses[i])
    clip_r <- list()
    
    for (j in 1:nrow(current)) {
      print(current[["short_filename"]][j])
      try(
        img <-  load.image( file.path( DATASET, current[["short_filename"]][j]) ) 
      )
      
      # 1,3,0,2 -> 2, 4, 1, 3
      bbox_1 <- current[['bbox_1']][j] 
      bbox_2 <- current[['bbox_2']][j] 
      bbox_3 <- current[['bbox_3']][j]  
      bbox_4 <- current[['bbox_4']][j]  
      clip <- imsub( img, x %inr% c(bbox_1, bbox_3), y %inr% c(bbox_2, bbox_4)  )
      clippy <- resize(clip, size_x = 50, size_y = 50)
      clip_r <- list.append(clip_r, clippy )
      clip_r <- list.append(clip_r, v_blank )
      
    }
    
    l <- length(clip_r); per_line <- 26
    num_rows <- ceiling(l / per_line)
    
    for (k in 1:num_rows) {
      lower <- ((k-1)*per_line +1);  upper <- ifelse( k < num_rows, (k*per_line), l )
      fin <- imappend(clip_r[lower:upper], "x")
      fin <- grayscale(fin)
      imager::save.image(fin, 
                         file.path(SAVE_RESULTS,  
                                   paste0("blindspots_", code_name, "_row_name_", k, ".png" )))
    }      
    plot(fin)
  }
  
  
  
  sink( file.path(SAVE_RESULTS, paste0( "stat_summary_", code_name, ".txt") ) )
  
  
  cat(paste0("\n\n\n------------\n\n"))
  cat(paste0("\nNumber of unique files: ", length(unique(events[['short_filename']])) ))
  
  e33 <- events %>% filter( event != "hallucination" )
  cat(paste0("\nNumber of non-hallucinations: ", nrow(e33) ))
  e333 <- e33 %>% group_by( gt_class ) %>% summarise( n= n())
  cat(paste0("\nNumber of non-hallucinations by ground truth: "))
  print(e333)
  N_total <- nrow(e33)
  
  e3333 <- e33 %>% filter( gt_class != "Artifact", gt_class != "Unknown" )
  cat(paste0("\nNumber of C. albicans cells: ", sum(e3333[['n']])))
  
  

  
  e_good <- events %>% filter( event == "class_good")
  cat(paste0("\nNumber of good classifications: ", e_good %>% nrow))
  cat(paste0("\nNumber of good classifications per morphology: "))
  e5 <- e_good %>% group_by( gt_class ) %>% summarise(n = n())
  print(e5 )
  
  
  e_bad <- events %>% filter( event == "class_error")
  cat(paste0("\nNumber of classification errors: ", e_bad %>% nrow))
  cat(paste0("\nNumber of classification errors per morphology: "))
  e6 <- e_bad %>% group_by( gt_class ) %>% summarise(n = n())
  print(e6 )
  N_errors <- e_bad %>% nrow
  
  
  N_TPs <- e_good %>% nrow
  N_FPs <- N_errors + N_hallucinations
  N_FNs <- N_blindspots
  
  
  cat(paste0("\nObject Detection"))
  cat(paste0("\n\tFalse Negatives/Blindspots. Number of blindspots: ", N_blindspots,  
      " Total trials: ", N_total, 
      " Rate: ", N_blindspots / N_total))
  
  cat(paste0("\n\tFalse Positives/Hallcuinations. Number of hallucinations:", N_hallucinations,
      " Total trials: ", N_total, 
      " Rate: ", N_hallucinations / N_total    ))
  
  cat(paste0("\nClassification accuracy. ", 
      " Total trials: ",  N_TPs + N_errors,
      " Total errors: ", N_errors, 
      " Total correct: ", N_TPs,
      " Rate: ", N_TPs / (N_TPs + N_errors))
  )
  
  
  cat(paste0("\nOverall: "))
  cat(paste0("\n\tSensitivity/Recall: ", N_TPs / (N_TPs + N_FNs), "\tPrecision: ", 
                    N_TPs / (N_TPs + N_FPs), "\tF1: ", 2*N_TPs / ((2*N_TPs + N_FPs + N_FNs))))
  
  
  sink()
  
  
  
  #### Not sure about this:
  
  
  ## global view of blindspots
  library(Cairo)
  tmp <- e_blindspots %>% group_by(short_filename) %>% summarise(n = n()) %>% relocate(n)
  targets <- c( "1604GOF_stained_w_CWF_overnight_30_degrees_.bmp",
                "Julyb9b019_Sc5314control_after_5hrs_in_10_percent_serum_75_rpm_starting_OD_0_5_in_5ml_CFW_stain_40x_6.bmp",
                "Sep32019_5314_strain__in_20_percent_serum_after_3hrs_starting_Od600_0_1_in_1_ml_37__no_shaking_CWf_stained__10.bmp")
  
  # you have to do each file manually
  target <- xxx[8]
  current <- e_blindspots %>% filter( short_filename == target)
  img <-  load.image( file.path( new_image_dir, target) ) 
  
  px <- list(); kls <- c()
  txt <- list()
  for (j in 1:nrow(current)) {
    bbox_1 <- current[['bbox_1']][j] 
    bbox_2 <- current[['bbox_2']][j] 
    bbox_3 <- current[['bbox_3']][j]   
    bbox_4 <- current[['bbox_4']][j]  
    
    txt[[j]] <- c(bbox_1, bbox_2-15)
    kls[j] <- current[['gt_class']][j]
    px[[j]] <- (Xc(img) %inr% c(bbox_1, bbox_3)) & (Yc(img) %inr% c(bbox_2, bbox_4))
    
  }
  for (j in 1:length(px)) {
    img <- imager::implot(img, { text(txt[[j]][1], txt[[j]][2], kls[j], cex=2, col="red") })
  }
  
  plot(img)
  for (j in 1:length(px)) {
    # img <- imager::implot(img, { text(txt[[j]][1], txt[[j]][2], kls[j], cex=1, col="red") })
    highlight(px[[j]])
  }
  ###



