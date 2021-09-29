# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
library(reticulate); library(tidyverse); library(ggpubr)
library(cvms)
library(broom)    # tidy()
library(rlist); library(feather); library(imager)

set.seed(1)



short_filename <- function( fnames ) {
  f <- str_split( fnames, pattern = "/" )
  return( unlist( lapply( f , "[[", length(f[[1]]) )) )  }

scale_bboxes <- function( bboxes, 
                          orig_height =  1040, orig_width = 1408,
                          target_height=800, target_width =800) {
  
  b <- apply(bboxes, MARGIN=1, FUN=function(x) 
    c( 
      max(1, floor(x[1]/orig_width *target_width)),
      max(1, floor(x[2]/orig_height*target_height)), 
      floor(x[3]/orig_width *target_width),
      floor(x[4]/orig_height*target_height)))
  return(t(b))
}


convert_to_pickle_format <- function(res, code) {
  output <- list()
  for (i in 1:length(res)) {
    nd <- length(output) + 1
    current <- res[[i]]
    
    tmp <- list()
    tmp$filename <- current$`External ID`
    tmp$width <- 1408
    tmp$height <- 1040
    
    obj <- current[4][[1]][[1]]
    
    labels <- c()
    bboxes <- matrix(0, nrow = length(obj), ncol = 4)
    for (j in 1:length(obj)) {
      kurrent <- obj[[j]]
      labels[j] <- as.numeric(code[kurrent$value])
      bboxes[j, 1] <- as.numeric(kurrent$bbox$left)
      bboxes[j, 3] <- as.numeric(kurrent$bbox$left + kurrent$bbox$width)
      # bboxes[j, 2] <-
      #   as.numeric(tmp$height - (kurrent$bbox$top + kurrent$bbox$height))
      # bboxes[j, 4] <- as.numeric(tmp$height - kurrent$bbox$top)
      bboxes[j, 2] <- as.numeric(kurrent$bbox$top)
      bboxes[j, 4] <- as.numeric(kurrent$bbox$top + kurrent$bbox$height)
    }
    tmp$ann <- list()
    
    tmp$ann$bboxes <- scale_bboxes(bboxes)
    tmp$width <- 800
    tmp$height <- 800
    tmp$width <- as.integer(tmp$width)
    tmp$height <- as.integer(tmp$height)
    
    tmp$ann$labels <- as.array(labels)
    output[[nd]] <- tmp
  } # end of for i
  return(output)
}




convert_to_pickle_format_macro <- function(res, code) {
  output <- list()
  for (i in 1:length(res)) {
    nd <- length(output) + 1
    current <- res[[i]]
    
    tmp <- list()
    tmp$filename <- current$`External ID`
    tmp$width <- 1408
    tmp$height <- 1040
    
    obj <- current[4][[1]][[1]]
    
    labels <- c()
    bboxes <- matrix(0, nrow = length(obj), ncol = 4)
    for (j in 1:length(obj)) {
      kurrent <- obj[[j]]
      labels[j] <- as.numeric(code[kurrent$value])
      bboxes[j, 1] <- as.numeric(kurrent$bbox$left) - tmp$width
      bboxes[j, 3] <- as.numeric(kurrent$bbox$left + kurrent$bbox$width) - tmp$width
      # bboxes[j, 2] <-
      #   as.numeric(tmp$height - (kurrent$bbox$top + kurrent$bbox$height))
      # bboxes[j, 4] <- as.numeric(tmp$height - kurrent$bbox$top)
      bboxes[j, 2] <- as.numeric(kurrent$bbox$top)
      bboxes[j, 4] <- as.numeric(kurrent$bbox$top + kurrent$bbox$height)
    }
    tmp$ann <- list()
    
    tmp$ann$bboxes <- scale_bboxes(bboxes)
    tmp$width <- 800
    tmp$height <- 800
    tmp$width <- as.integer(tmp$width)
    tmp$height <- as.integer(tmp$height)
    
    tmp$ann$labels <- as.array(labels)
    output[[nd]] <- tmp
  } # end of for i
  return(output)
}


make_unique_by_iou <- function( hallucin, upper_bound ){
  
  all_files <- unique(hallucin[["short_filename"]])
  final <- hallucin[-c(1:nrow(hallucin)),]
  
  for (i in 1:length(all_files)) {
    current_file <- all_files[i]
    hall <- hallucin %>% filter( short_filename == current_file)
    if (nrow(hall) < 2) { 
      final <- bind_rows(final, hall)
      next
    }
    
    ious <- matrix( nrow=nrow(hall), ncol = nrow(hall), data = 0)
    
    for (j in 1:(nrow(hall)-1)) {
      for (k in (j+1):nrow(hall)) {
        
        A <- c( hall[["bbox_1"]][j], hall[["bbox_2"]][j], hall[["bbox_3"]][j], hall[["bbox_4"]][j] )
        B <- c( hall[["bbox_1"]][k], hall[["bbox_2"]][k], hall[["bbox_3"]][k], hall[["bbox_4"]][k] )
        
        # x-dimension   
        xl <- max( A[1], B[1] )
        xr <- min( A[3], B[3] )
        if (xr <= xl) next
        
        yh <-min( A[2], B[2])
        yl <- max( A[4], B[4])
        if (yh >= yl) next
        
        num <- (xr - xl) * (yl - yh)
        denom <- num + ( (A[3]- A[1]) * (A[4]-A[2]) ) + ( (B[3]-B[1]) * (B[4]-B[4]) )
        
        ious[j, k] <-   num / denom
      } # end of k
    } # end of j
    
    to_remove <- c()
    while (max(ious) > upper_bound) {
      loc <- which(ious == max(ious), arr.ind = TRUE)
      to_remove <- c(to_remove, loc[1])
      ious[loc[1], ] <- 0
      ious[ , loc[1]] <- 0
    }
    
    if (length(to_remove) > 0) print( hall[to_remove, ] )
    
    ifelse(length(to_remove) > 0, final <- bind_rows(final, hall[-to_remove,]), final <- bind_rows(final, hall))
    
  }
  return(final)
}






