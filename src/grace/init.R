# BiocManager::install("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
library(reticulate); library(tidyverse); library(ggpubr)
library(cvms)
library(broom)    # tidy()
library(rlist); library(feather); library(imager)
library("readxl")


set.seed(1)
CANDESCENCE="/home/data/refined/candescence"
GRACE=file.path(CANDESCENCE, "grace")

grace <- readRDS(file.path(GRACE, "grace_library_annotations.rds"))


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

reformat_grace_annotations <- function( ) {
  
  raw <-  read_excel(file.path(GRACE, "grace_library_annotations.xlsx")) 
  raw <- raw %>%
      relocate( `Plate`, Position, `orf19 name`, Common, `Feature Name`, `Description`, 
                `Replicate 1 Macrophages`, `Replicate 1 TC conditions...36`, 
                `Replicate 2 Macrophages`, `Replicate 1 TC conditions...39`,
                `S.cerevisiae homologue`, `S. cerevisiae KO phenotype` )
  raw <- raw %>%
      rename( plate=`Plate`, position=Position, orf=`orf19 name`, common=Common, feature_name=`Feature Name`, 
              description=`Description`, 
              rep1_macro=`Replicate 1 Macrophages`, rep1_TC_36=`Replicate 1 TC conditions...36`, 
              rep2_macro=`Replicate 2 Macrophages`, rep1_TC_39=`Replicate 1 TC conditions...39`,
              sc_homologue=`S.cerevisiae homologue`, sc_ko_pheno=`S. cerevisiae KO phenotype` ) 
  
  raw$plate <- as.integer(str_split(raw$plate, pattern="Plate ", simplify=TRUE)[,2])
  raw <- raw %>% separate( col=position, into=c("row", "column"), sep=1 )
  raw$column <- as.character(as.integer(raw$column))
  
  grace <- raw
  saveRDS(grace, file.path(GRACE, "grace_library_annotations.rds"))
}

calculate_area_via_diagnonal <- function( t,l, b, r, width=10 ) {
  adj <- sqrt( width^2 / 2 )  
  if ((b-t) < width) 
    if ((r-l) < width) return( (b-t)*(r-l) )  else return( width * (r-l) )
        
  if ((r-l) < width) return( width * (b-t) )
  
  tri <- ((b-t-adj) * (r-l-adj))/2
  return( ((b-t) * (r-l)) - 2*(tri) )
}





