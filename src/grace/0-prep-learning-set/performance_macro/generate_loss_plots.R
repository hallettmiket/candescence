#!/usr/bin/env Rscript

#Rscript --vanilla 

options(warn = -1)
root <- rprojroot::find_root(".git/index"); 
source(file.path(root, "src/universal/code.R"))


tool <- "./analyze_logs.py"    # which tool to use?
target_dir <-  "/home/data/refined/candescence/output/exp_0" #  point this to the output from src/3-curriculum
save_results <- "/home/data/refined/candescence/performance" # expX/loss will be created for each ir in output_dir


# ----> Generate plot curves  <-------

files <- list.files( target_dir )

for (f in files) {
  cat("\n Experiment number: ", f)
  if (!dir.exists(file.path(save_results,  paste0(f, "_results"))))  success <- dir.create(file.path(save_results,  paste0(f, "_results")), showWarnings = FALSE)

  ff <- list.files(file.path(target_dir,  f))
  for (grade in grades) {
    if (! grade %in% ff) { next }
    target <- list.files( file.path(target_dir,  f, grade), pattern = "*.json")

    if (length(target) > 0) {
      target <- target[length(target)]
      system( capture.output(
        cat( "python ",
             #file.path( MMDETECTION, tool ), " plot_curve ",
             tool, " plot_curve ",
             file.path(target_dir, f, target),
             " --keys loss_cls loss_bbox loss_centerness loss ",
             " --legend loss_cls  loss_bbox  loss_centerness loss",
             " --out ", file.path(save_results, paste0(f, "_results"), paste0("loss_", grade)))
      ))
      } # end of if
    } #end of grade
} # end of f


# ----> read plot curves into R  <-------

all_dirs <- list.dirs( save_results, full.names = FALSE, recursive = FALSE );
if ("obsolete" %in% all_dirs) all_dirs <- setdiff(all_dirs, "obsolete")

master <- tibble()
for (i in seq_along(all_dirs)) {
  
  target <- file.path( save_results, all_dirs[i] );
  files <- list.files( target, pattern = "*.pkl" )
  grade <- unlist(lapply(str_split(unlist(lapply( str_split( files , pattern= "\\."), `[[`, 1)), pattern="_"), `[[`, 2))
  curve <- unlist(lapply(str_split(unlist(lapply( str_split( files , pattern= "\\."), `[[`, 1)), pattern="_"), `[[`, 3))
  
  pd <- import("pandas")
  for (f in seq_along(files)) {
    pth <- file.path( target, files[f] )
    dta <- pd$read_pickle( pth )
    master <- bind_rows(master, tibble( exp = all_dirs[i], grade = grade[f], curve = curve[f], x=dta[[1]], y = dta[[2]]))
  }
  
} # end of for i
master$curve <- master$curve %>% recode( "0" = "loss_cls", "1" = "loss_bbs", "2"= "loss_centerness", "3" = "loss")


# ----> ggplot the curves <-------

exps <- unique( master$exp )
for (i in seq_along(exps)){
  print(i)
  print(exps[i])
  gg <- list()
  m <- master %>% filter( exp== exps[i] )
  gradess <- unique( m$grade )
  
  new_grades <- grades[ which(gradess %in% grades) ] #reorders them according to grades 1-6.
  for (j in seq_along(new_grades)) {
    mm <- m %>% filter( grade == new_grades[j])
    tmp <- mm %>% filter(curve == "loss_cls") %>% summarise( min_loss_cls = min(y, na.rm = T)); min_loss_cls <- tmp$min_loss_cls
    tmp <- mm %>% filter(curve == "loss_bbs") %>% summarise( min_loss_bbs = min(y, na.rm = T)); min_loss_bbs <- tmp$min_loss_bbs
    tmp <- mm %>% filter(curve == "loss_centerness") %>% summarise( min_loss_centerness = min(y, na.rm = T)); min_loss_centerness <- tmp$min_loss_centerness
    tmp <- mm %>% filter(curve == "loss") %>% summarise( min_loss = min(y, na.rm = T)); min_loss <- tmp$min_loss
    
    gg[[length(gg)+1]] <- ggplot(data=mm, aes(x=x, y = y, color = curve)) +
      geom_point(size=1, shape=1, alpha = 1/20) +
      geom_smooth() +
      scale_y_continuous(limits=c(0, 1.5), breaks=seq(0,2,0.1)) +
      geom_hline( yintercept = min_loss_cls, color = "purple"  ) +
      geom_hline( yintercept = min_loss_bbs, color = "green"  ) +
      geom_hline( yintercept = min_loss_centerness, color = "blue"  ) +
      geom_hline( yintercept = min_loss, color = "red"  ) +
      labs(title= paste( exps[i], " ", new_grades[j]), x="Iteration", y = "Loss")
    # theme_classic()
  }
  
  ggarrange( plotlist = gg, ncol = 2, legend = "top", common.legend = TRUE)  %>% 
        ggexport(filename = file.path( save_results, exps[i], "summary_loss.png" ))
}

