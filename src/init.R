#' Purpose: Initialize paths and global constants for candescence_new experiments (R version)
#' Author: Hallett Lab
#' Date: 2026-01-27
#' Input: None
#' Output: Environment with project configuration variables

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

# Project configuration
PROJECT_NAME <- "candescence_new"

# Repository paths. REPO_PATH defaults to the repo root via here::here(); set
# CANDESCENCE_REPO to override. (Parity with the Python settings layer in
# src/candescence/core/settings.py.)
REPO_PATH <- Sys.getenv("CANDESCENCE_REPO", here::here())
SRC_PATH <- file.path(REPO_PATH, "src")
EXP_PATH <- file.path(REPO_PATH, "exp")
DATA_PATH <- file.path(REPO_PATH, "data")
OBSOLETE_PATH <- file.path(REPO_PATH, "obsolete")

# Data storage paths. Defaults match the Hallett-lab layout; override with the
# CANDESCENCE_RAW / CANDESCENCE_REFINED environment variables.
RAW_PATH <- Sys.getenv("CANDESCENCE_RAW", "/data/lab_vm/raw/candescence_new")
REFINED_PATH <- Sys.getenv("CANDESCENCE_REFINED", "/data/lab_vm/refined/candescence_new")

# Morphology grades from original candescence
MORPHOLOGY_GRADES <- c(
  "white",
  "opaque",
  "gray",
  "shmoo",
  "pseudohyphae",
  "hyphae"
)

#' Get the refined data path for a specific experiment
#'
#' @param experiment_name Name of the experiment (e.g., "0_baseline")
#' @return Character string with full path to experiment's refined directory
#' @examples
#' get_experiment_refined_path("0_baseline")
get_experiment_refined_path <- function(experiment_name) {
  file.path(REFINED_PATH, experiment_name)
}

#' Create directory structure for a new experiment
#'
#' @param experiment_name Name of the experiment (e.g., "0_baseline")
#' @return List with exp and refined paths
#' @examples
#' create_experiment_directories("0_baseline")
create_experiment_directories <- function(experiment_name) {
  exp_dir <- file.path(EXP_PATH, experiment_name)
  refined_dir <- file.path(REFINED_PATH, experiment_name)

  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(refined_dir, recursive = TRUE, showWarnings = FALSE)

  message(sprintf("Created experiment directories: %s, %s", exp_dir, refined_dir))

  list(
    exp = exp_dir,
    refined = refined_dir
  )
}

#' Validate that essential paths exist
#'
#' @return Invisible NULL, prints warnings for missing paths
validate_paths <- function() {
  essential_paths <- c(
    REPO_PATH,
    RAW_PATH,
    REFINED_PATH
  )

  for (path in essential_paths) {
    if (!dir.exists(path)) {
      warning(sprintf("Path does not exist: %s", path))
    }
  }

  invisible(NULL)
}

# Validate paths on load
validate_paths()

message(sprintf("Loaded candescence_new configuration from: %s", SRC_PATH))
