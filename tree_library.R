# source this file to load all the routines and libraries
setwd("/Users/mcgrath/NOENCRYPT/PROJECTS/DATA/TREE_RING/Iconic-trees/")

source("get_tree_data.R")
source("analyze_tree_data.R")
########################################################
# These variables can be changed to match your system.

# This is the directory that stores all the downloaded data
data.dir <- "/Users/mcgrath/DATA/TREE_RING/"

# What type of data are we looking for?
data.file.type <- "crd"
#data.file.type <- "rwl"

aggregate.data.file.name <- "combined_data.csv"

########################################################
# These libraries should not be changed.
# Use some important libraries for accessing the API
library(httr)
library(jsonlite)
library(dplR) # For tree rings
# These are for producing maps
library(ggplot2)
library(ggmap)
library(maps)
########################################################


# It appears that treating strings as factors is problematic.  This string helps.
options(stringsAsFactors = FALSE)



