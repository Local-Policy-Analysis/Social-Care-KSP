###############################################################################
##                              *SET UP*                                 ##
###############################################################################
# Author:     Becky Foster
# Script Description: Project set up for the KSP project
# -----------------------------------------------------------------------------

# SETUP ----
# Housekeeping
  cat("\014")                 # Clears the console
  rm(list = ls())             # Remove all variables of the work space

# Working directory 
  wd <- "~/GitHub/Social-Care-KSP/"
  setwd(wd)
  cat("WORKING DIRECTORY HAS BEEN SET TO: ", wd, sep = "")
  
#### PACKAGES ----
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse", "stringr", "readxl", "ggplot2", "generics", "tidyr", "vctrs", "withr", "pacman", "purrr", "readODS", "ggmap", "scales", "plotly", "openxlsx", "reshape2", "datadictionary", "cowplot", "rvest", "xml2") # list of packages to load
n_packages <- length(packages) # count how many packages are required

new.pkg <- packages[!(packages %in% installed.packages())] # determine which packages aren't installed

install.packages("sf", type = "source", configure.args = "--with-proj-lib=$(brew --prefix)/lib/")

# install missing packages
if(length(new.pkg)){
  install.packages(new.pkg)
}

# load all required libraries
for(n in 1:n_packages){
cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
eval(parse(text = lib_load)) # evaluate the string to load the library
}
library(sf)

#### COLOURS ----
# These are rough colour copies from LPA powerpoint templates
chart_palette <- c(
  "#00625E", # mhclg teal
  "#932A72", # Pink
  "#85292A", # Red
  "#BF4A1D", # Orange
  "#40611F", # Green
  "#205083", # Blue
  "#333366", # Indigo
  "#535453", # Grey
  "#FF5D88", # Bright Pink
  "#DC3230", # Bright Red
  "#FAA332", # Bright Orange
  "#98B83C", # Bright Green
  "#08B2D5", # Bright Blue
  "#8687C1", # Bright Indigo
  "#99988F"  # Bright Grey
)  

# ------------------------------------------------------------------------
