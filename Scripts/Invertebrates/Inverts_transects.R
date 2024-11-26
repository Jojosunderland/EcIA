# Results for the Invertebrate field data (transects)
# Species were identified to the order level, abundance data

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for invertebrate transect data
# for both Site A (south) and Site B (north)
invert.field <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Invert transects")

View(invert.field)