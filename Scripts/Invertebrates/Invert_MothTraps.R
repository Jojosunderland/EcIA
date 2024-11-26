# Results for the Invertebrate moth trap data
# Species were identified to the order level, abundance data

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for invertebrate moth trap data
# for both Site A (south) and Site B (north)
invert.moth <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Moth traps")

View(invert.field)
