## Results for the vertebrate tech group (audio and camera)
# species were identified to species level, present / absent data

# load packages required
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for vertebrate tech data
# for both Site A (south) and Site B (north)
vert.tech <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Vert tech")

View(vert.tech)
