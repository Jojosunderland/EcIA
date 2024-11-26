# Results for the vertebrate field transects (including incidentals)
# species were identified to species level, present / absent data

# load packages required
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for invertebrate transect data
# for both Site A (south) and Site B (north)
vert.field <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Vert transects")

View(vert.field)

# extract only the columns i'm interested in: site and scientificName
vert.field.dat <- select(vert.field, site, scientificName)

View(vert.field.dat)
