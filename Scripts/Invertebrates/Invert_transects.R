# Results for the Invertebrate field data (transects)
# Species were identified to the order level

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')

# read in appropriate data sheet for invertebrate transect data
# for both Site A (south) and Site B (north)
invert.dat.field <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Invert transects")

View(invert.dat.field)

# remove any rows that have NAs for the order
invert.data.field <- invert.dat.field[-which(is.na(invert.dat.field$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.dat.field.north <-invert.dat.field[which(invert.dat.field$site=="North"),]
invert.dat.field.south <-invert.dat.field[which(invert.dat.field$site=="South"),]


