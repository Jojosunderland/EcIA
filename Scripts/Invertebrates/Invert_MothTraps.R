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

View(invert.moth)

# extract only the columns i'm interested in: site, individualCount and order
invert.moth.dat <- select(invert.moth, site, order, individualCount)

View(invert.moth.dat)
# no NAs needed to be removed

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.moth.dat.north <-invert.moth.dat[which(invert.moth.dat$site=="North"),]
invert.moth.dat.south <-invert.moth.dat[which(invert.moth.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.moth.dat.north$order)) # 5 unique orders
length(unique(invert.moth.dat.south$order)) # 3 unique orders

