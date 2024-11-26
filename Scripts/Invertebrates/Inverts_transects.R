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

# extract only the columns i'm interested in: site, individualCount and order
invert.field.dat <- select(invert.field, site, order, individualCount)

View(invert.field.dat)

# remove any rows that have NAs for the order
invert.field.dat <- invert.field.dat[-which(is.na(invert.field.dat$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.field.dat.north <-invert.field.dat[which(invert.field.dat$site=="North"),]
invert.field.dat.south <-invert.field.dat[which(invert.field.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.field.dat.north$order)) # 6 unique orders
length(unique(invert.field.dat.south$order)) # 9 unique orders
