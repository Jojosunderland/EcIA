# Invertebrate stream data
# recorded to the order level

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for invertebrate transect data
# for both Site A (south) and Site B (north)
invert.stream <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Invert streams")

View(invert.stream)

# extract only the columns i'm interested in: site, individualCount and order
invert.stream.dat <- select(invert.stream, site, order, individualCount)

View(invert.stream.dat)

# remove any rows that have NAs for the order
invert.stream.dat <- invert.stream.dat[-which(is.na(invert.stream.dat$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.stream.dat.north <-invert.stream.dat[which(invert.stream.dat$site=="North"),]
invert.stream.dat.south <-invert.stream.dat[which(invert.stream.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.stream.dat.north$order)) # 13 unique orders
length(unique(invert.stream.dat.south$order)) # 9 unique orders
