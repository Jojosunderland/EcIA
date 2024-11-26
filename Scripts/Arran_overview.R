#Results page for overall comparison of the sites
# including all data (invertebrates and vertebrates)

# load packages required
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for data
arran <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Overview")

View(arran)

# extract only the required columns
arran.dat <- select(arran, site, order)

#remove NAs for the order column
arran.dat <- arran.dat[-which(is.na(arran.dat$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
arran.dat.north <-arran.dat[which(arran.dat$site=="North"),]
arran.dat.south <-arran.dat[which(arran.dat$site=="South"),]

# calculate how many unique orders there are between the sites
length(unique(arran.dat.north$order)) # 25 unique orders
length(unique(arran.dat.south$order)) # 25 unique orders


