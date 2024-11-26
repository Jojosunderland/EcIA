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

# extract only the columns i'm interested in: site, scientificName and individualCount
vert.tech.dat <- select(vert.tech, site, scientificName, individualCount)
# NOTE: individual count is the number of calls detected not number of species

View(vert.tech.dat)

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
vert.tech.dat.north <-vert.tech.dat[which(vert.tech.dat$site=="North"),]
vert.tech.dat.south <-vert.tech.dat[which(vert.tech.dat$site=="South"),]

# calculate how many unique species there are between the sites
length(unique(vert.tech.dat.north$scientificName)) # 6 unique species
length(unique(vert.tech.dat.south$scientificName)) # 4 unique species


