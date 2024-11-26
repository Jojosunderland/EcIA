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

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
vert.field.dat.north <-vert.field.dat[which(vert.field.dat$site=="North"),]
vert.field.dat.south <-vert.field.dat[which(vert.field.dat$site=="South"),]

# calculate how many unique species there are between the sites
length(unique(vert.field.dat.north$scientificName)) # 20 unique species
length(unique(vert.field.dat.south$scientificName)) # 15 unique species

## Create a site by species matrix ##

# ONLY presence/absence data
vert.field.dat.new <- vert.field.dat[,c("scientificName","site")] 
vert.field.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
vert.field.dat.new <- vert.field.dat.new %>%
  group_by(site, scientificName) %>%
  summarise(presence = sum(presence), .groups = "drop")

vert.field.dat.pa <- vert.field.dat.new %>% 
  pivot_wider(names_from=scientificName,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each species becomes a column
list0 <- as.list(rep(0,ncol(vert.field.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(vert.field.dat.pa)
vert.field.dat.pa <- as.data.frame(vert.field.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(vert.field.dat.pa) <- vert.field.dat.pa$site #site columns are assigned as row names, columns are species
vert.field.dat.pa <- vert.field.dat.pa[,-1] # delete the site column
vert.field.dat.pa[vert.field.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(vert.field.dat.pa)
