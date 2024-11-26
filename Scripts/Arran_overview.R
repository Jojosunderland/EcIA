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

## Create a site by species matrix ##

# ONLY presence/absence data - as verts dont have counts
arran.dat.new <- arran.dat[,c("order","site")] 
arran.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
arran.dat.new <- arran.dat.new %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

arran.dat.pa <- arran.dat.new %>% 
  pivot_wider(names_from=order,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each species becomes a column
list0 <- as.list(rep(0,ncol(arran.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(arran.dat.pa)
arran.dat.pa <- as.data.frame(arran.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(arran.dat.pa) <- arran.dat.pa$site #site columns are assigned as row names, columns are species
arran.dat.pa <- arran.dat.pa[,-1] # delete the site column
arran.dat.pa[arran.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(arran.dat.pa)
