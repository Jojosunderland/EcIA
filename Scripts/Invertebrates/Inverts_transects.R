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

## Create a site by species matrix ##

#abundance data
invert.field.dat.ab <- invert.field.dat %>%
  pivot_wider(
    names_from=order, #order as column names
    values_from=individualCount, # count data for each column (order)
    values_fill = list(individualCount = 0), # NAs into 0's
    values_fn = list(individualCount=sum)) # sums count data for each order
invert.field.dat.ab <- invert.field.dat.ab[,-1] # delete the site column

View(invert.field.dat.ab) # abundance matrix

#presence/absence data
invert.field.dat.new <- invert.field.dat[,c("order","site")] 
invert.field.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
invert.field.dat.new <- invert.field.dat.new %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.field.dat.pa <- invert.field.dat.new %>% 
  pivot_wider(names_from=order,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each order becomes a column
list0 <- as.list(rep(0,ncol(invert.field.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(invert.field.dat.pa)
invert.field.dat.pa <- as.data.frame(invert.field.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(invert.field.dat.pa) <- invert.field.dat.pa$site #site columns are assigned as row names, columns are orders
invert.field.dat.pa <- invert.field.dat.pa[,-1] # delete the site column
invert.field.dat.pa[invert.field.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(invert.field.dat.pa)
