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
invert.dat.field <- invert.dat.field[-which(is.na(invert.dat.field$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.dat.field.north <-invert.dat.field[which(invert.dat.field$site=="North"),]
invert.dat.field.south <-invert.dat.field[which(invert.dat.field$site=="South"),]

# calculate how many unique orders are there between the sites
north.orders <- length(unique(invert.dat.field.north$order)) # 6 orders
south.orders <- length(unique(invert.dat.field.south$order)) # 10 orders

## Create a site by species presence/absence matrix for each site

# For both sites
invert.dat.field.red <- invert.dat.field[, c("order", "site")]
invert.dat.field.red$presence <- 1 # add a presence column, all set to 1

#remove duplicates and sum presence values
invert.dat.field.red <- invert.dat.field.red %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

# pivot the data to create a site-by-species matrix
invert.dat.field.pa <- invert.dat.field.red %>%
  pivot_wider(names_from=order, values_from = c(presence), values_fill = 0) 
  # the values inside the matrix indicate presence (1) or absence (0)

# replace NA values with 0 (absence)              
list0 <- as.list(rep(0,ncol(invert.dat.field.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(invert.dat.field.pa)
invert.dat.field.pa <- as.data.frame(invert.dat.field.pa %>% replace_na(list0)) # replace NA values with 0, representing species absence

row.names(invert.dat.field.pa) <- invert.dat.field.pa$site #site columns are assigned as row names
invert.dat.field.pa <- invert.dat.field.pa[,-1] # delete the site column
invert.dat.field.pa[invert.dat.field.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(invert.dat.field.pa)


# For North (B)
invert.dat.field.north.red <- invert.dat.field.north[, c("order", "site")]
invert.dat.field.north.red$presence <- 1

invert.dat.field.north.red <- invert.dat.field.north.red %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.dat.field.north.pa <- invert.dat.field.north.red %>%
  pivot_wider(names_from = order, values_from = presence, values_fill = 0)  %>% 
  replace_na(list0)

row.names(invert.dat.field.north.pa) <- invert.dat.field.north.pa$site
invert.dat.field.north.pa <- invert.dat.field.north.pa[, -1]
invert.dat.field.north.pa[invert.dat.field.north.pa > 0] <- 1

View(invert.dat.field.north.pa)

# For South (A)
invert.dat.field.south.red <- invert.dat.field.south[, c("order", "site")]
invert.dat.field.south.red$presence <- 1

invert.dat.field.south.red <- invert.dat.field.south.red %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.dat.field.south.pa <- invert.dat.field.south.red %>% 
  pivot_wider(names_from = order, values_from = presence, values_fill = 0) %>% 
  replace_na(list0)
row.names(invert.dat.field.south.pa) <- invert.dat.field.south.pa$site
invert.dat.field.south.pa <- invert.dat.field.south.pa[, -1]
invert.dat.field.south.pa[invert.dat.field.south.pa > 0] <- 1

View(invert.dat.field.south.pa) 

## Calculate biodiversity indices 


