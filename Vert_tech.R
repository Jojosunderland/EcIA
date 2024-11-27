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

# extract only the columns i'm interested in: site, scientificName, class and individualCount
vert.tech.dat <- select(vert.tech, site, scientificName, individualCount, class)
# NOTE: individual count is the number of calls detected not number of species

View(vert.tech.dat)

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
vert.tech.dat.north <-vert.tech.dat[which(vert.tech.dat$site=="North"),]
vert.tech.dat.south <-vert.tech.dat[which(vert.tech.dat$site=="South"),]

# calculate how many unique species there are between the sites
length(unique(vert.tech.dat.north$scientificName)) # 6 unique species
length(unique(vert.tech.dat.south$scientificName)) # 4 unique species

## Create a site by species matrix ##

# ONLY presence/absence data
vert.tech.dat.new <- vert.tech.dat[,c("scientificName","site")] 
vert.tech.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
vert.tech.dat.new <- vert.tech.dat.new %>%
  group_by(site, scientificName) %>%
  summarise(presence = sum(presence), .groups = "drop")

vert.tech.dat.pa <- vert.tech.dat.new %>% 
  pivot_wider(names_from=scientificName,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each species becomes a column
list0 <- as.list(rep(0,ncol(vert.tech.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(vert.tech.dat.pa)
vert.tech.dat.pa <- as.data.frame(vert.tech.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(vert.tech.dat.pa) <- vert.tech.dat.pa$site #site columns are assigned as row names, columns are species
vert.tech.dat.pa <- vert.tech.dat.pa[,-1] # delete the site column
vert.tech.dat.pa[vert.tech.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(vert.tech.dat.pa)

## Calculate diversity indices ##
#using Vegan package

#Shannon 
vert.tech.shan <- diversity(vert.tech.dat.pa, index='shannon') # using P/A matrix
vert.tech.shan

#Beta
vert.tech.beta <- beta.pair(vert.tech.dat.pa)

vert.tech.beta$beta.sim # 25% of the dissimilarity is due to species turnover
vert.tech.beta$beta.sor # 40% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

# plot 1: difference in total number of recordings (individualCount) per site

plot1.vert.tech <- ggplot(vert.tech.dat, aes(x=site, y = individualCount, 
                                             fill = site)) +
  geom_col() +
  labs(x= "Sites", y = "Total number of recordings (audio and pictures)", fill = "Sites") +
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_manual(
    values = c("South" = "lightblue", "North" = "lightgreen")
) +
  theme(legend.position = 'none') #remove the legend


plot1.vert.tech

# plot 2 difference in total number of unique species present per site

# new dataframe that has the total number of unique species per site 
species.counts.tech <- vert.tech.dat %>%
  group_by(site) %>%
  summarise(unique_species = n_distinct(scientificName))

plot2.vert.tech <- ggplot(species.counts.tech, aes(x=site, y = unique_species, fill = site)) +
  geom_col() +
  labs(x='Sites', y = "Total number of different vertebrate species present") + 
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_manual(
    values = c("South" = "lightblue", "North" = "lightgreen")
  ) +
  theme(legend.position = 'none') #remove the legend

plot2.vert.tech

# plot 3 difference in species present in each site

plot3.vert.tech <- ggplot(vert.tech.dat, aes(x=site, fill = scientificName)) +
  geom_bar() +
  labs(x='Sites', y = "Total number of different vertebrate species",
       fill = 'Species') + 
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_viridis_d(option = "plasma")

plot3.vert.tech  

# plot 4 differnces in species class present in each site

plot4.vert.tech <- ggplot(vert.tech.dat, aes(x=site, fill = class)) +
  geom_bar() +
  labs(x='Sites', y = "Total number of different vertebrate classes",
       fill = 'Class') + 
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_viridis_d(option = "plasma", , begin =0.2, end = 0.6)

plot4.vert.tech

# Combine the two plots (species and class differences)
combined_plot5 <- plot3.vert.tech + plot4.vert.tech 

# Display the combined plot
quartz()
print(combined_plot5)
