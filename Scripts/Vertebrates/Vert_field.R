# Results for the vertebrate field transects (including incidentals)
# species were identified to species level, present / absent data

# load packages required
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for vertebrate transect data
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
vert.north.sp <- length(unique(vert.field.dat.north$scientificName)) # 20 unique species
vert.south.sp <- length(unique(vert.field.dat.south$scientificName)) # 15 unique species

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

## Calculate diversity indices ##
#using Vegan package

#Shannon 
vert.field.shan <- diversity(vert.field.dat.pa, index='shannon') # using P/A matrix
vert.field.shan

#Beta
vert.field.beta <- beta.pair(vert.field.dat.pa)

vert.field.beta$beta.sim # 33.33% of the dissimilarity is due to species turnover
vert.field.beta$beta.sor # 42.86% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

# plot 1 number of different species found at each site

ggplot(vert.field.dat, aes(x=scientificName, fill = site)) +
  geom_bar() +
  labs(x="Species present at the sites", y = "Number of sightings", fill = "Sites") +
  scale_fill_manual(values = c("South" = "lightblue", "North" = "lightgreen"),
                    limits = c("South", "North"),
                    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# plot 2 composition of sites divided by species

plot1.vert <- ggplot(vert.field.dat, aes(x=site, fill = scientificName)) +
  geom_bar() +
  labs(x='Sites', fill = 'Species', y = "Number of sightings") +
  scale_x_discrete(
                 limits = c("South", "North"),
                 labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_viridis_d(option = "plasma") 

plot1.vert

# plot 3 composition of sites divided by classes

#new dataframe that includes species class
vert.field.taxa <- select(vert.field, site, class, scientificName)

plot2.vert <- ggplot(vert.field.taxa, aes(x=site, fill = class)) +
  geom_bar() +
  labs(x='Sites', fill = 'Class', y = "Total number of sightings") +
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_viridis_d(option = "plasma") 

plot2.vert

install.packages('patchwork') # used to combine the generated plots together
library(patchwork)

# Combine the two plots with adjusted widths
combined_plot <- plot1.vert + plot2.vert + plot_layout(widths = c(2, 1))  # Left plot twice the width of the right plot

# Display the combined plot
quartz()
print(combined_plot)

# plot 3, bar chart showing difference in total number of species present between sites

#make a new dataframe with counts of the species seen

vert.field.dat.count <- vert.field.dat %>%
  group_by(scientificName, site) %>%
  summarise(count = n(), .groups = "drop")  # `n()` counts rows

plot3.vert <- ggplot(vert.field.dat.count, aes(x=site, y = count, fill = site))+
  geom_col() +
  labs(x="Sites", y="Total number of vertebrate species present") +
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_manual(
    values = c("South" = "lightblue", "North" = "lightgreen")
  ) +
  theme(legend.position = 'none')
  
# combine the count plot and class plot together:
combined_plot2 <- plot3.vert + plot2.vert 

# Display the combined plot
quartz()
print(combined_plot2)

# plot 4, the total unique species seen between sites
# new dataframe of unique species between the two sites
species_counts <- vert.field.dat %>%
  group_by(site) %>%
  summarise(unique_species = n_distinct(scientificName))

plot4.vert <- ggplot(species_counts, aes(x=site, y = unique_species, fill = site)) +
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

plot4.vert  

# combine the count plot and class plot together:
combined_plot3 <- plot4.vert + plot2.vert 

# Display the combined plot
quartz()
print(combined_plot3)


