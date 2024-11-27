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

# number of recordings

nrow(invert.field.dat.north) # 26 recordings
nrow(invert.field.dat.south) # 78 recordings

#calculate abundances of each order:
order.counts <-  invert.field.dat %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.n <- invert.field.dat.north %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.s <- invert.field.dat.south %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# View the results
print(order.counts.s)

# calculate how many unique orders are there between the sites
length(unique(invert.field.dat.north$order)) # 6 unique orders
length(unique(invert.field.dat.south$order)) # 9 unique orders

# tables created to see if there is a difference in the orders:

unique.invert.field.north <- setdiff(invert.field.dat.north$order, invert.field.dat.south$order)
unique.invert.field.south <- setdiff(invert.field.dat.south$order, invert.field.dat.north$order)

unique.invert.field.north # 1 unique order
unique.invert.field.south # 4 unique orders

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

## Calculate diversity indices ##
#using Vegan package

#Shannon 
invert.shan.ab <- diversity(invert.field.dat.ab, index = 'shannon') #using abundance matrix
invert.shan.ab

invert.shan.pa <- diversity(invert.field.dat.pa, index='shannon') # using P/A matrix
invert.shan.pa

#Beta
invert.beta <- beta.pair(invert.field.dat.pa)
invert.beta

invert.beta$beta.sim # 16.67% of the dissimilarity is due to species turnover
invert.beta$beta.sor # 33.33% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

## Plot 1: the difference in unique orders present between sites

ggplot(invert.field.dat, aes(x=site, fill = order)) +
  geom_bar() +
  labs(x = "Site", y = "Count", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  # Swapped the axis labels around and changed their names to match names in project
  theme_bw() 

#Change colours of legend to be more colour blind friendly

install.packages("viridis")  # colour blind friendly package
library(viridis)

plot1 <- ggplot(invert.field.dat, aes(x = site, fill = order)) +
  geom_bar() +
  labs(title = "Terrestrial invertebrate orders (only field)", x = "Site", y = "Abundance of Terrestrial Invertebrates", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  scale_fill_viridis_d(option = "plasma") + 
  theme_bw()+
  scale_y_continuous(breaks = seq(0, 100, 20))

plot1

## Plot 2: abundance differences between sites of orders

plot2 <- ggplot(invert.field.dat, aes(x=order, fill = site))+
  geom_bar() +
  labs(title = "Terrestrial invertebrate abundance (only field)",y = "Abundance of Terrestrial Invertebrates",x = 'Order', fill = 'Site') +
  scale_fill_manual(values = c("South" = "lightblue", "North" = "lightgreen"),
                    limits = c("South", "North"),
                    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# combine plots together

install.packages("gridExtra")
library(gridExtra)
quartz()
grid.arrange(plot1, plot2, ncol = 2)  # Arrange plots in 2 columns

## Plot 3: abundance differences between sites of orders

plot3 <- ggplot(order.counts.n, aes(y = total_count, x =order, fill = order))+
  geom_col() +
  labs(title = 'Site B', y = "Abundance of Field Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') +
  scale_fill_viridis_d(option = "plasma", end = 0.8)
plot3

plot4 <- ggplot(order.counts.s, aes(y = total_count, x =order, fill = order))+
  geom_col() +
  labs(title = 'Site A', y = "Abundance of Field Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') +
  scale_fill_viridis_d(option = "plasma")
plot4

# combine plots of abundances
library(gridExtra)
quartz()
grid.arrange(plot4, plot3, ncol = 2)  # Arrange plots in 2 columns
