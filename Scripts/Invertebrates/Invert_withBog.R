# Results for Bog data (found on Site A/south side)
# Going to manually add the bog sweep net data to the end of the invert field transects sheet in excel and then redo the plots and
# diversity indices to see if the bog makes any big changes.

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

## Invert terrestrial with bog data ##

# read in appropriate data sheet for bog data
invert.terr <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Invert terrestrial")

View(invert.terr)

# extract only the columns i'm interested in: site, individualCount and order
invert.terr.dat <- select(invert.terr, site, order, individualCount)

View(invert.terr.dat)

# remove any rows that have NAs for the order
invert.terr.dat <- invert.terr.dat[-which(is.na(invert.terr.dat$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.terr.dat.north <-invert.terr.dat[which(invert.terr.dat$site=="North"),]
invert.terr.dat.south <-invert.terr.dat[which(invert.terr.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.terr.dat.north$order)) # 6 unique orders
length(unique(invert.terr.dat.south$order)) # 9 unique orders

# number of recordings

nrow(invert.terr.dat.north) # 26 recordings
nrow(invert.terr.dat.south) # 90 recordings - 14 more recordings

#calculate abundances of each order:
order.counts <-  invert.terr.dat %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.n <- invert.terr.dat.north %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.s <- invert.terr.dat.south %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# View the results
print(order.counts.s)
print(order.counts.n)

# tables created to see if there is a difference in the orders:

unique.invert.terr.north <- setdiff(invert.terr.dat.north$order, invert.terr.dat.south$order)
unique.invert.terr.south <- setdiff(invert.terr.dat.south$order, invert.terr.dat.north$order)

unique.invert.terr.north # 1 unique order
unique.invert.terr.south # 4 unique orders

## Create a site by species matrix ##

#abundance data
invert.terr.dat.ab <- invert.terr.dat %>%
  pivot_wider(
    names_from=order, #order as column names
    values_from=individualCount, # count data for each column (order)
    values_fill = list(individualCount = 0), # NAs into 0's
    values_fn = list(individualCount=sum)) # sums count data for each order
invert.terr.dat.ab <- invert.terr.dat.ab[,-1] # delete the site column

View(invert.terr.dat.ab) # abundance matrix

#presence/absence data
invert.terr.dat.new <- invert.terr.dat[,c("order","site")] 
invert.terr.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
invert.terr.dat.new <- invert.terr.dat.new %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.terr.dat.pa <- invert.terr.dat.new %>% 
  pivot_wider(names_from=order,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each order becomes a column
list0 <- as.list(rep(0,ncol(invert.terr.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(invert.terr.dat.pa)
invert.terr.dat.pa <- as.data.frame(invert.terr.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(invert.terr.dat.pa) <- invert.terr.dat.pa$site #site columns are assigned as row names, columns are orders
invert.terr.dat.pa <- invert.terr.dat.pa[,-1] # delete the site column
invert.terr.dat.pa[invert.terr.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(invert.terr.dat.pa)


## Calculate diversity indices ##
#using Vegan package

#Shannon 
invert.terr.shan.ab <- diversity(invert.terr.dat.ab, index = 'shannon') #using abundance matrix
invert.terr.shan.ab

invert.terr.shan.pa <- diversity(invert.terr.dat.pa, index='shannon') # using P/A matrix
invert.terr.shan.pa

#Beta
invert.terr.beta <- beta.pair(invert.terr.dat.pa)
invert.terr.beta

invert.terr.beta$beta.sim # 16.67% of the dissimilarity is due to species turnover
invert.terr.beta$beta.sor # 33.33% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

## Plot 1: the difference in unique orders present between sites

ggplot(invert.terr.dat, aes(x=site, fill = order)) +
  geom_bar() +
  labs(x = "Site", y = "Count", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  # Swapped the axis labels around and changed their names to match names in project
  theme_bw() 

#Change colours of legend to be more colour blind friendly

install.packages("viridis")  # colour blind friendly package
library(viridis)

plot3 <- ggplot(invert.terr.dat, aes(x = site, fill = order)) +
  geom_bar() +
  labs(title = "Terrestrial invertebrate orders (field and bog)", x = "Site", y = "Abundance of Terrestrial Invertebrates", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  scale_fill_viridis_d(option = "plasma") + 
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_bw() 


## Plot 2: abundance differences between sites of orders

plot4 <- ggplot(invert.terr.dat, aes(x=order, fill = site))+
  geom_bar() +
  labs(title = "Terrestrial invertebrate abundance (field and bog)", y = "Abundance of Terrestrial Invertebrates",x = 'Order', fill = 'Site') +
  scale_fill_manual(values = c("South" = "lightblue", "North" = "lightgreen"),
                    limits = c("South", "North"),
                    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# combine plots together, compare between transect only data and transect+bog data

install.packages("gridExtra")
library(gridExtra)
# number of unique orders 
# plot1 is from invert_transects script
quartz()
grid.arrange(plot1, plot3, ncol = 2)  # Arrange plots in 2 columns

# invertebrate abundances (divided into diff orders)
#plot2 is from invert_transects script
quartz()
grid.arrange(plot2, plot4, ncol = 2)

## Plot 3: abundance differences between sites of orders

plot.ab.n <- ggplot(order.counts.n, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightgreen") +
  labs(title = 'Site B', y = "Abundance of Terrestrial Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 80, 20),
                     limits =c(0,80))

plot.ab.n

plot.ab.s <- ggplot(order.counts.s, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightblue") +
  labs(title = 'Site A', y = "Abundance of Terrestrial Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') 
plot.ab.s

# combine plots of abundances
library(gridExtra)
quartz()
grid.arrange(plot.ab.s, plot.ab.n, ncol = 2)  # Arrange plots in 2 columns




## Invertebrate aquatic data with the bog data ##
# Manually added the kicknet sampling data to the stream excel sheet

# read in appropriate data sheet for bog data
invert.aqua <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Invert aquatic")

View(invert.aqua)

# extract only the columns i'm interested in: site, individualCount and order
invert.aqua.dat <- select(invert.aqua, site, order, individualCount)

View(invert.aqua.dat)

# remove any rows that have NAs for the order
invert.aqua.dat <- invert.aqua.dat[-which(is.na(invert.aqua.dat$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.aqua.dat.north <-invert.aqua.dat[which(invert.aqua.dat$site=="North"),]
invert.aqua.dat.south <-invert.aqua.dat[which(invert.aqua.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.aqua.dat.north$order)) # 13 unique orders
length(unique(invert.aqua.dat.south$order)) # 12 unique orders

# number of recordings

nrow(invert.aqua.dat.north) # 45 recordings
nrow(invert.aqua.dat.south) # 40 recordings - 7 more recordings

#calculate abundances of each order:
order.counts <-  invert.aqua.dat %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.n <- invert.aqua.dat.north %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.s <- invert.aqua.dat.south %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# View the results
print(order.counts.s)
print(order.counts.n)

# tables created to see if there is a difference in the orders:

unique.invert.aqua.north <- setdiff(invert.aqua.dat.north$order, invert.aqua.dat.south$order)
unique.invert.aqua.south <- setdiff(invert.aqua.dat.south$order, invert.aqua.dat.north$order)

unique.invert.aqua.north # 3 unique order
unique.invert.aqua.south # 2 unique orders

## Create a site by species matrix ##

#abundance data
invert.aqua.dat.ab <- invert.aqua.dat %>%
  pivot_wider(
    names_from=order, #order as column names
    values_from=individualCount, # count data for each column (order)
    values_fill = list(individualCount = 0), # NAs into 0's
    values_fn = list(individualCount=sum)) # sums count data for each order
invert.aqua.dat.ab <- invert.aqua.dat.ab[,-1] # delete the site column

View(invert.aqua.dat.ab) # abundance matrix

#presence/absence data
invert.aqua.dat.new <- invert.aqua.dat[,c("order","site")] 
invert.aqua.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
invert.aqua.dat.new <- invert.aqua.dat.new %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.aqua.dat.pa <- invert.aqua.dat.new %>% 
  pivot_wider(names_from=order,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each order becomes a column
list0 <- as.list(rep(0,ncol(invert.aqua.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(invert.aqua.dat.pa)
invert.aqua.dat.pa <- as.data.frame(invert.aqua.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(invert.aqua.dat.pa) <- invert.aqua.dat.pa$site #site columns are assigned as row names, columns are orders
invert.aqua.dat.pa <- invert.aqua.dat.pa[,-1] # delete the site column
invert.aqua.dat.pa[invert.aqua.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(invert.aqua.dat.pa)

## Calculate diversity indices ##
#using Vegan package

#Shannon 
invert.aqua.shan.ab <- diversity(invert.aqua.dat.ab, index = 'shannon') #using abundance matrix
invert.aqua.shan.ab

invert.aqua.shan.pa <- diversity(invert.aqua.dat.pa, index='shannon') # using P/A matrix
invert.aqua.shan.pa

#Beta
invert.aqua.beta <- beta.pair(invert.aqua.dat.pa)
invert.aqua.beta

invert.aqua.beta$beta.sim # 16.67% of the dissimilarity is due to species turnover
invert.aqua.beta$beta.sor # 20% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

## Plot 1: the difference in unique orders present between sites

ggplot(invert.aqua.dat, aes(x=site, fill = order)) +
  geom_bar() +
  labs(x = "Site", y = "Count", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  # Swapped the axis labels around and changed their names to match names in project
  theme_bw() 

#Change colours of legend to be more colour blind friendly

install.packages("viridis")  # colour blind friendly package
library(viridis)

plot5 <- ggplot(invert.aqua.dat, aes(x = site, fill = order)) +
  geom_bar() +
  labs(title = "Aquatic invertebrate orders (streams and bog)", x = "Site", y = "Abundance of Freshwater Invertebrates", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  scale_fill_viridis_d(option = "plasma") + 
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  theme_bw() 


## Plot 2: abundance differences between sites of orders

plot6 <- ggplot(invert.aqua.dat, aes(x=order, fill = site))+
  geom_bar() +
  labs(title = "Aquatic invertebrate abundance (streams and bog)", y = "Abundance of Freshwater Invertebrates",x = 'Order', fill = 'Site') +
  scale_fill_manual(values = c("South" = "lightblue", "North" = "lightgreen"),
                    limits = c("South", "North"),
                    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# combine plots together, compare between transect only data and transect+bog data

install.packages("gridExtra")
library(gridExtra)
# number of unique orders 
quartz()
grid.arrange(plot1.stream, plot5, ncol = 2)  # Arrange plots in 2 columns

# invertebrate abundances (divided into diff orders)
quartz()
grid.arrange(plot2.stream, plot6, ncol = 2)

## Plot 3: abundance differences between sites of orders

plot3.aqua <- ggplot(order.counts.n, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightgreen") +
  labs(title = 'Site B', y = "Abundance of Freshwater Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') 

plot3.aqua

plot4.aqua <- ggplot(order.counts.s, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightblue") +
  labs(title = 'Site A', y = "Abundance of Freshwater Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 15, 5),
                     limits =c(0,15))
plot4.aqua

# combine plots of abundances
library(gridExtra)
quartz()
grid.arrange(plot4.aqua, plot3.aqua, ncol = 2)  # Arrange plots in 2 columns

