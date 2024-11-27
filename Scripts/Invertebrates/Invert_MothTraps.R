# Results for the Invertebrate moth trap data
# Species were identified to the order level, abundance data

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for invertebrate moth trap data
# for both Site A (south) and Site B (north)
invert.moth <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Moth traps")

View(invert.moth)

# extract only the columns i'm interested in: site, individualCount and order
invert.moth.dat <- select(invert.moth, site, order, individualCount)

View(invert.moth.dat)
# no NAs needed to be removed

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.moth.dat.north <-invert.moth.dat[which(invert.moth.dat$site=="North"),]
invert.moth.dat.south <-invert.moth.dat[which(invert.moth.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.moth.dat.north$order)) # 5 unique orders
length(unique(invert.moth.dat.south$order)) # 3 unique orders

# number of recordings

nrow(invert.moth.dat.north) # 17 recordings
nrow(invert.moth.dat.south) # 5 recordings

#calculate abundances of each order:
order.counts <-  invert.moth.dat %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.n <- invert.moth.dat.north %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.s <- invert.moth.dat.south %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# View the results
print(order.counts.s)
print(order.counts.n)

# tables created to see if there is a difference in the orders:

unique.invert.moth.north <- setdiff(invert.moth.dat.north$order, invert.moth.dat.south$order)
unique.invert.moth.south <- setdiff(invert.moth.dat.south$order, invert.moth.dat.north$order)

unique.invert.moth.north # 3 unique order
unique.invert.moth.south # 1 unique orders

## Create a site by species matrix ##

#abundance data
invert.moth.dat.ab <- invert.moth.dat %>%
  pivot_wider(
    names_from=order, #order as column names
    values_from=individualCount, # count data for each column (order)
    values_fill = list(individualCount = 0), # NAs into 0's
    values_fn = list(individualCount=sum)) # sums count data for each order
invert.moth.dat.ab <- invert.moth.dat.ab[,-1] # delete the site column

View(invert.moth.dat.ab) # abundance matrix

#presence/absence data
invert.moth.dat.new <- invert.moth.dat[,c("order","site")] 
invert.moth.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
invert.moth.dat.new <- invert.moth.dat.new %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.moth.dat.pa <- invert.moth.dat.new %>% 
  pivot_wider(names_from=order,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each order becomes a column
list0 <- as.list(rep(0,ncol(invert.moth.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(invert.moth.dat.pa)
invert.moth.dat.pa <- as.data.frame(invert.moth.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(invert.moth.dat.pa) <- invert.moth.dat.pa$site #site columns are assigned as row names, columns are orders
invert.moth.dat.pa <- invert.moth.dat.pa[,-1] # delete the site column
invert.moth.dat.pa[invert.moth.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(invert.moth.dat.pa)

## Calculate diversity indices ##
#using Vegan package

#Shannon 
invert.moth.shan.ab <- diversity(invert.moth.dat.ab, index = 'shannon') #using abundance matrix
invert.moth.shan.ab

invert.moth.shan.pa <- diversity(invert.moth.dat.pa, index='shannon') # using P/A matrix
invert.moth.shan.pa

#Beta
invert.moth.beta <- beta.pair(invert.moth.dat.pa)
invert.moth.beta

invert.moth.beta$beta.sim # 33.33% of the dissimilarity is due to species turnover
invert.moth.beta$beta.sor # 50% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

## Plot 1: the difference in unique orders present between sites

ggplot(invert.moth.dat, aes(x=site, fill = order)) +
  geom_bar() +
  labs(x = "Site", y = "Count", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  # Swapped the axis labels around and changed their names to match names in project
  theme_bw() 

#Change colours of legend to be more colour blind friendly

library(viridis)

plot7 <- ggplot(invert.moth.dat, aes(x = site, fill = order)) +
  geom_bar() +
  labs(x = "Site", y = "Recordings of Invertebrates", fill = "Order") + 
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  scale_fill_viridis_d(option = "plasma") + 
  theme_bw()

plot7
## Plot 2: abundance differences between sites of orders

plot8 <- ggplot(invert.moth.dat, aes(x=order, fill = site))+
  geom_bar() +
  labs(y = "Abundance of Invertebrates",x = 'Order', fill = 'Site') +
  scale_fill_manual(values = c("South" = "lightblue", "North" = "lightgreen"),
                    limits = c("South", "North"),
                    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
plot8
# combine plots together

library(gridExtra)
quartz()
grid.arrange(plot7, plot8, ncol = 2)  # Arrange plots in 2 columns

## Plot 3: abundance differences between sites of orders

plot3.moth <- ggplot(order.counts.n, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightgreen") +
  labs(title = 'Site B', y = "Abundance of Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') 

plot3.moth

plot4.moth <- ggplot(order.counts.s, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightblue") +
  labs(title = 'Site A', y = "Abundance of Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 20, 5),
                     limits =c(0,20))
plot4.moth

# combine plots of abundances
library(gridExtra)
quartz()
grid.arrange(plot4.moth, plot3.moth, ncol = 2)  # Arrange plots in 2 columns

