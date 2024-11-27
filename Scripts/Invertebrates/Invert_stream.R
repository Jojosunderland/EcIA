# Invertebrate stream data
# recorded to the order level

# load packages
library("readxl")
library("tidyverse")
library('ggplot2')
library('vegan')
library('betapart')
library('dplyr')

# read in appropriate data sheet for invertebrate stream data
# for both Site A (south) and Site B (north)
invert.stream <-
  read_xlsx("~/Documents/WorkingD/EcIA/Data/Arran_GBIF_data.xlsx",sheet="Invert streams")

View(invert.stream)

# extract only the columns i'm interested in: site, individualCount and order
invert.stream.dat <- select(invert.stream, site, order, individualCount)

View(invert.stream.dat)

# remove any rows that have NAs for the order
invert.stream.dat <- invert.stream.dat[-which(is.na(invert.stream.dat$order)),] #which orders have NAs

# create dataframes for each site (north = B, south = A) - keeping them named as north and south here to match the data. 
invert.stream.dat.north <-invert.stream.dat[which(invert.stream.dat$site=="North"),]
invert.stream.dat.south <-invert.stream.dat[which(invert.stream.dat$site=="South"),]

# calculate how many unique orders are there between the sites
length(unique(invert.stream.dat.north$order)) # 13 unique orders
length(unique(invert.stream.dat.south$order)) # 9 unique orders

# number of recordings

nrow(invert.stream.dat.north) # 45 recordings
nrow(invert.stream.dat.south) # 33 recordings

#calculate abundances of each order:
order.counts <-  invert.stream.dat %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.n <- invert.stream.dat.north %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

order.counts.s <- invert.stream.dat.south %>%
  group_by(site, order) %>%
  summarise(total_count = sum(individualCount), .groups = "drop")

# View the results
print(order.counts.s)
print(order.counts.n)

# tables created to see if there is a difference in the orders:

unique.invert.stream.north <- setdiff(invert.stream.dat.north$order, invert.stream.dat.south$order)
unique.invert.stream.south <- setdiff(invert.stream.dat.south$order, invert.stream.dat.north$order)

unique.invert.stream.north # 5 unique order
unique.invert.stream.south # 1 unique orders


## Create a site by species matrix ##

#abundance data
invert.stream.dat.ab <- invert.stream.dat %>%
  pivot_wider(
    names_from=order, #order as column names
    values_from=individualCount, # count data for each column (order)
    values_fill = list(individualCount = 0), # NAs into 0's
    values_fn = list(individualCount=sum)) # sums count data for each order
invert.stream.dat.ab <- invert.stream.dat.ab[,-1] # delete the site column

View(invert.stream.dat.ab) # abundance matrix

#presence/absence data
invert.stream.dat.new <- invert.stream.dat[,c("order","site")] 
invert.stream.dat.new$presence <- 1  # adds a new column presences (sets everything to 1s)

# group by site, order and sum the values (presence data) for each group 
invert.stream.dat.new <- invert.stream.dat.new %>%
  group_by(site, order) %>%
  summarise(presence = sum(presence), .groups = "drop")

invert.stream.dat.pa <- invert.stream.dat.new %>% 
  pivot_wider(names_from=order,values_from=c(presence)) 
# pivot_wider creates a wide-format data frame, each site becomes a row, each order becomes a column
list0 <- as.list(rep(0,ncol(invert.stream.dat.pa))) # creates a list where each column name is assigned a value of 0
names(list0) <- names(invert.stream.dat.pa)
invert.stream.dat.pa <- as.data.frame(invert.stream.dat.pa %>% replace_na(list0)) 
# replace NA values with 0, representing species absence
row.names(invert.stream.dat.pa) <- invert.stream.dat.pa$site #site columns are assigned as row names, columns are orders
invert.stream.dat.pa <- invert.stream.dat.pa[,-1] # delete the site column
invert.stream.dat.pa[invert.stream.dat.pa > 0] <- 1 # converts count data to presence (1), absences are 0

View(invert.stream.dat.pa)

## Calculate diversity indices ##
#using Vegan package

#Shannon 
invert.stream.shan.ab <- diversity(invert.stream.dat.ab, index = 'shannon') #using abundance matrix
invert.stream.shan.ab

invert.stream.shan.pa <- diversity(invert.stream.dat.pa, index='shannon') # using P/A matrix
invert.stream.shan.pa

#Beta
invert.stream.beta <- beta.pair(invert.stream.dat.pa)
invert.stream.beta

#simpson 
invert.stream.beta$beta.sim # 11.11% of the dissimilarity is due to species turnover
#sorensen
invert.stream.beta$beta.sor # 27.27% of the total diversity is due to differences between the two sites 


## VISUALISING THE DATA ##

# Plot 1: differences in unique number of orders per stream at each site
library(viridis) # colour blind friendly colours

plot1.stream <- ggplot(invert.stream.dat, aes(x=site, fill = order)) +
  geom_bar() +
  labs(x= 'Site', fill = 'Order', y = 'Recordings of Freshwater Invertebrates') +
  scale_x_discrete(limits = c("South", "North"), labels = c("South" = "A", "North" = "B")) +
  scale_fill_viridis_d(option = "plasma") + 
  theme_bw() 

plot1.stream

#Plot 2: abundance differences

plot2.stream <- ggplot(invert.stream.dat, aes(x=order, fill = site)) +
  geom_bar() +
  labs(title = 'Aquatic invertebrate abundance (only streams)', x='Order', y='Abundance of Freshwater Invertebrates', fill = 'Site') +
  scale_fill_manual(values = c("South" = "lightblue", "North" = "lightgreen"),
                    limits = c("South", "North"),
                    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# combine plots together

library(gridExtra)
quartz()
grid.arrange(plot1.stream, plot2.stream, ncol = 2)  # Arrange plots in 2 columns

## Plot 3: abundance differences between sites of orders

plot3.stream <- ggplot(order.counts.n, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightgreen") +
  labs(title = 'Site B', y = "Abundance of Freshwater Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') 

plot3.stream

plot4.stream <- ggplot(order.counts.s, aes(y = total_count, x =order, fill = order))+
  geom_col(fill = "lightblue") +
  labs(title = 'Site A', y = "Abundance of Freshwater Invertebrates",x = 'Order') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(0, 15, 5),
                     limits =c(0,15))
plot4.stream

# combine plots of abundances
library(gridExtra)
quartz()
grid.arrange(plot4.stream, plot3.stream, ncol = 2)  # Arrange plots in 2 columns


  