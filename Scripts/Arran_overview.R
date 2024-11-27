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

nrow(arran.dat.north) # 122 recordings
nrow(arran.dat.south) # 168 recordings

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

## Calculate diversity indices ##
#using Vegan package

#Shannon 
arran.shan <- diversity(arran.dat.pa, index='shannon') # using P/A matrix
arran.shan

#Beta
arran.beta <- beta.pair(arran.dat.pa)

arran.beta$beta.sim # 20% of the dissimilarity is due to species turnover
arran.beta$beta.sor # 20% of the total diversity is due to differences between the two sites 

## Plot diversity differences ##

library(ggplot2)

# plot 1 differences in order composition

plot1.arran <- ggplot(arran.dat, aes(x=site, fill = order)) +
  geom_bar() +
  labs(x="Sites", fill = "Order", y ='Number of times recorded') +
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_viridis_d(option='plasma')

plot1.arran
# plot 2 differences in class 

#new dataframe
arran.dat.class <- select(arran, site, class)
#remove NAs
arran.dat.class <- arran.dat.class[-which(is.na(arran.dat.class$class)),] 

plot2.arran <- ggplot(arran.dat.class, aes(x=site, fill = class)) +
  geom_bar() +
  labs(x="Sites", fill = "Class", y ='Number of times recorded') +
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_viridis_d(option='plasma')

plot2.arran

#combine plots together
library(patchwork)

combined_plot6 <- plot1.arran + plot2.arran 

# Display the combined plot
quartz()
print(combined_plot6)

#plot 3 number of unique orders per site

# new dataframe that has the total number of unique species per site 
order.count <- arran.dat %>%
  group_by(site) %>%
  summarise(unique_orders = n_distinct(order))

plot3.arran <- ggplot(order.count, aes(x=site, y = unique_orders, fill = site)) +
  geom_col() +
  labs(x='Sites', y = "Total number of different orders") + 
  scale_x_discrete(
    limits = c("South", "North"),
    labels = c("South" = "A", "North" = "B")) +
  theme_bw() +
  scale_fill_manual(
    values = c("South" = "lightblue", "North" = "lightgreen")
  ) +
  theme(legend.position = 'none') #remove the legend

plot3.arran

#combine plots together
library(patchwork)

combined_plot7 <- plot3.arran + plot1.arran 

# Display the combined plot
quartz()
print(combined_plot7)
