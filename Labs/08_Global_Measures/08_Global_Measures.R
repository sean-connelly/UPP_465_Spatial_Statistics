

# Load libraries ----------------------------------------------------------


#make sure the below libraries are installed on your machine. Use install.packages("LIBRARYNAME")
rm(list = ls()) #clear objects in memory

# Set working directory
setwd(paste0(here::here(), "/08_Global_Measures"))

# Load additional libraries
pacman::p_load(tidyverse, # for basic data manipulation, visualization
               scales, # for formatting number output
               patchwork, # for arranging ggplots in grids
               sf, # simple features for spatial
               nngeo, # nearest neighbors
               summarytools, # for checking data frame characteristics
               here, # for relative file paths
               knitr, # for tables
               kableExtra, # table styling
               janitor) # for cleaning and tabulations

#load your spatial libraries
library(sp)
library(raster)
library(spdep)

options(stringsAsFactors = FALSE, scipen = 999)


# Pearson Correlation -----------------------------------------------------


#generate variables z1 and z2
set.seed(54321)
Verror <- 3 #try with 15 
z1 <- runif(50)
z2 <- 3 * z1 + runif(50) * Verror
cor(z1, z2)
#Suppose z1 and z2 were collected from field data collection. Examine their relationship.
plot(z1, z2)
cor(z1, z2)

#Could this relationship have happened by chance?

#######Alternative 1
#correlation test - an analytical solution
#Null hypothesis: cor(z1, z2)  =  0
cor.test(z1, z2)
#see week 2 slides for what statitisc is computed and tested against the a student's t distribution.
#Note that the p-value << 0.05. Meaning,  if our Null hypothesis was correct,  the chances we would see such a large t statistic is very very very small. Thus we reject the null hypothesis of no correlation.

#An alternative way to do this is to delink the pairing of z1 and z2 values as observed. We can do a permutation on z2 and compute correlation repeatedly and see if the correleation of the observed data is similar to what we get under permutation. 
cor(z1, sample(z2))# just one try
#now let's do it 99 more times
cor_m <- vector(length = 99)#a vector to store values in
for (i in 1:99){cor_m[i] <- cor(z1, sample(z2))}#do 99 permutations and compute the correlation
summary(cor_m)
hist(cor_m, xlim = c(-1, 1), main = "Correlations under permutation")
abline(v = cor(z1, z2), col = 4)#the observed correlation

# We can see that the relationship between z1 and z2 is unlikely to be due to chance.
#Alter Verror  (line 6) to be equal to 15 and run the test again.
# the psuedo-pvalue from this analysis. Add cor(z1, z2) as the 100th observation of r.
cor_m2 <- data.frame(c(cor_m, cor(z1, z2)))
names(cor_m2) <- "r"
psuedo_p <- rank(-cor_m2$r)[100]/(nrow(cor_m2))
psuedo_p


# IL presidential election data (2012) ------------------------------------


# Read in RDS
il <- readRDS("Raw_Data/Il_2012_election.RDS")

# Convert to simple features
il_sf <- st_as_sf(il, crs = 26971)

# Explore
plot(il)
summary(il)
summary(il$diff) # Obama-Romney
spplot(il, "diff")
# Alternative
spplot(il, "diff", col.regions = rev(bpy.colors(100)), par.settings = list(axis.line = list(col = 0)))


# Create a neigbor list ---------------------------------------------------


# Queen and Rook
il_nb1 <- poly2nb(il) # Queen 
il_nb2 <- poly2nb(il, queen = FALSE) # Rook
diff <- diffnb(il_nb1, il_nb2) # Identify the difference

# Get the centroids of the counties
il_cent <- coordinates(il)

#plot the three neighbor definitions
par(mfrow = c(1, 3))
plot(il, border = 8)
plot(il_nb1, il_cent, add = TRUE)
title(main = paste("IL Counties Queen"))

plot(il, border = 8)
plot(il_nb2, il_cent, add = TRUE)
title(main = paste("IL Counties Rook"))

plot(il, border = 8)
plot(diff, il_cent, add = TRUE, col = 2)
title(main = paste("IL Counties Difference"))


# K Nearest Neighbors -----------------------------------------------------


# K Nearest Neighbors
il_nb_k1 <- knn2nb(knearneigh(il_cent, k = 1)) # 1 nearest neighbor
il_nb_k2 <- knn2nb(knearneigh(il_cent, k = 2)) # 2 nearest neighbor
il_nb_k3 <- knn2nb(knearneigh(il_cent, k = 3)) # 3 nearest neighbor
il_nb_k1[1] # how many neighbors for first county
il_nb_k3[1] # how many neighbors for first county
plot(il, border = 8)
plot(il_nb_k1, il_cent, add = TRUE)
plot(il, border = 8)
plot(il_nb_k2, il_cent, add = TRUE)
plot(il, border = 8)
plot(il_nb_k3, il_cent, add = TRUE)


# Distance Based Neighbors ------------------------------------------------


# First identify the nearest neighbors. Lets use 10 here.
# Compute distances. unlist just chanes the structure of the data from a list to a vector.
il_nb_k10 <- knn2nb(knearneigh(il_cent, k = 1))
dsts <- unlist(nbdists(il_nb_k10, il_cent))
summary(dsts) # distances range from 18.5Km to 43194Km
par(mfrow = c(1, 3))

# Use 25km
il_nb_d1 <- dnearneigh(il_cent, d1 = 0, d2 = 25000) # set the min and max distances for neighbor definitions
plot(il, border = 8)
plot(il_nb_d1, il_cent, add = TRUE)

# Use 50km
il_nb_d1 <- dnearneigh(il_cent, d1 = 0, d2 = 50000) # set the min and max distances for neighbor definitions
plot(il, border = 8)
plot(il_nb_d1, il_cent, add = TRUE)
#use 75km
il_nb_d1 <- dnearneigh(il_cent, d1 = 0, d2 = 75000) # set the min and max distances for neighbor definitions
plot(il, border = 8)
plot(il_nb_d1, il_cent, add = TRUE)

######################
# Criteria for no isolates
# Note the number of isolates at 25k. You want to make sure distances are no smaller than the maximum nearest neighbor (~44km) to ensure at least one neighbor is present.
######################
il_nb_d1 <- dnearneigh(il_cent, d1 = 0, d2 = 44000) # set the min and max distances for neighbor definitions
plot(il, border = 8)
plot(il_nb_d1, il_cent, add = TRUE)


# Global measures of autocorrelation --------------------------------------


# Join count statistics - binary attributes
il$ObamaWon <- ifelse(il$diff > 0, 1, 0)
il$ObamaWon <- factor(il$ObamaWon)
summary(il$ObamaWon)
spplot(il, "ObamaWon", col.regions = c(2, 4), par.settings = list(axis.line = list(col = 0)))
summary(il$ObamaWon)

# Permutations of il$ObamaWon
il$ObamaWon2 <- sample(il$ObamaWon)
il$ObamaWon3 <- sample(il$ObamaWon)
spplot(il, "ObamaWon2", col.regions = c(2, 4), par.settings = list(axis.line = list(col = 0)))
spplot(il, "ObamaWon3", col.regions = c(2, 4), par.settings = list(axis.line = list(col = 0)))
par(mfrow = c(1, 1))
plot(il, col = ifelse(il$ObamaWon = 1, 4, 2))
text(il, il$COUNTY, cex = 0.35)

######################
# In order to compute the different autocorrelation measure, we need to convert the neighbors list into a spatial weights matrix. This is done through the nb2listw function. The default is to standardize the weights matrix. 
# ?nb2listw
######################

# Joincount test
w1 <- nb2listw(il_nb1, style = "B") # binary weight matrix. il_nb1 is a queen neigbor definition.
# nb2listw converts a neighbor list to a weight matrix

# Summary of the analytical tests
joincount.multi(il$ObamaWon, nb2listw(il_nb1, style = "B"))
joincount.multi(il$ObamaWon2, nb2listw(il_nb1, style = "B"))
joincount.multi(il$ObamaWon3, nb2listw(il_nb1, style = "B"))
joincount.mc(il$ObamaWon, w1, nsim = 999)

# Moran's I
summary(factor(il$ObamaWon))
w <- nb2listw(il_nb1) # row standardized weight matrix
moran.test(il$diff, nb2listw(il_nb1))
moran.mc(il$diff, w, nsim = 999)

# Geary's C
summary(factor(il$ObamaWon))
w <- nb2listw(il_nb1) # row standardized weight matrix
geary.test(il$diff, nb2listw(il_nb1))
geary.mc(il$diff, w, nsim = 999)





# *************************************************************************
# START OF ASSIGNMENT
# *************************************************************************





# Assignment (Due April 6, 2020) ------------------------------------------


# Read in RDS
midwest <- readRDS("Raw_Data/Midwest_2012_election.RDS")

# Convert to simple features
midwest_sf <- st_as_sf(midwest, crs = 4269)

# Define neighbors
# Queen
midwest_queen <- poly2nb(midwest_sf)

# # Rook
# midwest_rook <- poly2nb(midwest_sf, queen = FALSE)
# 
# # Identify the difference
# midwest_diff <- diffnb(midwest_queen, midwest_rook) 
# 
# # Get the centroids of the counties
# midwest_cent <- coordinates(midwest)
# 
# #plot the three neighbor definitions
# par(mfrow = c(1, 3))
# plot(midwest, border = 8)
# plot(midwest_queen, midwest_cent, add = TRUE)
# title(main = paste("Midwest Counties Queen"))
# 
# plot(midwest, border = 8)
# plot(midwest_rook, midwest_cent, add = TRUE)
# title(main = paste("Midwest Counties Rook"))
# 
# plot(midwest, border = 8)
# plot(midwest_diff, midwest_cent, add = TRUE, col = 2)
# title(main = paste("Midwest Counties Difference"))

# Create dummy variable
midwest_sf <- midwest_sf %>% 
  mutate(obama_dummy = ifelse(WINNER == "Obama", 1, 0) %>% 
           as_factor())

# Compute global autocorrelation measures using the Midwest data
#1. Use join count based on a variable that takes 1 when Obamawon and 0 otherwise
joincount.multi(midwest_sf$obama_dummy, nb2listw(midwest_queen, style = "B"))
joincount.mc(midwest_sf$obama_dummy, nb2listw(midwest_queen, style = "B"), nsim = 999)

#2. Use Moran's I
moran.test(midwest_sf$diff, nb2listw(midwest_queen))
moran.mc(midwest_sf$diff, nb2listw(midwest_queen), nsim = 999)

#3. Use Geary's C
geary.test(midwest_sf$diff, nb2listw(midwest_queen))
geary.mc(midwest_sf$diff, nb2listw(midwest_queen), nsim = 999)

#4. Interpret your results
