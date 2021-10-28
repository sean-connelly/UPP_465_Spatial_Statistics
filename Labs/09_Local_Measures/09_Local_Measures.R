

# Load libraries ----------------------------------------------------------


#make sure the below libraries are installed on your machine. Use install.packages("LIBRARYNAME")
rm(list = ls()) #clear objects in memory

# Set working directory
setwd(paste0(here::here(), "/09_Local_Measures"))

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


# In Class ----------------------------------------------------------------


# Read in data
il <- readRDS("Raw_Data/Il_2012_election.rds")
#Names of counties that start with M appear to be wrong.
il$COUNTY <- ifelse(il$COUNTY == "Macoupin", "Mchenry", as.character(il$COUNTY))

plot(il)
summary(il)
summary(il$diff)#Obama-Romney
spplot(il, "diff")
#alternative
spplot(il, "diff", col.regions = rev(bpy.colors(100)), par.settings = list(axis.line = list(col = 0)))

#create neighbor list and weight matrix
il_nb1 <- poly2nb(il)
w <- nb2listw(il_nb1)

#Moran's plot
moran.plot(il$diff, w, labels = il$COUNTY)

moran.plot(il$diff, w, labels = il$COUNTY, xlim = c(-65, 60), ylim = c(-50, 30), 
           xlab = "Difference (%Obama - %Romney)", ylab = "Spatially Laged Difference (%Obama - %Romney)")

#Note: The Moran plot is between diff and the average of diff at the neighbors of each unit. We can manually draw it as follows:
il$lag_diff <- stats::lag(w, il$diff)#create the lag variable
plot(il$diff, il$lag_diff, cex = 0.75)#plot the lag against lag_diff
text(il$diff, il$lag_diff, il$COUNTY, cex = 0.75, pos = 2)#add text
abline(lm(il$lag_diff ~ il$diff))#add a line based on a linear model that regresses lag_diff against diff
abline(h = mean(il$lag_diff), lty = 2)#draw a horizontal line at the mean of the lag_diff
abline(v = mean(il$diff), lty = 2)#draw a virtical line at the mean of diff

#Note: the Global Moran's I is equal to the slope in the regression of the spatial lags on the variable of interest
summary(lm(il$lag_diff ~ il$diff))#Global Moran's I = the estimate for the slope of il$diff
moran.mc(il$diff, w, nsim = 999)

########################################3
##Local Moran I 
## Local Autocorrelation: Local Moran's I (normality assumption)
lm1 <- localmoran(il$diff, listw = w)
summary(lm1)
#Each column represnts the local Moran's I, the expected value under the null, the variance, Z value, p-value

#For a row standardize w matrix, the mean of the local Moran I's is equal to the global Moran's I
mean(lm1[,1])

#Let's get some of these values into the il dataset
il$lmoran_i <- lm1[,1] ## Extract Ii
il$lmoran_z <- abs(lm1[,4]) ## Extract the absolute value of the z values
il$lmoran_p <- lm1[,5] ## Extract p values
il$lmoran_sig01 <- lm1[,5] < 0.01 #True/Fals p<0.01
il$lmoran_sig05 <- lm1[,5] < 0.05 #True/Fals p<0.05
summary(il)

#Plotting the Ii is not informative. Rather, we are interested in seeing if the measure departs from what we expect at random.
#We then also examine the Moran scatter plat to see what type of clustering it implies


#Define a color palett to be used for mapping
lm.palette <- colorRampPalette(c("white", "orange", "red"), space = "rgb")
spplot(il, zcol = "lmoran_z", col.regions = lm.palette(100), main = "Local Moran's I (|z| scores)")

il$lm.palette1 <- ifelse(il$lmoran_p < 0.05, "red", "white")
il$lm.palette2 <- ifelse(il$lmoran_p < 0.01, "red", "white")

plot(il, col = il$lm.palette1, main = "Ii, p-val<0.05")
plot(il, col = il$lm.palette2, main = "Ii, p-val<0.01")

#Because we are using the p-values only, we don't know yet the type of cluster we are looking at. We can use the Moran scatter plot to determine which quadrant these polgons fall in
il$scaled_diff <- scale(il$diff)#scale the diff variable (x-mean(x))/sd(x)
il$lag_scaled_diff <- stats::lag(w, il$scaled_diff)#calculate the lagged variable for each county

#Use above two variables to identify where the point lies in the moran plot
il$lag_cat <- 
  ifelse(il$scaled_diff > 0 & il$lag_scaled_diff > 0, "High-High", 
         ifelse(il$scaled_diff > 0 & il$lag_scaled_diff < 0, "High-Low", 
                ifelse(il$scaled_diff < 0 & il$lag_scaled_diff < 0, "Low-Low", "Low-High")))
il$lag_cat <- factor(il$lag_cat)
spplot(il, "lag_cat", col = "grey")

#lets isolate just the significant areas
il$lag_cat2 <- ifelse(il$lmoran_sig05 == TRUE, as.character(il$lag_cat), "Not Sig") #p<0.05
il$lag_cat3 <- ifelse(il$lmoran_sig01 == TRUE, as.character(il$lag_cat), "Not Sig") #p<0.01

il$lag_cat2 <- factor(il$lag_cat2)
il$lag_cat3 <- factor(il$lag_cat3)

summary(il$lag_cat2)
summary(il$lag_cat3)

spplot(il, "lag_cat2", col.regions = c(2, 4, 0), col = 8, main = "Local Moran's I, p<0.05")
spplot(il, "lag_cat3", col.regions = c(2, 4, 0), col = 8, main = "Local Moran's I, p<0.01")

il$col2 <- ifelse(il$lag_cat2 == "High-High", 2, ifelse(il$lag_cat2 == "Low-Low", 4, 0))
il$col3 <- ifelse(il$lag_cat3 == "High-High", 2, ifelse(il$lag_cat3 == "Low-Low", 4, 0))

################################
#Corrections for p-values
################################
#Reported p-values may lead to false positives so we can use more conservative p-values or apply corrections
lm2c <- localmoran(il$diff, w, p.adjust.method = "bonferroni")
plot(lm2c[,5], lm1[,5])
abline(0, 1)
abline(h = 0.05)
abline(v = 0.05)
#Compare number of locations with p values < 0.05 before and after correction
table(withCorrection = lm2c[,5] < 0.05, berforeCorrection = lm1[,5] < 0.05)
table(withCorrection = lm2c[,5] < 0.01, berforeCorrection = lm1[,5] < 0.01)
table(withCorrection = lm2c[,5] < 0.001, berforeCorrection = lm1[,5] < 0.001)

############################################
######################
#Local G and G* (Getis and Ord)
############################################

#Local G
############################################
lG <- localG(il$diff, w)
lG #these outputs are the z values (from a standard normal distribution)

il$lG <- as.vector(lG)
il$lG_sig05 <- ifelse(il$lG < -1.96, "Low", ifelse(il$lG < 1.96, "Not sig", "High")) # p<0.05 
il$lG_sig01 <- ifelse(il$lG < -2.58, "Low", ifelse(il$lG < 2.58, "Not sig", "High")) # p<0.01
il$lG_sig05 <- factor(il$lG_sig05)
il$lG_sig01 <- factor(il$lG_sig01)


spplot(il, "lG_sig05", col.regions = c(2, 4, 8), main = "Local G, p-val<0.05")
spplot(il, "lG_sig01", col.regions = c(2, 4, 8), main = "Local G, p-val<0.01")


#Local G Star
############################################
#Need to include self in the neighbor list
il_nb2 <- include.self(il_nb1) #include self as neighbor
lGs <- localG(il$diff, nb2listw(il_nb2)) #can do adjustments here as well

#Bring the local G variables into your data
il$lGs <- as.vector(lGs)
il$lGs_sig05 <- ifelse(il$lGs < -1.96, "Low", ifelse(il$lGs < 1.96, "Not sig", "High"))
il$lGs_sig01 <- ifelse(il$lGs < -2.58, "Low", ifelse(il$lGs < 2.58, "Not sig", "High"))
il$lGs_sig001 <- ifelse(il$lGs < -3.29, "Low", ifelse(il$lGs < 3.29, "Not sig", "High"))
il$lGs_sig05 <- factor(il$lGs_sig05)
il$lGs_sig01 <- factor(il$lGs_sig01)
il$lGs_sig001 <- factor(il$lGs_sig001)

# Plot local G
spplot(il, "lGs_sig001", col.regions=c(2, 4, 8)) #core of cluster
spplot(il, "lGs_sig01", col.regions=c(2, 4, 8)) #core+neighbors
spplot(il, "lGs_sig05", col.regions=c(2, 4, 8))

#To find the different critical values from the normmal distribution
qnorm(0.05/2)
qnorm(0.01/2)
qnorm(0.001/2)





# *************************************************************************
# START OF ASSIGNMENT
# *************************************************************************






# Midwest data ------------------------------------------------------------


# Read in RDS
midwest <- readRDS("Raw_Data/Midwest_2012_election.rds")

# Convert to simple features
midwest_sf <- st_as_sf(midwest, crs = 4269)

# Create a queen neighbors and a row standardized weights
midwest_queen <- poly2nb(midwest_sf)
midwest_weights <- nb2listw(midwest_queen)

#Optional: plot a map showing the network of neighbors
plot(midwest, border = 8)
plot(midwest_queen, coordinates(midwest), add = TRUE)
title(main = paste("Midwest Counties Queen"))


# Local Moran's I ---------------------------------------------------------


# Create a Moran plot. Use County name as label
moran.plot(midwest_sf$diff, midwest_weights, labels = midwest_sf$COUNTY)

# Create a lag variable for the diff variable
midwest_sf <- midwest_sf %>% mutate(lag_diff = stats::lag(midwest_weights, diff))
plot(midwest_sf$diff, midwest_sf$lag_diff, cex = 0.75)
text(midwest_sf$diff, midwest_sf$lag_diff, midwest_sf$COUNTY, cex = 0.75, pos = 2)
abline(lm(midwest_sf$lag_diff ~ midwest_sf$diff))
abline(h = mean(midwest_sf$lag_diff), lty = 2)
abline(v = mean(midwest_sf$diff), lty = 2)

# Undertake a local Moran's I analysis
moran_midwest <- localmoran(midwest_sf$diff, listw = midwest_weights) %>% 
  as_tibble() %>% 
  rename("moran_i" = Ii,
         "exp_moran_i" = E.Ii,
         "var_moran_i" = Var.Ii,
         "moran_z" = Z.Ii,
         "moran_p" = "Pr(z > 0)")

# Bring the p-values into your data, create factor variables for significance at the 0.05, 0.01 and 0.001 thresholds
midwest_sf <- bind_cols(midwest_sf, moran_midwest) %>% 
  mutate(moran_sig001 = ifelse(moran_p < 0.001, TRUE, FALSE),
         moran_sig01 = ifelse(moran_p < 0.01, TRUE, FALSE),
         moran_sig05 = ifelse(moran_p < 0.05, TRUE, FALSE))

# Plot the significant areas
plot(midwest, col = ifelse(midwest_sf$moran_sig001 == TRUE, "red", "white"), 
     main = "Midwest, p-val<0.001")
plot(midwest, col = ifelse(midwest_sf$moran_sig01 == TRUE, "red", "white"),
     main = "Midwest, p-val<0.01")
plot(midwest, col = ifelse(midwest_sf$moran_sig05 == TRUE, "red", "white"), 
     main = "Midwest, p-val<0.05")

# Create factor variables that combine information from significance and the Moran scatter plot
midwest_sf <- midwest_sf %>% 
  mutate(scaled_diff = scale(diff),
         lag_scaled_diff = stats::lag(midwest_weights, scaled_diff),
         lag_cat = case_when(scaled_diff > 0 & lag_scaled_diff > 0 ~ "High-High",
                             scaled_diff > 0 & lag_scaled_diff < 0 ~ "High-Low",
                             scaled_diff < 0 & lag_scaled_diff < 0 ~ "Low-Low", 
                             TRUE ~ "Low-High") %>% 
           as_factor())

# Plot clusters and significance
ggplot() +
  geom_sf(data = midwest_sf, aes(fill = lag_cat), color = "gray") +
  geom_sf(data = midwest_sf, aes(color = moran_sig001), fill = NA) +
  scale_color_manual(values = c("gray", "black")) +
  theme_void()


# Local G* ----------------------------------------------------------------


# Update the neighbor list to include self and recreate the weight matrix, compute the local G z values and import into your data
gstar_midwest <- localG(midwest_sf$diff, nb2listw(include.self(midwest_queen))) %>% 
  as.vector()

# Create variables for differet significance levels
midwest_sf$gstar <- gstar_midwest

midwest_sf <- midwest_sf %>% 
  mutate(gstar_sig001 = case_when(gstar < -3.29 ~ "Low",
                                  gstar > 3.29 ~ "High",
                                  TRUE ~ "Not sig"),
         gstar_sig01 = case_when(gstar < -2.58 ~ "Low",
                                 gstar > 2.58 ~ "High",
                                 TRUE ~ "Not sig"),
         gstar_sig05 = case_when(gstar < -1.96 ~ "Low",
                                 gstar > 1.96 ~ "High",
                                 TRUE ~ "Not sig"))

# Plot your hot and cold spots
midwest_sf %>% 
  dplyr::select(FIPS, starts_with("gstar_sig")) %>% 
  st_drop_geometry() %>% 
  pivot_longer(-FIPS, names_to = "sig_level", values_to = "z_score") %>% 
  left_join(midwest_sf %>% dplyr::select(FIPS, geometry), ., by = "FIPS") %>% 
  ggplot() +
  geom_sf(aes(fill = z_score), color = "gray") +
  facet_wrap( ~ sig_level) +
  theme_void()