

# Load libraries ----------------------------------------------------------


#make sure the below libraries are installed on your machine. Use install.packages("LIBRARYNAME")
rm(list = ls()) #clear objects in memory

# Set working directory
setwd(paste0(here::here(), "/11_Autocorrelation_Lab"))

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
library(spatialreg)
library(spdep)

options(stringsAsFactors = FALSE, scipen = 999)


# OLS Regression ----------------------------------------------------------


# Read in RDS
data(faithful)

# Explore
head(faithful)
plot(faithful$waiting, faithful$eruptions)

# Models
m0 <- lm(formula = faithful$eruptions ~ 1)
summary(m0)

m1 <- lm(formula = faithful$eruptions ~ faithful$waiting)
summary(m1)

# Compare
anova(m0, m1)

# Update model
m2 <- lm(formula = faithful$eruptions ~ I(faithful$waiting - mean(faithful$waiting)))
summary(m2)


# Spatial Regression ------------------------------------------------------


# Read in data
columbus <- readRDS("Raw_Data/Columbus.rds")

# Convert to simple features
columbus_sf <- st_as_sf(columbus)

# Create a queen neighbors and a row standardized weights
columbus_queen <- poly2nb(columbus_sf)
columbus_weights <- nb2listw(columbus_queen)

# Model
model1 <- lm(formula = crime ~ inc + hoval, data = columbus_sf)
summary(model1)

# Residuals
par(mfrow = c(2, 2)) 
plot(model1)

# Check spatial relationship
columbus$resid_model1 <- residuals(model1)
spplot(columbus, "resid_model1")

# Spatial lag model
model1_lag <- lagsarlm(formula = crime ~ inc + hoval,
                       data = columbus_sf,
                       listw = columbus_weights)
summary(model1_lag)
