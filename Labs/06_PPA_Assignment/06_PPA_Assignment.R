

# Load libraries ----------------------------------------------------------


#make sure the below libraries are installed on your machine. Use install.packages("LIBRARYNAME")
rm(list = ls()) #clear objects in memory

# Set working directory
setwd(paste0(here::here(), "/06_PPA_Assignment"))

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
library(spatstat)
library(rgdal)
library(maptools)
library(rgeos)
library(GISTools)
library(shapefiles)
library(aspace)

options(stringsAsFactors = FALSE, scipen = 999)


# Assignment -------------------------------------------------------------


# Import point patterns data
p1 <- readRDS("Raw_Data/pattern1.rds")
p2 <- readRDS("Raw_Data/pattern2.rds")
p3 <- readRDS("Raw_Data/pattern3.rds")

# 1. Populate the table below
# Number of events
p1$n
p2$n
p3$n
# Area of observation
spatstat::area(p1$window)
spatstat::area(p2$window)
spatstat::area(p3$window)
# Average intensity
p1$n / spatstat::area(p1$window)
p2$n / spatstat::area(p2$window)
p3$n / spatstat::area(p3$window)

# 2. Assessment of point patterns requires you to compare your event data against a pattern of CSR with the same level of average intensity. For each of the three patterns, if you were to carve out a 2 square unit area:
#   a. How many points do you expect to see in that area if the pattern is CSR?
p1_csr_hat <- (p1$n / spatstat::area(p1$window)) * 2
p2_csr_hat <- (p2$n / spatstat::area(p2$window)) * 2
p3_csr_hat <- (p3$n / spatstat::area(p3$window)) * 2

p1_csr_hat
p2_csr_hat
p3_csr_hat

#   b. What is the prob of observing exactly 5 events in the 2 square unit area if the pattern is CSR? 
p1_prob5 <- (exp(-p1_csr_hat) * (p1_csr_hat ^ 5)) / factorial(5)
p2_prob5 <- (exp(-p2_csr_hat) * (p2_csr_hat ^ 5)) / factorial(5)
p3_prob5 <- (exp(-p3_csr_hat) * (p3_csr_hat ^ 5)) / factorial(5)

p1_prob5
p2_prob5
p3_prob5

# 3. For each pattern, estimate the Kernel Density and show a plot overlaid with the point patterns.
# Calculate densities
p1_density <- density(p1) 
p2_density <- density(p2)
p3_density <- density(p3)

# P1
plot(p1_density, main = "Pattern 1 Kernel Density and Points")
plot(p1, add = TRUE)

# P2
plot(p2_density, main = "Pattern 2 Kernel Density and Points")
plot(p2, add = TRUE)

# P3
plot(p3_density, main = "Pattern 3 Kernel Density and Points")
plot(p3, add = TRUE)

# 4. Using the G function and the Ripley's K function assess whether each point pattern resembles a CSR, a clustered pattern, or a regular pattern. Include figures that show the envelope and the values of the G and K functions for all three patterns. Briefly explain the figures and your conclusion

# P1, G and Ripley's K
par(mfrow = c(1, 2))
p1_g_env <- envelope(p1, Gest)
plot(p1_g_env, main = "Pattern 1 - G Function")
p1_k_env <- envelope(p1, Kest)
plot(p1_k_env, main = "Pattern 1 - Ripley's K")

# P2, G and Ripley's K
par(mfrow = c(1, 2))
p2_g_env <- envelope(p2, Gest)
plot(p2_g_env, main = "Pattern 2 - G Function")
p2_k_env <- envelope(p2, Kest)
plot(p2_k_env, main = "Pattern 2 - Ripley's K")

# P3, G and Ripley's K
par(mfrow = c(1, 2))
p3_g_env <- envelope(p3, Gest)
plot(p3_g_env, main = "Pattern 3 - G Function")
p3_k_env <- envelope(p3, Kest)
plot(p3_k_env, main = "Pattern 3 - Ripley's K")

