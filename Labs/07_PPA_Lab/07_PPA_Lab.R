

# Load libraries ----------------------------------------------------------


#make sure the below libraries are installed on your machine. Use install.packages("LIBRARYNAME")
rm(list = ls()) #clear objects in memory

# Set working directory
setwd(paste0(here::here(), "/07_PPA_Lab"))

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


# Import data -------------------------------------------------------------


# Read in RDS
cr17 <- readRDS("Raw_Data/crimes2017.rds")
ca <- readRDS("Raw_Data/CommunityAreas.rds")
beats <- readRDS("Raw_Data/beats.rds")

# Convert to simple features
cr17_sf <- st_as_sf(cr17, crs = 26971)
ca_sf <- st_as_sf(beats, crs = 26971)
beats_sf <- st_as_sf(beats, crs = 26971)

# Explore
barplot(table(cr17$month))
barplot(table(cr17$ampm))
plot(table(cr17$month, cr17$ampm))
plot(table(cr17$violent, cr17$ampm))

# Filter to July
cr17_july <- subset(cr17, cr17$month == 7)


# Create PPP --------------------------------------------------------


# Create coordinates
r <- dim(cr17_july[1])
cx <- coordinates(cr17_july)[, 1] + runif(r) / 1000
cy <- coordinates(cr17_july)[, 2] + runif(r) / 1000 
  
# Create window
chi.win <- aggregate(beats)
chi.winw <- as.owin(chi.win)

# Create PPP object
cr7_ppp <- ppp(x = cx, y = cy, window = chi.winw, marks = cr17_july$violent)
cry_ppp <- as.ppp(cr7_ppp)
marks(cr7_ppp) <- factor(marks(cr7_ppp))

# Visualize
plot(cr7_ppp)
plot(split(cr7_ppp))
plot(density(cr7_ppp))
plot(density(split(cr7_ppp)))

cr7_ppsd <- density(split(cr7_ppp), positive = TRUE)
plot(cr7_ppsd)


# Define cases and controls, create density map -----------------------------------------------


# Create objects
cases <- unmark(subset(cr7_ppp, marks(cr7_ppp) == "Violent"))
controls <- unmark(subset(cr7_ppp, marks(cr7_ppp) != "Violent"))

par(mfrow = c(1, 2))
plot(cases)
plot(controls)

# Create density maps
bw <- 1200
kcases <- density(cases, sigma = bw)
kcontrols <- density(controls, sigma = bw)

par(mfrow = c(1, 2))
plot(kcases)
plot(kcontrols)


# Relative risk -----------------------------------------------------------


# Ratio of densities
kratio <- kcases / kcontrols
plot(kratio)
text(ca, "community", cex = 0.5)
summary(as.vector(kratio$v))

# Create cut points
cutp2 <- c(quantile(na.omit(as.vector(kratio$v)), probs = seq(0, 1, by = 0.25)))

# Create new density map using these quantiles
kratio2 <- cut(kratio, cutp2)
plot(kratio2)
text(ca, "community", cex = 0.5)

# Probabilities compute over space
pratio <- kcases / (kcases + kcontrols)
plot(pratio)


# Alternative method -----------------------------------------------------------


# Using relrisk
rr1 <- relrisk(cr7_ppp, sigma = bw, control = "Non-violent", relative = TRUE)
rr2 <- relrisk(cr7_ppp, sigma = bw)

plot(rr1, main = "Relative Risk")
plot(rr2, main = "Probabilities")

# Standard errors
rr1 <- relrisk(cr7_ppp, sigma = bw, control = "Non-violent", relative = TRUE, se = TRUE)
rr2 <- relrisk(cr7_ppp, sigma = bw, se = TRUE)

plot(rr1$SE, main = "Relative Risk")
plot(rr2$SE, main = "Probabilities")


# Chorley data --------------------------------------------------------


# Import 
data(chorley)

# Explore
summary(chorley)
chorley.extra$plotit()

# Overall - G Function
chorley_g_env <- envelope(chorley, Gest)
plot(chorley_g_env, main = "Chorley - G Function")

# By Type - G Function
larynx <- split(chorley)$larynx
lung <- split(chorley)$lung

par(mfrow = c(1, 2))
larynx_g_env <- envelope(larynx, Gest)
plot(larynx_g_env, main = "Larynx - G Function")
lung_g_env <- envelope(lung, Gest)
plot(lung_g_env, main = "Lung - G Function")


# Chorley relative risk ---------------------------------------------------


# Estimates and standard errors
chorley_rr <- relrisk(chorley, sigma = 1.2, control = "lung", relative = TRUE, se = TRUE)
plot(chorley_rr$estimate, main = "Chorley - Relative Risk")
points(chorley.extra$incin, pch = 10, cex = 2, col = 4)


# Model -------------------------------------------------------------------


# Estimate theta
lung_z <- density(lung, sigma = 1.2, positive = TRUE)
ppm(larynx ~ 1)
ppm(larynx ~ offset(log(lung_z)))

# Create distance measure
incin <- ppp(x = chorley.extra$incin$x, y = chorley.extra$incin$y, window = chorley$window)
incin_z <- density(incin, sigma = 1.2, positive = TRUE)
ppm(larynx ~ offset(log(incin_z)))

