

# Load libraries ----------------------------------------------------------


#make sure the below libraries are installed on your machine. Use install.packages("LIBRARYNAME")
rm(list=ls()) #clear objects in memory

# Set working directory
setwd(paste0(here::here(), "/05_NNI"))

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
library(spatstat)

options(stringsAsFactors = FALSE, scipen = 999)
set.seed(54321)


# Create random data for quadrat ------------------------------------------


# Create x and y coordinates for two point patterns
#create 200 random numbers from a uniform distribution between 0 and 1
x1 <- runif(200)
y1 <- runif(200)
?runif #look at bottom of help page for examples
hist(x1, 10)

#create 200 random numbers from a normal distribution with mean 0.5 and standard deviation 0.15
x2 <- rnorm(200, 0.5, 0.15)
y2 <- rnorm(200, 0.5, 0.15)
?rnorm
hist(x2, 10)

#-------------------------------
#create an observation window
w1 <- owin(c(0, 1), c(0, 1))
?owin #explore owin

#-------------------------------
#create your ppp objects,  summarize them,  note similarities/differences
p1 <- ppp(x1, y1, window = w1)
p2 <- ppp(x2, y2, window = w1)


# Compute NN distances ---------------------------------------------------------


# Average NN distances
p1_dist <- nndist(p1) %>% mean()
p2_dist <- nndist(p2) %>% mean()
  
# CSR values
csr_dist = 0.5 / sqrt(p1$n / spatstat::area(w1))
csr_se = 0.2614 / sqrt((p1$n) ^ 2 / spatstat::area(w1))
  
# Z-scores, compare against CSR
(p1_dist - csr_dist) / csr_se
(p2_dist - csr_dist) / csr_se


# The G function ----------------------------------------------------------


# Cdf plot of distances
plot(ecdf(nndist(p2)))

# Compute probability of no neighbors
lambda <- 10
r1 <- seq(0, 1, .01)
ar1 <- pi * r1 ^ 2
p0ev <- exp(-lambda * ar1)

# Plot
par(mfrow = c(1, 2))
plot(r1, p0ev, type = "l", xlim = c(0, 1), main = "Prob. of no events at distance R")
plot(r1, I(1 - p0ev), type = "l", xlim = c(0, 1), main = "Prob. of at least 1 event")

# Compare to CSR - above CSR line suggests clustering, below indicates regular spacing
g_p2 <- Gest(p2)
plot(g_p2)

# Simulation based inference
z1 <- runif(50)
z2 <- 3 * z1 + runif(50) * 2

# Test
cor(z1, z2)
cor(z1, sample(z2))
cor_m <- vector(length = 99)
for (i in 1:99){cor_m[i] <- cor(z1, sample(z2))}
summary(cor_m)
hist(cor_m, xlim = c(-1, 1))
abline(v = cor(z1, z2), col = 4)

# G function with simulate 
g_p2e <- envelope(p2, Gest)
plot(g_p2e)


# The F function ----------------------------------------------------------


# Instead of distances b/w events, measures from reference points
# P1
f_p1 <- Fest(p1)
plot(f_p1)
f_p1e <- envelope(p1, Fest)
plot(f_p1e)

# P2
f_p2 <- Fest(p2)
plot(f_p2)
f_p2e <- envelope(p2, Fest)
plot(f_p2e)


# The J function ----------------------------------------------------------


# Combination of G and F functions
# P1
j_p1 <- Jest(p1)
plot(j_p1)
j_p1e <- envelope(p1, Jest)
plot(j_p1e)

# P2
j_p2 <- Jest(p2)
plot(j_p2)
j_p2e <- envelope(p2, Jest)
plot(j_p2e)


# Ripley's K function -----------------------------------------------------


# P1
k_p1 <- Kest(p1)
plot(k_p1)
k_p1e <- envelope(p1, Kest)
plot(k_p1e)

# P2
k_p2 <- Kest(p2)
plot(k_p2)
k_p2e <- envelope(p2, Kest)
plot(k_p2e)


# L function -----------------------------------------------------


# P1
l_p1 <- Lest(p1)
plot(l_p1)
l_p1e <- envelope(p1, Lest)
plot(l_p1e, . - r ~ r)

# P2
l_p2 <- Lest(p2)
plot(l_p2)
l_p2e <- envelope(p2, Lest)
plot(l_p2e, . - r ~ r)


# Cross K-Function -----------------------------------------------------


# Read in pines data
p <- readRDS("../04_PPP/Raw_Data/pines.rds")
pnew <- p
infected <- sample(c(0, 0, 0, 1, 1), 65, replace = TRUE)#generate 65 observations from the vector c(0, 0, 0, 1, 1) with replacement. Roughly 40% (2  / 5) of the trees are infected
marks(p) <- infected
#define the marks as a factor
marks(p) <- factor(marks(p))
marks(pnew) <- ifelse(pnew$x < 0.5 & pnew$y <0.5, 1, 0)
kc1 <- envelope(p, Kcross)
kc2 <- envelope(pnew, Kcross)

plot(kc1)
plot(kc2)



# As simple features object
p1_sf <- st_as_sf(p1)
p2_sf <- st_as_sf(p2)

# Plots
p1_plot <- ggplot() +
  geom_sf(data = p1_sf %>% filter(label == "point"), color = "red", fill = NA, size = 1.5) +
  geom_sf(data = p1_sf %>% filter(label == "window"), fill = NA, size = 0.5) +
  hrbrthemes::theme_ipsum()

p2_plot <- ggplot() +
  geom_sf(data = p2_sf %>% filter(label == "point"), color = "red", fill = NA, size = 1.5) +
  geom_sf(data = p2_sf %>% filter(label == "window"), fill = NA, size = 0.5) +
  hrbrthemes::theme_ipsum()

p1_plot | p2_plot


# Quadrat tests -----------------------------------------------------------


#Use a quadrat test to examine if point pattern shows complete spatial randomness (CSR)
#Perform a quadrat test. Null hypothesis is CSR.

#Show the quadrat counts
quadratcount(p1)
plot(quadratcount(p1))

#you can also specify the number of bins
#quadratcount(p1, 6, 6)
#plot(quadratcount(p1, 6, 6))

quadrat.test(p1)#p-value > 0.5. Fail to reject CSR.
quadrat.test(p1, 4, 4)#one can change the number of quadrats used if needed
plot(quadrat.test(p1, 4, 4))#plot the quadrat test.  Gives observed count in cell,  expected count,  and pearson residuals.

#repeat with p2
#Show the quadrat counts
quadratcount(p1)
plot(quadratcount(p1))

quadrat.test(p2)#reject hypothesis of CSR. 
plot(quadrat.test(p2))
plot(intensity(quadratcount(p1), image = TRUE))
plot(intensity(quadratcount(p2), image = TRUE))
#Not CSR. But is it clustered or regularly spaced? Visual inspection suggests clustered. 
quadrat.test(p2, alternative = "clustered")#reject the null in favor of a clustered spaced pattern
quadrat.test(p2, alternative = "regular")#can't reject the null in favor of a regularly spaced pattern
#the pattern p2 shows clustering then would be expected from a CSR process.

#-------------------------------
#Compute the kernel density for pattern p1
d1 <- density(p1)#calls the density.ppp function since object p1 is ppp object
?density.ppp#this is the function being used
?density#this is another function from a different library (the stats library) 

#plot the density function
plot(d1)

#Examine density with different standard deviation specified.  The density.ppp function uses a Gaussian (Normal) kernel. Other options are also possible. 
#-------------------------------
#for the density,  one can specify the bandwidith  / standard deviation of the kernel function. This specifies how peaked or how flat the kernel function is at the observed points. Note the differences below with different sigma values
par(mfrow = c(2, 2))
plot(density(p1))#here R computes an appropriate standard deviation for the density function
plot(density(p1, sigma = 0.01))#but you can also specificy the standard deviation. Note the differences from the plots
plot(density(p1, sigma = .1))
plot(density(p1, sigma = 1))
#########note the different images

#-------------------------------
#Repeat with p2
#for the density,  one can specify the bandwidith  / standard deviation of the kernel function. This specifies how peaked or how flat the kernel function is at the observed points. Note the differences below with different sigma values
par(mfrow = c(2, 2))
plot(density(p2))#here R computes an appropriate standard deviation for the density function
plot(density(p2, sigma = 0.01))#but you can also specificy the standard deviation. Note the differences from the plots
plot(density(p2, sigma = .1))
plot(density(p2, sigma = 1))
#
par(mfrow = c(1, 1))#reset the plot window

#-------------------------------
#Explore the density object
class(d1)#image
names(d1)#contain the followin attributes

d1$v#values of the kernel density for each element in the grid
d1$xstep#the dimensions of the grid in the x direction
d1$ystep#the dimensions of the grid in the y direction

#get the sum of the intensity multiplied by the area of each grid. What do you expect the result to be?
sum(d1$v * d1$xstep * d1$ystep)
#the total number of events


par(mfrow = c(1, 2))
d2 <- density(p2)
plot(d1)
plot(d2)

#when it is sensible to do,  these density objects can be summed,  divided,  etc. as appropriate (See example with marked ppp at end of this document ).
par(mfrow = c(1, 1))
plot(d2  / (d1  + d2))
plot(d1  / (d1  + d2))





###### ====================================================================
# Start of submission
###### ====================================================================





#-----------------
#On your own. Download the pines and redwood data provided on blackboard
#repeat the analysis above 
#-----------------
par(mfrow = c(1, 2))
p <- readRDS("Raw_Data/pines.RDS")
r <- readRDS("Raw_Data/redwood.RDS")

#summarize the pines and redwood data
#What class is the data?
class(p)
class(r)

#Create a summary for each data pattern
summary(p)
summary(r)

#do a quadrat count for each
quadratcount(p)
quadratcount(r)

#do a quadrat test for each pattern to test against CSR
#you may need to increase the areas by specifying that 3 horizontal and 3 vertical intervals be used for the test
plot(quadrat.test(p, nx = 3))
plot(quadrat.test(r, nx = 3))

#do a quadrat test for each pattern with a clustered and regular alternative specificed
# P
quadrat.test(p, nx = 3,  alternative = "clustered")
quadrat.test(p, nx = 3,  alternative = "regular")

# R
quadrat.test(r, nx = 3,  alternative = "clustered")
quadrat.test(r, nx = 3,  alternative = "regular")

#Note your conclusions
# We cannot reject the null hypothesis that the Pines point pattern is CSR. One-tail tests both note p-values above our threshold for clustered or regularly spaced. The Redwood point pattern does suggest that the data is spatially clustered beyond what one might reasonably expect under conditions of an underlying homogenous Poisson distribution.

#Create and plot the densities of each point pattern
p_dens <- density(p)
r_dens <- density(r)

plot(p_dens)
plot(r_dens)

#--------------------------------------------------------------
#Working with a marked ppp object
#--------------------------------------------------------------
#Lets create marks for one of the data points. For example,  lets assume that we had also collected data on whether or not the pines were infected by a particular tree disease (0 = no,  1 = yes)
#generate the simulated disease data
summary(p)#65 points so need to create 65 infected  / not-infected data points
infected <- sample(c(0, 0, 0, 1, 1), 65, replace = TRUE)#generate 65 observations from the vector c(0, 0, 0, 1, 1) with replacement. Roughly 40% (2  / 5) of the trees are infected
infected
summary(infected)
marks(p) <- infected
summary(p)
#define the marks as a factor
marks(p) <- factor(marks(p))

summary(p)
plot(p)

#now that we have marks,  we can split the point pattern by the mark 
pines2 <- split(p)
summary(pines2)
plot(pines2)


plot(density(pines2))
?split#note that you can specify which mark to use,  when you have multiple marks in a point pattern data.

#Compute the kernel density
d3 <- density(pines2)
d3
d3[1]
d3[2]
plot(d3)

#compute and plot the relative proportiopns of the intensity 
plot(d3[2] / (d3[1] + d3[2]), main = "P-infected")
#shows the relative proportion of intensity of infections
