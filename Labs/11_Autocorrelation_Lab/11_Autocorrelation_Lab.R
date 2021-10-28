

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
library(raster)
library(spdep)
library(epitools)

options(stringsAsFactors = FALSE, scipen = 999)


# Import data -------------------------------------------------------------

# Read in RDS
CA <- readRDS("Raw_Data/ChicagoCommunityAreas.rds")
hmcd17 <- readRDS("Raw_Data/Homicides2017.rds")

# Convert ot simple features
CA_sf <- st_as_sf(CA, crs = 2790)
hmcd17_sf <- st_as_sf(hmcd17, crs = 2790)


# Homicides by Community Area ---------------------------------------------


# Identify homicides by community area
hmcd17_CA <- st_join(hmcd17_sf, CA_sf)

# Aggregate homicides by community area, join to spatial data
CA2_sf <- hmcd17_CA %>% 
  st_drop_geometry() %>% 
  group_by(community) %>% 
  summarize(nhmcd17 = n()) %>% 
  left_join(CA_sf, ., by = "community") %>% 
  mutate(nhmcd17 = ifelse(is.na(nhmcd17), 0, nhmcd17))

# Create a population rate
CA2_sf <- CA2_sf %>% 
  mutate(hmcdr = (nhmcd17 / population) * 10000)

# Plots
# Homicides
plot_CA_hmcd <- ggplot() +
  geom_sf(data = CA2_sf, aes(fill = nhmcd17), color = "gray") +
  labs(title = "2017 Homicides by Community Area",
       subtitle = "Chicago, IL") +
  hrbrthemes::theme_ipsum()

# Homicide Rate
plot_CA_hmcdr <- ggplot() +
  geom_sf(data = CA2_sf, aes(fill = hmcdr), color = "gray") +
  labs(title = "2017 Homicide Rate Per 10K Residents\nby Community Area",
       subtitle = "Chicago, IL") +
  hrbrthemes::theme_ipsum()

plot_CA_hmcd + plot_CA_hmcdr


# Global spatial autocorrelation ------------------------------------------


# Create neighbor list
CA2_queen <- poly2nb(CA2_sf) # Queen 

# Construct weights
CA2_weights <- nb2listw(CA2_queen)

# Moran plot
moran.plot(CA2_sf$hmcdr, CA2_weights, labels = CA2_sf$community)

# Moran's I
moran.test(CA2_sf$hmcdr, CA2_weights)
moran.mc(CA2_sf$hmcdr, CA2_weights, nsim = 999)

# Geary's C
geary.test(CA2_sf$hmcdr, CA2_weights)
geary.mc(CA2_sf$hmcdr, CA2_weights, nsim = 999)

# Create factor variable that combines info from significance and the Moran scatter plot
CA2_sf <- CA2_sf %>% 
  mutate(scaled_hmcd = scale(hmcdr),
         lag_scaled_hmcd = stats::lag(CA2_weights, scaled_hmcd),
         msp_cat = case_when(scaled_hmcd > 0 & lag_scaled_hmcd > 0 ~ "High-High",
                             scaled_hmcd > 0 & lag_scaled_hmcd < 0 ~ "High-Low",
                             scaled_hmcd < 0 & lag_scaled_hmcd < 0 ~ "Low-Low", 
                             TRUE ~ "Low-High") %>% 
           as_factor())

# Plot
ggplot() +
  geom_sf(data = CA2_sf, aes(fill = msp_cat), color = "gray") +
  labs(title = "Moran Scatter Plot by Community Area",
       subtitle = "Chicago, IL") +
  hrbrthemes::theme_ipsum()


# Local Moran's I ---------------------------------------------------------


# Undertake a local Moran's I analysis
moran_CA2 <- localmoran(CA2_sf$hmcdr, listw = CA2_weights) %>% 
  as_tibble() %>% 
  rename("moran_i" = Ii,
         "exp_moran_i" = E.Ii,
         "var_moran_i" = Var.Ii,
         "moran_z" = Z.Ii,
         "moran_p" = "Pr(z > 0)")

# Bring the p-values into your data, create factor variables for significance at the 0.05, 0.01 and 0.001 thresholds
CA2_sf <- bind_cols(CA2_sf, moran_CA2) %>% 
  mutate(moran_sig001 = ifelse(moran_p < 0.001, TRUE, FALSE),
         moran_sig01 = ifelse(moran_p < 0.01, TRUE, FALSE),
         moran_sig05 = ifelse(moran_p < 0.05, TRUE, FALSE))

# Plot the significant areas
plot(CA, col = ifelse(CA2_sf$moran_sig001 == TRUE, "red", "white"), 
     main = "p-val<0.001")
plot(CA, col = ifelse(CA2_sf$moran_sig01 == TRUE, "red", "white"),
     main = "p-val<0.01")
plot(CA, col = ifelse(CA2_sf$moran_sig05 == TRUE, "red", "white"), 
     main = "p-val<0.05")

# Plot clusters and significance
ggplot() +
  geom_sf(data = CA2_sf, aes(fill = msp_cat), color = "gray") +
  geom_sf(data = CA2_sf, aes(color = moran_sig001), fill = NA) +
  scale_color_manual(values = c("gray", "black")) +
  hrbrthemes::theme_ipsum()


# Local G* ----------------------------------------------------------------


# Update the neighbor list to include self and recreate the weight matrix, compute the local G z values and import into your data
gstar_CA2 <- localG(CA2_sf$hmcdr, nb2listw(include.self(CA2_queen))) %>% 
  as.vector()

# Create variables for differet significance levels
CA2_sf$gstar <- gstar_CA2

CA2_sf <- CA2_sf %>% 
  mutate(gstar_sig001 = case_when(gstar < -3.29 ~ "Cold",
                                  gstar > 3.29 ~ "Hot",
                                  TRUE ~ "Not sig"),
         gstar_sig01 = case_when(gstar < -2.58 ~ "Cold",
                                 gstar > 2.58 ~ "Hot",
                                 TRUE ~ "Not sig"),
         gstar_sig05 = case_when(gstar < -1.96 ~ "Cold",
                                 gstar > 1.96 ~ "Hot",
                                 TRUE ~ "Not sig"))

# Plot your hot and cold spots
CA2_sf %>% 
  dplyr::select(community, starts_with("gstar_sig")) %>% 
  st_drop_geometry() %>% 
  pivot_longer(-community, names_to = "sig_level", values_to = "z_score") %>% 
  left_join(CA2_sf %>% dplyr::select(community, geometry), ., by = "community") %>% 
  ggplot() +
  geom_sf(aes(fill = z_score), color = "gray") +
  facet_wrap(. ~ sig_level) +
  hrbrthemes::theme_ipsum()


# Binomial Distribution ---------------------------------------------------


# Probabilities
p_city <- sum(CA2_sf$nhmcd17) / sum(CA2_sf$population)
CA2_sf <- CA2_sf %>% 
  mutate(p_ca = nhmcd17 / population,
         expected_hmcd = p_city * population)

# Look at results
p_city
summary(CA2_sf$p_ca)
summary(CA2_sf$expected_hmcd)

# Plot
plot(CA2_sf$nhmcd17, CA2_sf$expected_hmcd, xlim = c(0, 80), ylim = c(0, 80))
abline(0, 1)

# Exceedence probabilities
CA2_sf <- CA2_sf %>% 
  mutate(p_exceed_exp = 1 - pbinom(expected_hmcd, population, p_ca),
         p_exceed_2exp = 1 - pbinom(2 * expected_hmcd, population, p_ca))

# Plot
ggplot() +
  geom_sf(data = CA2_sf, aes(fill = p_exceed_2exp), color = "gray") +
  scale_fill_gradient(low = muted("blue"), high = muted("red")) +
  hrbrthemes::theme_ipsum()

# SMR
CA2_sf <- CA2_sf %>% 
  mutate(smr = nhmcd17 / expected_hmcd)

# Plot
ggplot() +
  geom_sf(data = CA2_sf, aes(fill = smr), color = "gray") +
  scale_fill_gradient2(low = muted("blue"), mid = "white", midpoint = 1, high = muted("red")) +
  hrbrthemes::theme_ipsum()