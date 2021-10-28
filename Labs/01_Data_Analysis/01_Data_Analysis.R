########################
### NAME::: Sean Connelly
########################

rm(list = ls())

# Load additional libraries
pacman::p_load(tidyverse, # for basic data manipulation, visualization
               ggpmisc, # for regression line info on ggplots
               patchwork, # for arranging ggplots in grids
               sf, # simple features for spatial
               summarytools, # for checking data frame characteristics
               here, # for relative file paths
               janitor) # for cleaning and tabulations

# Set working directory
setwd(paste0(here::here(), "/01_Data_Analysis"))



####################################
# Basic arthimetic
####################################

# Addition, Subtraction, Multiplication, Division
5 + 3
5 * 3
5 - 3
5 / 3

# Assign values to variables
a <- 5+3
a

# further operation on your created variables
a + 5 -3
b <- a + 5 -3
b
c <- a * b

# What is the value of c?
c

# what items are in memory?
ls()

# remove c
rm(c)

# what items are in memory now?
ls()



####################################
# Manually enter data
####################################

weight_lb <- c(120, 150, 210, 180) # in lbs
height <- c(150, 170, 155, 172) / 100 # in meters
# BMI: Ratio of weight (in kg) to height (in meteres) squared. 1lb is 0.454kg.
# convert the weight into kg and store it in a variable called weight_kg
# compute and summarize the BMI for the subjects. store the BMI values in a variable called bmi.


#..... (on your own)

weight_kg <- weight_lb * 0.454
BMI <- weight_kg / (height ^ 2) # should be meters squared based on formula
sort(BMI)



####################################
#------------------------------

class(weight_kg)
is.vector(weight_kg)
is.data.frame(weight_kg)
# is bmi a vector?
is.vector(BMI)
# is bmi a data.frame?
is.data.frame(BMI)

# create a data.frame object
ww <- data.frame(weight_kg, height, BMI)
ww # see the data you created
class(ww) # what is the class of the object
names(ww) # what are the variable names?
dim(ww) # dimensions of ww

# create a categorical variable - for example: height <1.6 or above
ww$height_cat <- ifelse(ww$height < 1.6, "L1.6", "G1.6")
ww$height_cat <- factor(ww$height_cat)
summary(ww)
ww$height_cat <- factor(ww$height_cat, levels = c("L1.6", "G1.6"))

# summarize the data
summary(ww)
ww

# look at differnt parts of the ww file
ww[,1]
ww[,1:2]
ww[1,1:2]
ww[1:2,]
ww[2:1,]

# order of a variable
order(ww$weight_kg) # what is the order of the weight_kg variable
# sort data
ww[order(ww$weight_kg),] # sort the data by the weight variable

# subset a data: create a new data called ww2 for only people shorter then 1.60
ww2 <- subset(ww, ww$height < 1.60)
# what is the dimension of ww2?
ww2
dim(ww2)

ls() # what is in memory?
rm("ww2")
ls() # what is in memory?
rm(list = ls())



####################################
#------------------------------
# FILL IN WITH YOUR OWN CODE
#------------------------------
# data women included in the R base package includes the height and weight for American women 30-39. Compute the BMI for these women. Summarize the data. Find out how many of the women are are Normal weight or overweight
#BMI Categories
# Underweight (UW) = <18.5
# Normal weight (NW) = 18.5–24.9 
# Overweight (OW)= 25–29.9 
# Obesity (OB)= BMI of 30 or greater
#------------------------------

data(women) # call the data
women # see the data

# See the metadata
?women # get help about the data
######
# Compute the BMI for the women in the data, summarize your findings. Write your code below######
######
#1. Summarize the data  ...
summary(women) 

#2. Compute BMI ...
women <- women %>% 
  mutate(BMI = (weight * 0.454) / ((height * 0.0254) ^ 2))

#3. Summarize BMI ...
summary(women$BMI)

###*****************
# Complete to this point and wait.################
###*****************
#################################
# Suppose it was found out that there was a data entry error.
# The weights for the 6th, 8th, and 9th persons were incorrect and should have been 192, 153, 193. Make these corrections and reevaluate the BMI.
#################################
women$weight
women$weight[c(6, 8, 9)]
women$weight[c(6, 8, 9)] <- c(192, 153, 193)

#*****************
# ON YOUR OWN - FILL IN WITH YOUR OWN CODE
#*****************
# recompute the bmi into a variable names bmi_corrected
women <- women %>% 
  mutate(BMI_corrected = (weight * 0.454) / ((height * 0.0254) ^ 2))

#create a categorical variable that indicates the BMI categories 
women <- women %>% 
  mutate(BMI_cat = case_when(BMI_corrected < 18.5 ~ 
                               "Underweight (UW)",
                             BMI_corrected >= 18.5 & 
                               BMI_corrected < 25 ~ 
                               "Normal weight (NW)",
                             BMI_corrected >= 25 & 
                               BMI_corrected < 30 ~ 
                               "Overweight (OW)",
                             BMI_corrected > 30 ~ 
                               "Obesity (OB)"))

# look at frequencies for BMI categories
women %>% 
  tabyl(BMI_cat)

#*****************STOP  HERE
####################################
#save the file to a csv file
write_csv(women, "Output/women_connelly.csv") #the file is now written to your working directory as a comma separated values (.csv) file
saveRDS(women, "Output/women_connelly.rds") #save a single object in an R format. A good idea once you have made changes to your data. It recalls the order of factors, etc. as you have set them.
#####################################
#*****************************************************


#Reading an external file
#####################################
#*****************************************************
#####################################
d1 <- read.csv("Output/women_connelly.csv", header = TRUE)#this works if you have specificed your working directory as the place where the file is
d2 <- readRDS("Output/women_connelly.rds")
#read.table is also commonly used. 
#?readRDS
summary(d1)
summary(d2)
#################################################
rm(list = ls())#remove all objects in memory
ls()
#################################################



#**********************************************************************************
#--------------------------------------------------------------------------------
###############################################
#creating barcharts
#read the vegetation data used in last weeks lecture

# setwd("~/Dropbox/Classes/UIC/Spring 2020/UPP 465/Lecture materials/Lecture 3/BlackBoard 2")
v <- read.csv("Raw_Data/lakeveg.csv")
v
names(v) <- "veg"
head(v)
vsum <- table(v$veg)
barplot(vsum)
barplot(sort(vsum))
barplot(rev(sort(vsum)))
barplot(rev(sort(vsum)),col = 2)
barplot(rev(sort(vsum)),col = heat.colors(5))
barplot(rev(sort(vsum)),col = terrain.colors(5))
barplot(rev(sort(vsum)),col = cm.colors(5))
rm(list = ls())



#**********************************************************************************
#--------------------------------------------------------------------------------
###############################################
#creating histograms
#read the dissolved oxygen data used in last weeks lecture
do <- read.csv("Raw_Data/dodata.csv", header = TRUE)
summary(do)
names(do) <- "DO"
hist(do$DO, col = "grey")
hist(do$DO, col = "grey", xlab = "Dissolved Oxyen", main = "")
abline(v = mean(do$DO), col = 2)
IQR(do$DO)
sd(do$DO)
#Public water supplies should have DO levels of 5mg/litre. How many fall short of that? 
summary(do$DO < 5)
table(do$DO < 5) / length(do$DO)
rm(do)



#**********************************************************************************
#--------------------------------------------------------------------------------
###############################################
#USArrests data  
#*****************************************************
#############################
data(USArrests) 
?USArrests
#This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973.
head(USArrests)

summary(USArrests)
pairs(USArrests)

row.names(USArrests)
which.max(USArrests$Murder)
row.names(USArrests)[10]
row.names(USArrests)[which.max(USArrests$Murder)]
row.names(USArrests)[which.min(USArrests$Murder)]
USArrests[order(USArrests$Murder),]
USArrests[rev(order(USArrests$Murder)),]

#--------------------------------------------------------------------------------
#which state had the highest rate of Assaults? 
#Evaluate and record your answer here in a comment.
# ANSWER: North Carolina had the highest rate of Assault arrests per 100K residents in 1973
USArrests %>% 
  tibble::rownames_to_column(., "State") %>% 
  arrange(desc(Assault)) %>% 
  slice(1)


rm(list=ls())
#**********************************************************************************
#Take some time and work on this
#**********************************************************************************
#--------------------------------------------------------------------------------
#Chicago ward population is given in the file ward_n_pop.csv
#data from http://media.apps.chicagotribune.com/ward-redistricting/index.html
w <- read.csv("Raw_Data/ward_n_pop.csv", header = TRUE)
head(w)
#1. Read the file and summarize the data into an object named w
summary(w)

#2. What was the total population in Chicago in 2000 and 2010
# ANSWER: 2000 = 2,896,016 and 2010 = 2,695,598
w %>% 
  summarize_at(vars("pop00", "pop10"), sum) %>% 
  mutate_all(scales::comma)

#3. Report the change in population in percentage terms
# ANSWER: Decreased by -6.92% citywide from 2000 to 2010
w %>% 
  summarize_at(vars("pop00", "pop10"), sum) %>% 
  mutate(pct_change_pop = ((pop10 - pop00) / pop00))

#4. Assess the percentage change in population per ward. How may saw a decline? How many saw an increase? 
# ANSWER: Decline = 42, Increase = 8
w <- w %>% 
  mutate(pct_change_pop = ((pop10 - pop00) / pop00),
         change_cat = ifelse(pct_change_pop > 0, "Increase", "Decline"))

# Frequencies by ward
w %>% 
  tabyl(change_cat)
  


#**********************************************************************************
#--------------------------------------------------------------------------------
########################################################
#Chicago 2007 Arson data
########################################################
library(sp)  
library(raster)
library(rgeos)

arson <- readRDS("Raw_Data/ChicagoArson2007.rds")

class(arson)
summary(arson)
#projection codes http://www.epsg-registry.org  
#epsg:26971 NAD83 / Illinois East
head(arson)
dim(arson)
par(mfrow = c(1, 1))
plot(arson)
plot(arson, cex = 0.5)
summary(factor(arson$Description))

# Convert to simple features object
arson_sf <- st_as_sf(arson)

#1. Summarize the data
summary(arson_sf)

#2. How many arson cases were recorded in 2007
# ANSWER: 710 
arson_sf %>% 
  st_drop_geometry() %>% 
  filter(Year == 2007) %>% 
  summarize(n())

#3. Do a bar chart of the description of the arson type (Description variable)
ggplot(arson_sf, aes(x = fct_rev(fct_infreq(Description)))) +
  geom_bar() +
  labs(x = "Description", y = "Cases") + 
  coord_flip() +
  theme_minimal()

#4. In how many cases was an arrest made?
# ANSWER: 93 out of 710 (13%) 
arson_sf %>% 
  st_drop_geometry() %>% 
  tabyl(Arrest)

#5. How many cases were classified as Domestic? 
# ANSWER: 33 out of 710 (4%)
arson_sf %>% 
  st_drop_geometry() %>% 
  tabyl(Domestic)

#6. Are arrests more common in Domestic cases?
# ANSWER: Yes - arrests were made in 60.6% of Domestic cases,
# versus 10.8% in non-Domestic cases
arson_sf %>% 
  st_drop_geometry() %>% 
  tabyl(Arrest, Domestic) %>% 
  adorn_totals("col") %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting() %>%
  adorn_ns() %>%
  adorn_title("combined")

#7. How many arson cases were there per ward?
ward_arson <- arson_sf %>% 
  st_drop_geometry() %>% 
  tabyl(Ward) %>% 
  rename("n_arson" = n)

#8. Merge the arson frequency to the ward file
w2 <- left_join(w,
                ward_arson %>% dplyr::select(-percent),
                by = c("ward" = "Ward"))

#9. Compute arson cases per 1000 population
w2 <- w2 %>% 
  mutate(arson_p1000 = (n_arson / pop10) * 1000)

########Read in the ward shape file
ward10 <- readRDS("Raw_Data/ward10.rds")

# Convert to simple features object
ward10_sf <- st_as_sf(ward10)

#1. Summarize the data
summary(ward10_sf)

#plot the wards and arson cases
ggplot() +
  geom_sf(data = ward10_sf, color = "black") +
  geom_sf(data = arson_sf, color = "red", alpha = 0.6) +
  theme_void()

names(ward10)
head(ward10)

#merge your ward arson counts to the ward 10 file
ward10v2_sf <- left_join(ward10_sf,
                         w2 %>% mutate(ward = as.character(ward)),
                         by = "ward") 

#plot the ward file showing the population change and arson cases per 1000
# Population change
p1_pop_change <- ggplot() +
  geom_sf(data = ward10v2_sf, aes(fill = pct_change_pop)) +
  scale_fill_gradient2(low = "Red", high = "Green",
                       labels = scales::percent) +
  labs(fill = "% Change\nin Population\n2000 - 2010") +
  theme_void()

# Arson cases
p2_arson <- ggplot() +
  geom_sf(data = ward10v2_sf, aes(fill = arson_p1000)) +
  scale_fill_distiller(palette = "Oranges", direction = 0) +
  labs(fill = "Arson cases per\n1,000 people\n2010") +
  theme_void()

# Show plots together
p1_pop_change + p2_arson

# Scatter plot and model
m1 <- lm(ward10v2_sf$arson_p1000 ~ ward10v2_sf$pct_change_pop)
summary(m1)

ggplot(ward10v2_sf, aes(x = pct_change_pop, y = arson_p1000)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.x = "right") +
  theme_minimal()

#Due Next week. Populate the code above. Write in comments to discuss what you observe. 
#Submit your findings from"
#1) The changes in the ward population.
#2) The analysis of the arson data 
#3) The final plots of arson and population change