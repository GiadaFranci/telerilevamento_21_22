
# AUTHOR: Giada Franci

# This code processes images taken from sentinel_2A and sentinel_3B e 3A of the In.Cal.System (sources of data: EarthExplorer, Copernicus).
# The images from sentinel 2A include a little aria (1.385.673 ha) in Yarra Ranges National Park while The images from sentinel 3B e 3A include
# an LST (Land Surface Temperature) variable of a large aria in in South Australia .
# In the first part of my project, I calculated the trees that were lost due to the fires that took place in Australia.
# In the second part of my project I look at the LST, of the various periods, to understand how the disappearance of
# the trees altered the temperatures in the area douring the current period.

# Data include all the 10 mts resolution bands avaiable: RGB.

# NAMES OF THE USED BANDS:

#  T55HEU_20191119T001109_TCI_10m.jp2
#   T55HEU_20200227T001109_TCI_10m.jp2
#    T55HEU_20220122T001111_TCI_10m.jp2

#  S3A_SL_2_LST____20190531T233505.SEN3
#   S3A_SL_2_LST____20200102T233500.SEN3
#    S3B_SL_2_LST____20220130T231810.SEN3

# SUMMARY
# 1 # LAND COVER ANALYSIS
## 2 # LAND SURFACE TEMPERATURE ANALYSIS

# 1 # ANALISI DI LAND COVER
# # IMPORT AND DATA PREPARATION

library(raster)   
library(viridis)
library(ggplot2)
library(RStoolbox)
library(patchwork)

setwd("D:/fire_data")

#blu banda 1
#verde banda 2
#rosso banda 3

lg2019 <- brick("2019.jpg")
lg2019

lg2020 <- brick("2020.jpg")
lg2020

lg2022 <- brick("2022.jpg")
lg2022

# collect images in the same window

pg19 <- ggRGB(lg2019, r=1, g=2, b=3, stretch="lin")
pg20 <- ggRGB(lg2020, r=1, g=2, b=3, stretch="lin")
pg22 <- ggRGB(lg2022, r=1, g=2, b=3, stretch="lin")

pg19/pg20/pg22

# ## LAND COVER

# I classify images by function unsuperClass

dev.off()

lg2019c <- unsuperClass(lg2019, nClasses=4)
plot(lg2019c$map)
lg2019c
# class 1: clouds
# class 2: bare ground
# class 3: cultivated land
# class 4: woodland

lg2020c <- unsuperClass(lg2020, nClasses=4)
plot(lg2020c$map)
lg2020c
# class 1: bare ground
# class 2: burned area
# class 3: woodland
# class 4: cultivated land

lg2022c <- unsuperClass(lg2022, nClasses=4)
plot(lg2022c$map)
lg2022c
# class 1: cultivated land + recovery area
# class 2: bare ground
# class 3: clouds
# class 4: woodland

# Visualizing this data has been useful to obtain maps where I have pixels of green areas (woods),
# so I can begin to calculate the occupied area and the proportion of pixels of green areas in the year 2019
# and compare it with the years 2020 and 2022.
# I can see this data from the frequency (measure of how many times a certain event happens).

# for this i use the function freq.

# ### FREQUENCY CALCULATION 

# total pixels 92001420 divided like this:

freq(lg2019c$map)
#value  count:
#classe 1   818023 pixel
#classe 2 10563469 pixel
#classe 3 20264754 pixel 
#classe 4 60355174 pixel 

freq(lg2020c$map)
#value  count:
#classe 1  3397852 pixel
#classe 2 29784744 pixel
#classe 3 43757344 pixel
#classe 4 15061480 pixel 

freq(lg2022c$map)
#value  count:
#classe 1 18657949 pixel
#classe 2  5995202 pixel
#classe 3   288324 pixel
#classe 4 67059945 pixel 

# Woods distribution and percentage for years 2019, 2020 and 2022.

tot19 <- 92001420
prop_green_19 <- 60355174 / tot19
percent_green_19 <- 60355174 * 100 / tot19
#percent 65,602437

tot20 <- 92001420
prop_green_20 <- 43757344 / tot20
percent_green_20 <- 43757344 * 100 / tot20
#percent 47,561596

tot22 <- 92001420
prop_green_22 <- 67059945 / tot22
percent_green_22 <- 67059945 * 100 / tot22
#percent 72,890119

# burned area in 2020

tot20 <- 92001420
prop_fire_20 <- 29784744 / tot20
percent_fire_20 <- 29784744 * 100 / tot20
#percent 32,374222

# calculation of the total trees, lost throughout the Australia, taking into account
# the WWF data that report about 19 million hectares of woods impacted by the fire.

green_lost <- 60355174 - 43757344 
# = 16597830

percent_green_lost <- ( green_lost / 60355174) * 100
#percent 27,500260

# ### DATAFRAME

class <- c("2019", "2020")
green_perc_19_20 <- c(65.602437, 47.561596)
green_perc_19_22 <- c(65.602437, 72.890119)

# we build the table with the function data.frame

multitemporal <- data.frame(class, green_perc_19_20, green_perc_19_22)
multitemporal

View(multitemporal)

# #### PLOT

# plot of  2019 and 2020
ggplot(multitemporal, aes(x=class, y=green_perc_19_20, color=class)) +
  geom_bar(stat="identity", fill="orange")


# plot of 2019 and 2022
ggplot(multitemporal, aes(x=class, y=green_perc_19_22, color=class)) +
  geom_bar(stat="identity", fill="green")


## 2 # LAND SURFACE TEMPERATURE ANALYSIS

# we use the variable LST (Land Surface Temperature) of Copernicus
# to see how much the temperature in Australia has changed
# before after and during the fire

lst2019 <- raster("lst2019.tif")
lst2020 <- raster("lst2020.tif")
lst2022 <- raster("lst2022.tif")

# we create a color palet

cl <- colorRampPalette(c("blue", "light blue", "pink", "red")) (100)

# ## I bring the images all together

# I use the function lapply.
# this function takes a list of files and applies the same command to all the file.

# 1) I create the list of files with the function list.files

# with the argument pattern we explain the common feature 
# of the fail that we want to import

rlist <- list.files(pattern = "lst")
rlist

# 2) I apply the raster function using the lapply function

import <- lapply(rlist, raster)
import

# 3) we create the stak that puts together the various layers.

tgr <- stack(import)
tgr

# tgr is a rasterstack it is the same thing of a rasterbrick, but the rasterbrick is
# the import of a whole satellite image, instead of rasterstack that we have created
# with the function stack.

plot(tgr, col=cl)


