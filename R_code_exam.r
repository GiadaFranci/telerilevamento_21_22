

# AUTHOR: Giada Franci

# This code processes images taken from sentinel_2A and sentinel_3B e 3A of the In.Cal.System (sources of data: EarthExplorer, Copernicus).
# The images from sentinel 2A include a little aria (1.385.673 ha) in Yarra Ranges National Park while The images from sentinel 3B e 3A include
# an LST (Land Surface Temperature) variable of a large aria in in South Australia .
# In the first part of my project, I calculated the trees that were lost due to the fires that took place in Australia.
# In the second part of my project I look at the LST, of the various periods, to understand how the disappearance of
# the trees altered the temperatures in the area douring the current period.

# Data include all the 10 mts resolution bands avaiable: RGB and NIR.

# NAMES OF THE USED BANDS:

#  T55HEU_20191119T001109_TCI_10m.jp2
#   T55HEU_20200227T001109_TCI_10m.jp2
#    T55HEU_20220122T001111_TCI_10m.jp2

#  T55HEU_20191119T001109_B02_10m.jp2
#   T55HEU_20191119T001109_B03_10m.jp2
#    T55HEU_20191119T001109_B04_10m.jp2
#     T55HEU_20191119T001109_B08_10m.jp2

#  T55HEU_20200227T001109_B02_10m.jp2
#   T55HEU_20200227T001109_B03_10m.jp2
#    T55HEU_20200227T001109_B04_10m.jp2
#     T55HEU_20200227T001109_B08_10m.jp2

#  T55HEU_20220122T001111_B02_10m.jp2
#   T55HEU_20220122T001111_B03_10m.jp2
#    T55HEU_20220122T001111_B04_10m.jp2
#     T55HEU_20220122T001111_B08_10m.jp2

#  S3A_SL_2_LST____20190531T233505.SEN3
#   S3A_SL_2_LST____20200102T233500.SEN3
#    S3B_SL_2_LST____20220130T231810.SEN3

# SUMMARY
# 1 # LAND COVER ANALYSIS
## 2 # LAND SURFACE TEMPERATURE ANALYSIS

# 1 # ANALISI DI LAND COVER
# # IMPORT AND DATA PREPARATION

library(raster)   
library(ggplot2)
library(RStoolbox)
library(patchwork)
library(RColorBrewer)

setwd("D:/fire_data")

# blue band 1
# green band 2
# red band 3
# infrared band 4

# 2019
rlist_19 <- list.files(pattern = "2019_B") 
rlist_19

import_19 <- lapply(rlist_19, raster)
import_19

tgr_19 <- stack(import_19)
tgr_19

plotRGB(tgr_19, r=4, g=3, b=2, stretch="lin")

# 2020
rlist_20 <- list.files(pattern = "2020_B") 
rlist_20

import_20 <- lapply(rlist_20, raster)
import_20

tgr_20 <- stack(import_20)
tgr_20

plotRGB(tgr_20, r=4, g=3, b=2, stretch="lin")

# 2022
rlist_22 <- list.files(pattern = "2022_B") 
rlist_22

import_22 <- lapply(rlist_22, raster)
import_22

tgr_22 <- stack(import_22)
tgr_22

plotRGB(tgr_22, r=4, g=3, b=2, stretch="lin")

#considering that i put the infrared band on red, everything that appears red in the image is vegetation

# ## LAND COVER

# I classify images by function unsuperClass

dev.off()

lg2019c <- unsuperClass(tgr_19, nClasses=4)
plot(lg2019c$map)
lg2019c
# class 1: clouds + bare ground
# class 2: clouds
# class 3: woodland 
# class 4: woodland 

lg2020c <- unsuperClass(tgr_20, nClasses=4)
plot(lg2020c$map)
lg2020c
# class 1: burned area
# class 2: bare ground
# class 3: woodland
# class 4: cultivated land

lg2022c <- unsuperClass(tgr_22, nClasses=4)
plot(lg2022c$map)
lg2022c
# class 1: clouds
# class 2: woodland
# class 3: bare ground+ ex burned area
# class 4: cultivated land  

# Visualizing this data has been useful to obtain maps where I have pixels of green areas (woods),
# so I can begin to calculate the occupied area and the proportion of pixels of green areas in the year 2019
# and compare it with the years 2020 and 2022.
# I can see this data from the frequency (measure of how many times a certain event happens).

# for this i use the function freq.

# ### FREQUENCY CALCULATION 

# total pixels 91982062 divided like this:

freq(lg2019c$map)
#value  count:
#classe 1  14343988 pixel
#classe 2    976465 pixel
#classe 3  36821671 pixel 
#classe 4  39839938 pixel 

freq(lg2020c$map)
#value  count:
#classe 1  31156016 pixel
#classe 2  10451394 pixel
#classe 3  46232134 pixel
#classe 4   4142518 pixel 

freq(lg2022c$map)
#value  count:
#classe 1    112573 pixel
#classe 2  42704248 pixel
#classe 3  34768672 pixel
#classe 4  14396569 pixel 

# Woods distribution and percentage for years 2019, 2020 and 2022.

tot19 <- 91982062
tot_green <- 36821671  + 39839938
# = 76661609
prop_green_19 <- 76661609 / tot19
percent_green_19 <- 76661609 * 100 / tot19
#percent 83,326547

tot20 <- 91982062
prop_green_20 <- 46232134 / tot20
percent_green_20 <- 46232134 * 100 / tot20
#percent 50,251543

tot22 <- 91982062
prop_green_22 <- 42704248 / tot22
percent_green_22 <- 42704248 * 100 / tot22
#percent 46,416944

# calculation of the total trees, lost throughout the Australia, taking into account
# the WWF data that report about 19 million hectares of woods impacted by the fire.

green_lost <- 76661609 - 46232134 
# = 30429475

percent_green_lost <- ( green_lost / 76661609) * 100
#percent 39,693231

# ### DATAFRAME

class <- c("2019", "2020")
green_perc_19_20 <- c(83.326547, 50.251543)
green_perc_19_22 <- c(83.326547, 46.416944)

# we build the table with the function data.frame

multitemporal <- data.frame(class, green_perc_19_20, green_perc_19_22)
multitemporal

View(multitemporal)

# #### PLOT

# plot of  2019 and 2020
ggplot(multitemporal, aes(x=class, y=green_perc_19_20, color=class)) +
  geom_bar(stat="identity", fill=c("red", "green"), width=0.5) +
  geom_text(aes(label=green_perc_19_20), vjust=-0.3, size=3.5)


# plot of 2019 and 2022
ggplot(multitemporal, aes(x=class, y=green_perc_19_22, color=class)) +
  geom_bar(stat="identity", fill=c("red", "green"), width=0.5) +
  geom_text(aes(label=green_perc_19_22), vjust=-0.3, size=3.5)


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



