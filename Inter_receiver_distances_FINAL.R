#Title: Inter-receiver distance check
#Author: Luc Visser 
#Date: 15-02-2024

#DISTANCE CALCULATIONS. 'rgdax' is said to not be a package. Jena used rgdal as a package but I couldn't find that in R v 4.3.2
rm(list=ls()) # clear workspace
knitr::opts_knit$set(root.dir = '/tmp')

# Loading packages
library(rgdax)
library(openxlsx) # Write excel sheets from R data sets
library(tidyr)
library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(sp)
library(sf)
library(ggmap)
library(geosphere)
library(knitr)
library(data.table)

# Import receiver location data
Receivers2023 <- read.csv(file= "Receivers2023.csv")
Deployment <- Receivers2023[c('Stationname', 'X_WGS84', 'Y_WGS84', 'DEPLOYMENT_START')]
Deployment <- plyr::rename(Deployment, c("Stationname" = "Station_name"))
Deployment <- plyr::rename(Deployment, c("Y_WGS84" = "Deploy_lat"))
Deployment <- plyr::rename(Deployment, c("X_WGS84" = "Deploy_lon"))
Deployment <- plyr::rename(Deployment, c("DEPLOYMENT_START" = "Deploy_date"))

# Select only relevant columns from deployment data set
Station_coordinates <- Deployment %>% 
  select(Station_name, Deploy_lon, Deploy_lat)

# Alter the coordinate data into spatial coordinates
coordinates(Station_coordinates) <- ~Deploy_lon + Deploy_lat
is.projected(Station_coordinates) # see if projection is defined, answer is NA
proj4string(Station_coordinates) <- CRS('+init=epsg:4326') # project as WGS coordinates
Station_coordinates

# Choose which projection to use. Here, the EPSG:32631 projected coordinate system is used.

# EPSG:32631 WGS 84 / UTM zone 31
Station_coordinates <- spTransform(Station_coordinates, CRS('+init=epsg:4326'))  
Station_coordinates
Station_coordinates_DF <- data.frame(Station_coordinates)

# Calculate distances between receivers and create distance matrix
Distances <- spDists(Station_coordinates)
Distances_DF <- data.frame(Distances) # Create a dataframe of the distance matrix

# Transform values to meters instead of kilometers
Distances_DF <- Distances_DF * 1000

# Change column and row names in the data frame
colnames(Distances_DF) <- c(Station_coordinates_DF$Station_name)
rownames(Distances_DF) <- c(Station_coordinates_DF$Station_name)

# Write excel and csv files of the inter-receiver distance matrix
write.xlsx(Distances_DF, "Receiver_distances_matrix.xlsx_correct", rowNames = TRUE)
write.csv(Distances_DF, "Receiver_distances_matrix_correct.csv")




