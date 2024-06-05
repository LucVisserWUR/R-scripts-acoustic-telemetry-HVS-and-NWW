title: "Sluice management type and tide analysis at HVS" 
Author: "Luc Visser"
date: "2024-04-16"

# Load correct packages
library(tidyr)
library(dplyr)
library(ggtext)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(openxlsx)
library(readxl)

# Definitions used throughout the script:
  # Spui = Discharge. Kier = Water inlet, ajar. Getij = Tide. 

# Clean worksheet and keep the detection efficiency calculations data sets
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)
knitr::opts_chunk$set(echo = TRUE)

# Load data file with sluice management type data
Getij_Spui_Kier_data <- read.csv("Env_Getij_Spui_Kier_HVS.csv")

# Rename column names of GetijVan and GetijTot, which indicate when high tide started and ended
Getij_Spui_Kier_data <- Getij_Spui_Kier_data %>% # GetijVan
  dplyr::rename(GetijVan = GetijVan_dd_mm_yyyy_HH_MM)

Getij_Spui_Kier_data <- Getij_Spui_Kier_data %>% # GetijTot
  dplyr::rename(GetijTot = GetijTot_dd_mm_yyyy_HH_MM)

# Create copy of GetijVan and GetijTot to couple the times in the Spui and Kier columns to date later on
Getij_Spui_Kier_data$GetijVan_Date <- Getij_Spui_Kier_data$GetijVan
Getij_Spui_Kier_data$GetijTot_Date <-Getij_Spui_Kier_data$GetijTot

# Put these copied values in a column where only date is shown without hour or minute
Getij_Spui_Kier_data$GetijVan_Date <- as.POSIXct(Getij_Spui_Kier_data$GetijVan_Date, format = '%d/%m/%Y %H:%M') # Does not change the timezone, only the formatting
Getij_Spui_Kier_data$GetijVan_Date <- format(Getij_Spui_Kier_data$GetijVan_Date, format = '%Y-%m-%d')

Getij_Spui_Kier_data$GetijTot_Date <- as.POSIXct(Getij_Spui_Kier_data$GetijTot_Date, format = '%d/%m/%Y %H:%M')
Getij_Spui_Kier_data$GetijTot_Date <- format(Getij_Spui_Kier_data$GetijTot_Date, format = '%Y-%m-%d')

# Order this data set to make it easy to see how date will be merged with time (HH:MM)
Getij_Spui_Kier_data <- Getij_Spui_Kier_data %>%
  dplyr::select(GetijVan, GetijTot, GetijVan_Date, GetijTot_Date, SpuienVan_HH_MM, SpuienTot_HH_MM, SpuienDuur_HH_MM, KierenVan_HH_MM, KierenTot_HH_MM, KierenDuur_HH_MM, Spuivolume_brknd_m3, Inlaatvolume_brknd_m3)

# Format the Spui and Kier data (for which only the time was given)
Getij_Spui_Kier_data$GetijVan_Date <- format(Getij_Spui_Kier_data$GetijVan_Date, format = '%Y-%m-%d')
Getij_Spui_Kier_data$GetijTot_Date <- format(Getij_Spui_Kier_data$GetijTot_Date, format = '%Y-%m-%d')

Getij_Spui_Kier_data$SpuienVan_HH_MM <- format(Getij_Spui_Kier_data$SpuienVan_HH_MM, format = '%H:%M') # SpuiVan
Getij_Spui_Kier_data$SpuienTot_HH_MM <- format(Getij_Spui_Kier_data$SpuienTot_HH_MM, format = '%H:%M') # SpuiTot
Getij_Spui_Kier_data$KierenVan_HH_MM <- format(Getij_Spui_Kier_data$KierenVan_HH_MM, format = '%H:%M') # KierVan
Getij_Spui_Kier_data$KierenTot_HH_MM <- format(Getij_Spui_Kier_data$KierenTot_HH_MM, format = '%H:%M') # KierTot

# Merge Getij dates and Spui time. It must be done now. Only can can we convert Spui and Kier data to UTC format
Getij_Spui_Kier_data$SpuienVan <- paste(paste(Getij_Spui_Kier_data$GetijVan_Date), format(Getij_Spui_Kier_data$SpuienVan_HH_MM), sep = " ") # SpuiVan
Getij_Spui_Kier_data$SpuienTot <- paste(paste(Getij_Spui_Kier_data$GetijTot_Date), format(Getij_Spui_Kier_data$SpuienTot_HH_MM), sep = " ") # SpuiTot
Getij_Spui_Kier_data$KierenVan <- paste(paste(Getij_Spui_Kier_data$GetijVan_Date), format(Getij_Spui_Kier_data$KierenVan_HH_MM), sep = " ") # KierVan
Getij_Spui_Kier_data$KierenTot <- paste(paste(Getij_Spui_Kier_data$GetijTot_Date), format(Getij_Spui_Kier_data$KierenTot_HH_MM), sep = " ") # KierTot

# Spuien en Kieren does not always occur (Especially Kieren is more rare). Remove all the dates when there was no Spuien or Kieren during that moment
Getij_Spui_Kier_data$SpuienVan_HH_MM[trimws(Getij_Spui_Kier_data$SpuienVan_HH_MM) == ""] <- NA # Apparently the blank spaces in the SpuienVan_HH_MM were filled with 'whitespace characters' which were removed using the trimwc argument
Getij_Spui_Kier_data$SpuienVan[is.na(Getij_Spui_Kier_data$SpuienVan_HH_MM)] <- NA

Getij_Spui_Kier_data$SpuienTot_HH_MM[trimws(Getij_Spui_Kier_data$SpuienTot_HH_MM) == ""] <- NA
Getij_Spui_Kier_data$SpuienTot[is.na(Getij_Spui_Kier_data$SpuienTot_HH_MM)] <- NA

Getij_Spui_Kier_data$KierenVan_HH_MM[trimws(Getij_Spui_Kier_data$KierenVan_HH_MM) == ""] <- NA
Getij_Spui_Kier_data$KierenVan[is.na(Getij_Spui_Kier_data$KierenVan_HH_MM)] <- NA

Getij_Spui_Kier_data$KierenTot_HH_MM[trimws(Getij_Spui_Kier_data$KierenTot_HH_MM) == ""] <- NA
Getij_Spui_Kier_data$KierenTot[is.na(Getij_Spui_Kier_data$KierenTot_HH_MM)] <- NA

# Make a selection and order of the columns that should be included
Getij_Spui_Kier_data <- Getij_Spui_Kier_data %>%
  dplyr::select(GetijVan, GetijTot, GetijVan_Date, GetijTot_Date, SpuienVan_HH_MM, SpuienVan, SpuienTot_HH_MM, SpuienTot, SpuienDuur_HH_MM, KierenVan_HH_MM, KierenVan, KierenTot_HH_MM, KierenTot, KierenDuur_HH_MM, Spuivolume_brknd_m3, Inlaatvolume_brknd_m3)

# Set data in correct format. DateTime is in summer & wintertime. Transform to CET  / GMT -  1 and then to UTC time
Getij_Spui_Kier_data$GetijVan <- as.POSIXct(Getij_Spui_Kier_data$GetijVan, format = '%d/%m/%Y %H:%M') # Tides
Getij_Spui_Kier_data$GetijTot <- as.POSIXct(Getij_Spui_Kier_data$GetijTot, format = '%d/%m/%Y %H:%M')

Getij_Spui_Kier_data$SpuienVan <- as.POSIXct(Getij_Spui_Kier_data$SpuienVan, format = '%Y-%m-%d %H:%M') # Spuien 
Getij_Spui_Kier_data$SpuienTot <- as.POSIXct(Getij_Spui_Kier_data$SpuienTot, format = '%Y-%m-%d %H:%M')
Getij_Spui_Kier_data$KierenVan <- as.POSIXct(Getij_Spui_Kier_data$KierenVan, format = '%Y-%m-%d %H:%M') # Kieren
Getij_Spui_Kier_data$KierenTot <- as.POSIXct(Getij_Spui_Kier_data$KierenTot, format = '%Y-%m-%d %H:%M')

# Reformat the DateTime (now given in summer / winter time) to CET timezone without seasonal transitions
Getij_Spui_Kier_data$GetijVan_CET <- as.POSIXct(Getij_Spui_Kier_data$GetijVan, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1") # Tides
Getij_Spui_Kier_data$GetijTot_CET <- as.POSIXct(Getij_Spui_Kier_data$GetijTot, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")

Getij_Spui_Kier_data$SpuienVan_CET <- as.POSIXct(Getij_Spui_Kier_data$SpuienVan, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1") # Spuien & Kieren
Getij_Spui_Kier_data$SpuienTot_CET <- as.POSIXct(Getij_Spui_Kier_data$SpuienTot, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Getij_Spui_Kier_data$KierenVan_CET <- as.POSIXct(Getij_Spui_Kier_data$KierenVan, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Getij_Spui_Kier_data$KierenTot_CET <- as.POSIXct(Getij_Spui_Kier_data$KierenTot, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")

# Reformat the CET time to UTC time.
Getij_Spui_Kier_data$GetijVan_UTC <- with_tz(Getij_Spui_Kier_data$GetijVan_CET, tz = "UTC")  # Tides
Getij_Spui_Kier_data$GetijTot_UTC <- with_tz(Getij_Spui_Kier_data$GetijTot_CET, tz = "UTC") 

Getij_Spui_Kier_data$SpuienVan_UTC <- with_tz(Getij_Spui_Kier_data$SpuienVan_CET, tz = "UTC")  # Spuien
Getij_Spui_Kier_data$SpuienTot_UTC <- with_tz(Getij_Spui_Kier_data$SpuienTot_CET, tz = "UTC") 
Getij_Spui_Kier_data$KierenVan_UTC <- with_tz(Getij_Spui_Kier_data$KierenVan_CET, tz = "UTC")  # Kieren
Getij_Spui_Kier_data$KierenTot_UTC <- with_tz(Getij_Spui_Kier_data$KierenTot_CET, tz = "UTC") 

# Select only relevant columns. Both summer/winter cet timezone and UTC timezone are included for comparison.
Getij_Spui_Kier_data <- Getij_Spui_Kier_data %>%
  dplyr::select(GetijVan, GetijVan_UTC, GetijTot, GetijTot_UTC, GetijVan_Date, GetijTot_Date, SpuienVan_HH_MM, SpuienVan, SpuienVan_UTC, SpuienTot_HH_MM, SpuienTot, SpuienTot_UTC ,SpuienDuur_HH_MM, KierenVan, KierenVan_UTC, KierenTot, KierenTot_UTC, KierenDuur_HH_MM, Spuivolume_brknd_m3, Inlaatvolume_brknd_m3)

# Select only relevant times because these will later on be linked to the environmental data
relevant_time_min_utc <- as.POSIXct("2023-01-26 11:59:00", tz = "UTC") # These start and end times are based on twelve / three hourly observed_expected detections
relevant_time_max_utc <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")
Getij_Spui_Kier_data_relevant <- Getij_Spui_Kier_data %>%
  filter(GetijVan_UTC > relevant_time_min_utc)
Getij_Spui_Kier_data_relevant <- Getij_Spui_Kier_data_relevant %>%
  filter(GetijTot_UTC < relevant_time_max_utc)

# First create GetijVan_UTC_Date and GetijTot_UTC_Date columns. Getij dates will be merged with Spui and Kier data again because some dates changed after converting the time to UTC format
Getij_Spui_Kier_data_relevant$GetijVan_UTC_Date <- format(Getij_Spui_Kier_data_relevant$GetijVan_UTC, format = '%d/%m/%Y', tz = 'UTC')
Getij_Spui_Kier_data_relevant$GetijTot_UTC_Date <- format(Getij_Spui_Kier_data_relevant$GetijTot_UTC, format = '%d/%m/%Y', tz = 'UTC')

# SOLVING THE DATE ISSUE WITH Spui_Van, Spui_Tot, Kier_Van and Kier_Tot data

# We needed the merging with dates before for Spui and Kier data to convert those to UTC. However, some of these are incorrect. 
# It can happen that GetijTot > 00:00:00 and thus a next date, but SpuiTot < 00:00:00 and should be the same date as SpuiVan
# We now assumed that SpuiVan$Date = GetijVan$Date and SpuiTot$Date = GetijTot$Date, which is untrue

# In order to ressolve this issue we have to seperate the Minutes and Hours from the SpuiVan, SpuiTot, KierVan and KierTot data
Getij_Spui_Kier_data_relevant$SpuienVan_UTC_HM <- format(Getij_Spui_Kier_data_relevant$SpuienVan_UTC, format = '%H:%M:%S', tz = 'UTC')
Getij_Spui_Kier_data_relevant$KierenVan_UTC_HM <- format(Getij_Spui_Kier_data_relevant$KierenVan_UTC, format = '%H:%M:%S', tz = 'UTC')
Getij_Spui_Kier_data_relevant$SpuienTot_UTC_HM <- format(Getij_Spui_Kier_data_relevant$SpuienTot_UTC, format = '%H:%M:%S', tz = 'UTC')
Getij_Spui_Kier_data_relevant$KierenTot_UTC_HM <- format(Getij_Spui_Kier_data_relevant$KierenTot_UTC, format = '%H:%M:%S', tz = 'UTC')

# Create two subsets: one in which it can be assumed that the date equals the GetijVan date and one where you check the data in which you deal with date issue Spui and Kier data

# This is dependent on whether GetijVan equals GetijTot. If it does not, there is a chance that the Date of SpuiTot_UTC and KierTot_UTC is incorrect and that can be checked in the way below
Getij_Spui_Kier_data_EQUAL_GetijVan_Tot <- subset(Getij_Spui_Kier_data_relevant, GetijVan_UTC_Date == GetijTot_UTC_Date) # From this one you can assume that spuienVan and spuienTot date equals the GetijVan date
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot <- subset(Getij_Spui_Kier_data_relevant, GetijVan_UTC_Date != GetijTot_UTC_Date) # This subset requires further exploring

# First: create new Spui_Van, Spui_Tot, Kier_Van and Kier_Tot for Getij_Spui_Kier_data_EQUAL_GetijVan_Tot subset
# Always link the time of Spui and Kier to the date of Getij_Van

# Spuien Van
Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$SpuienVan_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$GetijVan_UTC_Date, Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$SpuienVan_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)

# Spuien Tot
Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$SpuienTot_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$GetijVan_UTC_Date, Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$SpuienTot_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)

# Kieren Van
Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$KierenVan_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$GetijVan_UTC_Date, Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$KierenVan_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)

# Kieren Tot
Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$KierenTot_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$GetijVan_UTC_Date, Getij_Spui_Kier_data_EQUAL_GetijVan_Tot$KierenTot_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)


# Now that everything is in order, let's select only the relevant columns
Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC <- Getij_Spui_Kier_data_EQUAL_GetijVan_Tot %>%
  select(GetijVan_UTC, GetijTot_UTC, SpuienVan_UTC_correct, SpuienTot_UTC_correct, SpuienDuur_HH_MM, KierenVan_UTC_correct, KierenTot_UTC_correct, KierenDuur_HH_MM, Spuivolume_brknd_m3, Inlaatvolume_brknd_m3)

# Rename the correct Spui_tot and Kier_tot columns
colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC) == "SpuienVan_UTC_correct"] <- "SpuienVan_UTC"
colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC) == "KierenVan_UTC_correct"] <- "KierenVan_UTC"
colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC) == "SpuienTot_UTC_correct"] <- "SpuienTot_UTC"
colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC) == "KierenTot_UTC_correct"] <- "KierenTot_UTC"

# Now create new Spui_Van, Spui_Tot, Kier_Van and Kier_Tot for Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot subset. Needs more attention. 

# When for example SpuiTot_UTC_HM exceeds 00:00:00, the data should indeed go to the next day. When it does not however, the same date should be kept.
# The date of GetijVan_UTC is always day 1. The date of GetijTot_UTC can be day 2 if the time exceed 00:00:00.

# It could be expected that when SpuiTot <00:00:00 and > 12:00:00, it should be GetijVan_UTC_Date. When SpuiTot > 00:00:00 and < 12:00:00 it should be GetijTot_UTC_Date
# Therefore, create a new column that provides information if SpuiTot should be given a GetijVan or GetijTot date based on time of SpuiTot

# This code worked almost flawlessly, but sometimes when Spuien started before GetijVan before 12:00:00 it gave wrong values for the new GetijVan. Altering the 12:00:00 to 09:00:00 as minimum limit for SpuiVan and KierVan ressolved this issue!

# Spuien Van. 
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienVan_Date_check <- ifelse(
  format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienVan_UTC_HM, format = '%H:%M:%S', tz = 'UTC') > "09:00:00" |
    format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienVan_UTC_HM, format = '%H:%M:%S', tz = 'UTC') < "00:00:00",
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijVan_UTC_Date,
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijTot_UTC_Date
)

# Kieren Van
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenVan_Date_check <- ifelse(
  format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenVan_UTC_HM, format = '%H:%M:%S', tz = 'UTC') > "09:00:00" |
    format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenVan_UTC_HM, format = '%H:%M:%S', tz = 'UTC') < "00:00:00",
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijVan_UTC_Date,
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijTot_UTC_Date
)

# Spuien Tot
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienTot_Date_check <- ifelse(
  format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienTot_UTC_HM, format = '%H:%M:%S', tz = 'UTC') > "12:00:00" |
    format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienTot_UTC_HM, format = '%H:%M:%S', tz = 'UTC') < "00:00:00",
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijVan_UTC_Date,
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijTot_UTC_Date
)

# Kieren Tot
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenTot_Date_check <- ifelse(
  format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenTot_UTC_HM, format = '%H:%M:%S', tz = 'UTC') > "12:00:00" |
    format(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenTot_UTC_HM, format = '%H:%M:%S', tz = 'UTC') < "00:00:00",
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijVan_UTC_Date,
  Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$GetijTot_UTC_Date
)


# Now merge the checked dates (SpuienTot_Date_check e.g.) with the UTC times (SpuienTot_UTC_HM e.g.). These are almost the correct DateTime values for the Spui and Kier tot data

# Spuien Van
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienVan_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienVan_Date_check, Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienVan_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)

# Spuien Tot
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienTot_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienTot_Date_check, Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$SpuienTot_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)


# Kieren Van
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenVan_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenVan_Date_check, Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenVan_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)

# Kieren Tot
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenTot_UTC_correct <- as.POSIXct(
  paste(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenTot_Date_check, Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot$KierenTot_UTC_HM),
  format = '%d/%m/%Y %H:%M:%S',
  tz = 'UTC'
)

# Now that everything is checked and in order, let's select only the relevant columns
Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC <- Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot %>%
  select(GetijVan_UTC, GetijTot_UTC, SpuienVan_UTC_correct, SpuienTot_UTC_correct, SpuienDuur_HH_MM, KierenVan_UTC_correct, KierenTot_UTC_correct, KierenDuur_HH_MM, Spuivolume_brknd_m3, Inlaatvolume_brknd_m3)


# Rename the correct Spui_tot and Kier_tot columns
colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC) == "SpuienVan_UTC_correct"] <- "SpuienVan_UTC"
colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC) == "KierenVan_UTC_correct"] <- "KierenVan_UTC"
colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC) == "SpuienTot_UTC_correct"] <- "SpuienTot_UTC"
colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC)[colnames(Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC) == "KierenTot_UTC_correct"] <- "KierenTot_UTC"


# Merge the two subsets (Equal GetijVan - Tot and Different GetijVan_Tot) back together to form the complete Getij, Spui and Kier dataset
Getij_Spui_Kier_data_UTC_total <- rbind(Getij_Spui_Kier_data_EQUAL_GetijVan_Tot_UTC, Getij_Spui_Kier_data_DIFFERENT_GetijVan_Tot_UTC) 
Getij_Spui_Kier_data_UTC_total <- arrange(Getij_Spui_Kier_data_UTC_total, GetijVan_UTC)


# Manually change the values that could not be handled with the dataset 
Getij_Spui_Kier_data_UTC_total_final <- Getij_Spui_Kier_data_UTC_total %>%
  dplyr::mutate(SpuienVan_UTC = case_when(
    row_number() == 9 ~ as.POSIXct("2023-01-30 22:54:00", tz = "UTC"),
    row_number() == 40 ~ as.POSIXct("2023-02-15 23:56:00", tz = "UTC"),
    row_number() == 123 ~ as.POSIXct("2023-03-30 22:28:00", tz = "UTC"),
    row_number() == 181 ~ as.POSIXct("2023-04-29 23:15:00", tz = "UTC"),
    row_number() == 380 ~ as.POSIXct("2023-08-10 23:23:00", tz = "UTC"),
    row_number() == 411 ~ as.POSIXct("2023-08-26 23:16:00", tz = "UTC"),
    row_number() == 467 ~ as.POSIXct("2023-09-24 23:11:00", tz = "UTC"),
    row_number() == 579 ~ as.POSIXct("2023-11-21 22:47:00", tz = "UTC"),
    row_number() == 608 ~ as.POSIXct("2023-12-06 23:17:00", tz = "UTC"),
    row_number() == 610 ~ as.POSIXct("2023-12-07 23:56:00", tz = "UTC"),
    row_number() == 668 ~ as.POSIXct("2024-01-06 23:38:00", tz = "UTC"),
    row_number() == 695 ~ as.POSIXct("2024-01-20 23:47:00", tz = "UTC"),
    TRUE ~ SpuienVan_UTC
  ))

# Also one case for one Kier moment
Getij_Spui_Kier_data_UTC_total_final <- Getij_Spui_Kier_data_UTC_total_final %>%
  dplyr::mutate(KierenTot_UTC = case_when(
    row_number() == 208 ~ as.POSIXct("2023-05-14 12:07:00", tz = "UTC"),
    TRUE ~ KierenTot_UTC
  ))

# This is the final dataset. Rename it and create an excel and csv file of it 

write.csv(Getij_Spui_Kier_data_UTC_total_final, file = "Getij_Spui_Kier_UTC_relevant_correct.csv", row.names = FALSE)
write.xlsx(Getij_Spui_Kier_data_UTC_total_final, file = "Getij_Spui_Kier_UTC_relevant_CHECK_22_04_2024.xlsx")

# Create dataframes with just the relevant time bins in there.

# 1 hour bins, especially designed to fit exactly to Enviro_combined_DE_1hour_HVS and Enviro_combined_DE_1hour_NWW
Getij_Spui_Kier_1hour_sequence <- seq(
  from = as.POSIXct("2023-01-26 23:00:00", tz = "UTC"),
  to = as.POSIXct("2024-01-25 08:00:00", tz = "UTC"),
  by = "hour"
)

Getij_Spui_Kier_1hour <- data.frame(Hour = Getij_Spui_Kier_1hour_sequence)


# Merge these hour keys with the Getij, Spui and Kier file. 
# The script below will create spui or kier moments per hour based on the ranges in Getij_Spui_Kier_data_UTC_total_final.

# Create a Getij_Spui_Kier_data_UTC_total_final that allows for Spui and Kier moments happening in the same hour: these are called 'transition' periods.
# Do so by rounding of SpuiVan and KierVan to the whole hour. E.g: SpuiVan 22:23:00 will become 22:00:00.
# If this is not done, SpuiVan will be read from 23:00:00 onwards, leading to invalid spui and kier periods.
Getij_Spui_Kier_data_UTC_total_Trans <- Getij_Spui_Kier_data_UTC_total_final 
Getij_Spui_Kier_data_UTC_total_Trans$SpuienVan_UTC <- floor_date(Getij_Spui_Kier_data_UTC_total_Trans$SpuienVan_UTC, "hour")
Getij_Spui_Kier_data_UTC_total_Trans$SpuienTot_UTC<- as.POSIXct(Getij_Spui_Kier_data_UTC_total_Trans$SpuienTot_UTC, format = '%d/%m/%Y %H:%M:%S', tz = "UTC")
Getij_Spui_Kier_data_UTC_total_Trans$KierenVan_UTC <- floor_date(Getij_Spui_Kier_data_UTC_total_Trans$KierenVan_UTC, "hour")
Getij_Spui_Kier_data_UTC_total_Trans$KierenTot_UTC<- as.POSIXct(Getij_Spui_Kier_data_UTC_total_Trans$KierenTot_UTC, format = '%d/%m/%Y %H:%M:%S', tz = "UTC")

# SPUIEN 
Getij_Spui_Kier_1hour$Spui <- NA
Getij_Spui_Kier_1hour$Hour <- as.POSIXct(Getij_Spui_Kier_1hour$Hour, format = '%d/%m/%Y %H:%M:%S', tz = "UTC")


# Make the process of finding any suitable spui range a loop process for every hour in the Getij_Spui_Kier_1hour dataset 
for (i in seq_along(Getij_Spui_Kier_1hour$Hour)) { # Make it a loop for every hour in the file containing all the hours
  hour <- Getij_Spui_Kier_1hour$Hour[i]
  
  # Check if hour falls within any range in Getij_Spui_Kier_data_UTC_total
  if (any(!is.na(Getij_Spui_Kier_data_UTC_total_Trans$SpuienVan_UTC) & # First two lines inserted to leave out NA values
          !is.na(Getij_Spui_Kier_data_UTC_total_Trans$SpuienTot_UTC) &
          hour >= Getij_Spui_Kier_data_UTC_total_Trans$SpuienVan_UTC & # For every hour it is checked if it falls within any Spui range
          hour <= Getij_Spui_Kier_data_UTC_total_Trans$SpuienTot_UTC)) {
    Getij_Spui_Kier_1hour$Spui[i] <- "Spui"
  }
}

# KIEREN 
Getij_Spui_Kier_1hour$Kier <- NA
Getij_Spui_Kier_1hour$Hour <- as.POSIXct(Getij_Spui_Kier_1hour$Hour, format = '%d/%m/%Y %H:%M:%S', tz = "UTC")


# Make the process of finding any suitable spui range a loop process for every hour in the Getij_Spui_Kier_1hour dataset
for (i in seq_along(Getij_Spui_Kier_1hour$Hour)) { # Make it a loop for every hour in the file containing all the hours
  hour <- Getij_Spui_Kier_1hour$Hour[i]
  
  # Check if hour falls within any range in Getij_Spui_Kier_data_UTC_total
  if (any(!is.na(Getij_Spui_Kier_data_UTC_total_Trans$KierenVan_UTC) & 
          !is.na(Getij_Spui_Kier_data_UTC_total_Trans$KierenTot_UTC) &
          hour >= Getij_Spui_Kier_data_UTC_total_Trans$KierenVan_UTC & 
          hour <= Getij_Spui_Kier_data_UTC_total_Trans$KierenTot_UTC)) {
    Getij_Spui_Kier_1hour$Kier[i] <- "Kier"
  }
}

# Now create one column containing either Spui, Kier or transition values when both Spui and Kier occur
Getij_Spui_Kier_1hour$Spui_Kier <- NA

# We can only be sure of Spuien and Kieren. Sometimes, the file says there is no spuien or kieren, but there is still water coming through the sluices
# We can therefore only state that HVS is closed when Avg_sluice_opening == 0.0
# Add values for 'Closed' later in the Enviromental variables file

Getij_Spui_Kier_1hour$Spui_Kier[Getij_Spui_Kier_1hour$Spui == "Spui"] <- "Spui"
Getij_Spui_Kier_1hour$Spui_Kier[Getij_Spui_Kier_1hour$Kier == "Kier"] <- "Kier"
Getij_Spui_Kier_1hour$Spui_Kier[Getij_Spui_Kier_1hour$Spui == "Spui" & Getij_Spui_Kier_1hour$Kier == "Kier"] <- "Transition"


# Write an excel file to check this final dataset
# write.xlsx(Getij_Spui_Kier_1hour, file = "Getij_Spui_Kier_UTC_1Hour_bins_CHEK_22_04_2024_TRANSITION_SPUI_KIER_3.xlsx")

# Write a csv file of this final 1 hour bin kier and spui file
# write.csv(Getij_Spui_Kier_1hour, file = "Spui_Kier_UTC_1hour_bins_correct_22_04.csv", row.names = FALSE)


