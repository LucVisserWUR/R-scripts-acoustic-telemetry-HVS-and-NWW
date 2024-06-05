title: "Environmental variable ~ DE correlation HVS and NWW telemetry networks: Main script behind the plots" 
Author: "Luc Visser"
date: "2024-20-04"

# Load correct packages
library(tidyr) 
library(dplyr)
library(magrittr) # also for piping functions
library(ggtext)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(openxlsx)
library(readxl)
library(GGally) 
library(patchwork) 
library(corrplot) 
library(readr) 
library(TukeyC)
library(gridExtra) 

# Chapters:
# 1. Import and format data and take relevant discharge periods
# 2. Select correct columns for water height, chloride and temperature
# 3. Select relevant time periods for all env. variables 
# 4. Retrieve receiver information from VUE
# 5. Create a complete env. variable dataset
# 6. Create relevant sluice management dataframes
# 7. Create 1 hourly bins for the env. variables
# 8. Select data from the env. variable dataset untill December for env. variable correlation
# 9. Merge detection efficiency with the total env. variable sheet
# 10. Clarify names of total env. variable data set + select receivers for env.var - detection efficiency data sets


# Clean worksheet and keep the detection efficiency calculations data sets
#rm(list = ls())
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW', 'Getij_Spui_Kier_data_UTC_total', 'Getij_Spui_Kier_1hour')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)
knitr::opts_chunk$set(echo = TRUE)


## 1. IMPORT AND FORMAT DATA AND TAKE RELEVANT DISCHARGE PERIODS

# Load in data from csv files
Discharge <- read.csv("Env_afvoer_OSPAR_0308.csv")
Discharge_before_2023 <- read.csv("Env_hvs-debiet_0308.csv")
Discharge_Lobith <- read.csv("Env_afvoer_Lobith_0308.csv")
Waterheight_NWW <- read.csv("Env_waterhoogte_hoekvholland_0308.csv") 
Waterheight_Hellevoetsluis <- read.csv("Env_waterhoogte_hellvss_0308.csv")
Waterheight_Stellendam_btn <- read.csv("Env_waterhoogte_stelldbtn_0308.csv")
Hefhoogte <- read.csv("Env_hefhoogte_HVS_0308.csv")
Chloride_Stelldbnn <- read.csv("Env_Chloride_stelldbnn.csv")
Chloride_Stelldbtn <- read.csv("Env_Chloride_stelldbtn.csv")
Temperature_Stelldbnn <- read.csv("Env_Temperature_stelldbnn_0308.csv")
Temperature_Stelldbtn <- read.csv("Env_Temperature_stelldbtn_0308.csv")
Wind_HVS <- read.csv("Env_Wind_HVS_0308.csv")
Wind_NWW <- read.csv("Env_Wind_hoekvholland_0308.csv")
Transmitter_information <- read.csv("Transmitterinformation2023_2024.csv") # In case you need some important transmitter information, for example the power output

# First set Date and Time in correct format

# Set correct format for DateTime data (CET = ECT = GMT - 1)
Discharge$DateTime_CET <- as.POSIXct(Discharge$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Discharge_Lobith$DateTime_CET <- as.POSIXct(Discharge_Lobith$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Discharge_before_2023$DateTime_CET <- as.POSIXct(Discharge_before_2023$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Waterheight_NWW$DateTime_CET <- as.POSIXct(Waterheight_NWW$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Waterheight_Hellevoetsluis$DateTime_CET <- as.POSIXct(Waterheight_Hellevoetsluis$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Waterheight_Stellendam_btn$DateTime_CET <- as.POSIXct(Waterheight_Stellendam_btn$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Hefhoogte$DateTime_CET <- as.POSIXct(Hefhoogte$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Chloride_Stelldbnn$DateTime_CET <- as.POSIXct(Chloride_Stelldbnn$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Chloride_Stelldbtn$DateTime_CET <- as.POSIXct(Chloride_Stelldbtn$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Temperature_Stelldbnn$DateTime_CET <- as.POSIXct(Temperature_Stelldbnn$DateTime_CET , format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Temperature_Stelldbtn$DateTime_CET <- as.POSIXct(Temperature_Stelldbtn$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Wind_HVS$DateTime_CET <- as.POSIXct(Wind_HVS$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")
Wind_NWW$DateTime_CET <- as.POSIXct(Wind_NWW$DateTime_CET, format = '%d/%m/%Y %H:%M', tz="Etc/GMT-1")

# Set DateTime to UTC format
Discharge$DateTime_UTC <- with_tz(Discharge$DateTime_CET, "UTC") 
Discharge_Lobith$DateTime_UTC <- with_tz(Discharge_Lobith$DateTime_CET, "UTC") 
Discharge_before_2023$DateTime_UTC <- with_tz(Discharge_before_2023$DateTime_CET, "UTC") 
Waterheight_NWW$DateTime_UTC <- with_tz(Waterheight_NWW$DateTime_CET, "UTC")
Waterheight_Hellevoetsluis$DateTime_UTC <- with_tz(Waterheight_Hellevoetsluis$DateTime_CET, "UTC")
Waterheight_Stellendam_btn$DateTime_UTC <- with_tz(Waterheight_Stellendam_btn$DateTime_CET, "UTC")
Hefhoogte$DateTime_UTC <- with_tz(Hefhoogte$DateTime_CET, "UTC")
Chloride_Stelldbnn$DateTime_UTC <- with_tz(Chloride_Stelldbnn$DateTime_CET, "UTC")
Chloride_Stelldbtn$DateTime_UTC <- with_tz(Chloride_Stelldbtn$DateTime_CET, "UTC")
Temperature_Stelldbnn$DateTime_UTC <- with_tz(Temperature_Stelldbnn$DateTime_CET, "UTC")
Temperature_Stelldbtn$DateTime_UTC <- with_tz(Temperature_Stelldbtn$DateTime_CET, "UTC")
Wind_HVS$DateTime_UTC <- with_tz(Wind_HVS$DateTime_CET, "UTC")
Wind_NWW$DateTime_UTC <- with_tz(Wind_NWW$DateTime_CET, "UTC")

# Merge discharge data sets together to create a data set for the complete time period with all available data (2016 - 2023)

# Haringvlietsluizen. Discharge data of period 2016-2022 is not available for NWW
colnames(Discharge_before_2023)[colnames(Discharge_before_2023) == "Discharge_HVS_total_m3_s"] <- "Discharge_HVS_m3_s" 
Discharge_HVS <- dplyr::bind_rows(Discharge, Discharge_before_2023) %>%  # Merge into one dataset so Discharge_HVS contains all dishcarges from 2016 - 2023
  dplyr::select(DateTime_UTC, Discharge_HVS_m3_s)
Discharge_HVS <- Discharge_HVS %>%
  dplyr::arrange(DateTime_UTC)

# Select only relevant DateTime data for discharge data
relevant_time_min_utc <- as.POSIXct("2023-01-26 11:59:00", tz = "UTC") # These start and end times are based on twelve / three hourly observed_expected detections
relevant_time_max_utc <- as.POSIXct("2023-12-31 09:00:00", tz = "UTC")
Discharge_relevant <- Discharge %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Discharge_relevant <- Discharge_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

Discharge_relevant <- Discharge_relevant %>% # Select only the relevant columns of this newly created dataset
  dplyr::select("DateTime_UTC", "Discharge_HVS_m3_s", "Discharge_Maasmond_m3_s", "Discharge_NWW_m3_s")

# --> Chapter 1  of 'Env_Var_HVS_NWW_PLOTS_FINAL.R' script for visualization


## 2. SELECT CORRECT COLUMNS FOR WATER HEIGHT, CHLORIDE AND TEMPERATURE

# Water height, chloride and temperature were provided in G (Validated) and O (Non-validated) columns. Select only the G columns.    

# HVS river
Waterheight_Hellevoetsluis <- Waterheight_Hellevoetsluis[c("DateTime_UTC", "Waterhoogte_Hellevoetsluis_O_cm")]
colnames(Waterheight_Hellevoetsluis)[colnames(Waterheight_Hellevoetsluis) == "Waterhoogte_Hellevoetsluis_O_cm"] <- "Waterheight_Hellevoetsluis_cm"

# HVS sea
Waterheight_Stellendam_btn <- Waterheight_Stellendam_btn[c("DateTime_UTC", "Waterhoogte_StellendamBuiten_O_cm")]
colnames(Waterheight_Stellendam_btn)[colnames(Waterheight_Stellendam_btn) == "Waterhoogte_StellendamBuiten_O_cm"] <- "Waterheight_Stellendam_btn_cm"

# Create total HVS water height data set & calculate delta water height (Water height river - sea)
Water_height_HVS <-  Waterheight_Hellevoetsluis %>%
  full_join(Waterheight_Stellendam_btn, by = "DateTime_UTC")
Water_height_HVS$Delta_Waterheight <- Water_height_HVS$Waterheight_Hellevoetsluis_cm - Water_height_HVS$Waterheight_Stellendam_btn_cm

# NWW
Waterheight_NWW <- Waterheight_NWW[c("DateTime_UTC", "Waterhoogte_HoekvanHolland_O_cm")]
colnames(Waterheight_NWW)[colnames(Waterheight_NWW) == "Waterhoogte_HoekvanHolland_O_cm"] <- "Waterheight_NWW_cm"

# CHLORIDE. Select only 200cm depth. 200cm was the most relevant depth with respect to receiver positioning.

# HVS river
Chloride_Stelldbnn <- Chloride_Stelldbnn[c("DateTime_UTC", "Chloride_StellendamBinnen_200cm_mg_L_G")] 
colnames(Chloride_Stelldbnn)[colnames(Chloride_Stelldbnn) == "Chloride_StellendamBinnen_200cm_mg_L_G"] <- "Chloride_stelldbnn_200cm"

# HVS sea
Chloride_Stelldbtn <- Chloride_Stelldbtn[c("DateTime_UTC", "Chloride_StellendamBuiten_200cm_mg_L_G")]
colnames(Chloride_Stelldbtn)[colnames(Chloride_Stelldbtn) == "Chloride_StellendamBuiten_200cm_mg_L_G"] <- "Chloride_stelldbtn_200cm"

# Create one HVS chloride file
Chloride_HVS <-  Chloride_Stelldbnn %>%
  full_join(Chloride_Stelldbtn, by = "DateTime_UTC")

# TEMPERATURE. Again, only select 200 cm depth. 

# HVS river
Temperature_Stelldbnn <- Temperature_Stelldbnn[c("DateTime_UTC", "Temp_stelldbnn_200cm_G")] 
colnames(Temperature_Stelldbnn)[colnames(Temperature_Stelldbnn) == "Temp_stelldbnn_200cm_G"] <- "Temp_stelldbnn_200cm"

# HVS sea
Temperature_Stelldbtn <- Temperature_Stelldbtn[c("DateTime_UTC", "Temp_stelldbtn_200cm_G")]
colnames(Temperature_Stelldbtn)[colnames(Temperature_Stelldbtn) == "Temp_stelldbtn_200cm_G"] <- "Temp_stelldbtn_200cm"

# Create one HVS temperature file
Temperature_HVS <-  Temperature_Stelldbnn %>%
  full_join(Temperature_Stelldbtn, by = "DateTime_UTC")

## 3. SELECT RELEVANT TIME PERIODS FOR ALL ENV. VARIABLES

# WATER HEIGHT

# Select relevant time period that can be used for all variables
relevant_time_min_utc <- as.POSIXct("2023-01-26 11:59:00", tz = "UTC")
relevant_time_max_utc <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")

# HVS river & HVS sea
Water_height_HVS_relevant <- Water_height_HVS %>%
  filter(DateTime_UTC >= relevant_time_min_utc & DateTime_UTC <= relevant_time_max_utc)

# NWW
Water_height_NWW_relevant <- Waterheight_NWW %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Water_height_NWW_relevant <- Water_height_NWW_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# SLUICE OPENING ( = hef hoogte)
Hefhoogte_relevant <- Hefhoogte %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Hefhoogte_relevant <- Hefhoogte_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# Additional calculations for sluice opening: total height in cm (sum of cm of all sluices)

# Calculate the total sluice opening (cm) by adding up sluice opening HVS sluise 1-17
Hefhoogte_relevant$Total_sluice_opening_HVS_cm <- rowSums(Hefhoogte_relevant[, 2:18])

# Select only relevant columns for sluice opening
Hefhoogte_relevant <- Hefhoogte_relevant %>%
  dplyr::select("DateTime_UTC", "Total_sluice_opening_HVS_cm", "Total_sluice_opening_HVS_m2")

# CHLORIDE
Chloride_HVS_relevant <- Chloride_HVS %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Chloride_HVS_relevant <- Chloride_HVS_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# TEMPERATURE
Temperature_HVS_relevant <- Temperature_HVS %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Temperature_HVS_relevant <- Temperature_HVS_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# WIND

# HVS
Wind_HVS_relevant <- Wind_HVS %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Wind_HVS_relevant <- Wind_HVS_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# NWW
Wind_NWW_relevant <- Wind_NWW %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Wind_NWW_relevant <- Wind_NWW_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# Full join the two wind files by DateTime_UTC
Wind_HVS_NWW_relevant <-  Wind_HVS_relevant %>%
  full_join(Wind_NWW_relevant, by = "DateTime_UTC")

# Select only relevant columns in this total relevant wind file
Wind_HVS_NWW_relevant <- Wind_HVS_NWW_relevant %>% 
  dplyr::select("DateTime_UTC", "WindSnelheid_HVS_m_s", "WindRichting_HVS_graad", "WindSnelheid_HoekvanHolland_m_s", "WindRichting_HoekvanHolland_graad")


## 4. RETRIEVE RECEIVER INFORMATION FROM VUE (Vemco User Environment)

# Load the receiver data as csv files obtained from VUE

# For sync transmitters
Tilt_Noise_Temp_HDC <- read.csv("Env_Tilt_Noise_Temp_HDC_VUE") # Haringvliet HD-C
#Tilt_Noise_Temp_HS6 <- read.csv("Env_Tilt_Noise_Temp_HS6_VUE") # Slijkgat HS6
#Tilt_Noise_Temp_NW2 <- read.csv("Env_Tilt_Noise_Temp_NW2_VUE") # NieuweWaterweg NW2
#Tilt_noise_Temp_HDG <- read.csv()

# For receivers
Tilt_Noise_Temp_HS8 <- read.csv("Env_Tilt_Noise_Temp_HS8_VUE") # Slijkgat HS8
Tilt_Noise_Temp_NW3 <- read.csv("Env_Tilt_Noise_Temp_NW3_VUE") # NieuweWaterweg NW3

# Rename DateTime column names to make the imported csv files able sot merge with other files
colnames(Tilt_Noise_Temp_HDC)[colnames(Tilt_Noise_Temp_HDC) == "Date.and.Time..UTC."] <- "DateTime_UTC" # HDC
colnames(Tilt_Noise_Temp_HS8)[colnames(Tilt_Noise_Temp_HS8) == "Date.and.Time..UTC."] <- "DateTime_UTC" # HS8
colnames(Tilt_Noise_Temp_NW3)[colnames(Tilt_Noise_Temp_NW3) == "Date.and.Time..UTC."] <- "DateTime_UTC" # NW3

# Now clarify that the DateTime_UTC column is indeed in a UTC format
# *1 In VUE they refer to 'PC time', which is in UTC + 1. The time column data gets automatically imported as 'Date.and.Time..UTC.', of which the starting time is one hour earlier of this PC time
# For this reason, I would say it is safe to assume that this VUE time is already in a UTC format
# *2 Moreover, it seems that there are no winter - summer time transitions) 
Tilt_Noise_Temp_HDC$DateTime_UTC <- as.POSIXct(Tilt_Noise_Temp_HDC$DateTime_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # time zone = UTC
Tilt_Noise_Temp_HS8$DateTime_UTC <- as.POSIXct(Tilt_Noise_Temp_HS8$DateTime_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # time zone = UTC
Tilt_Noise_Temp_NW3$DateTime_UTC <- as.POSIXct(Tilt_Noise_Temp_NW3$DateTime_UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") # time zone = UTC

# Select only the relevant hours for the tilt, noise and temperature date
relevant_time_min_utc <- as.POSIXct("2023-01-26 22:59:00", tz = "UTC") 
relevant_time_max_utc <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")

Tilt_Noise_Temp_HDC_relevant <- Tilt_Noise_Temp_HDC %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Tilt_Noise_Temp_HDC_relevant <- Tilt_Noise_Temp_HDC_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

Tilt_Noise_Temp_HS8_relevant <- Tilt_Noise_Temp_HS8 %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Tilt_Noise_Temp_HS8_relevant <- Tilt_Noise_Temp_HS8_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

Tilt_Noise_Temp_NW3_relevant <- Tilt_Noise_Temp_NW3 %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Tilt_Noise_Temp_NW3_relevant <- Tilt_Noise_Temp_NW3_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# Alter the format of the hours a bit: the format where it is in now contains some 'loose' minutes like '2023-01-27 02:00:01'. Keeping this in will complicate merging later on. 
Tilt_Noise_Temp_HDC_relevant$DateTime_UTC <- as.POSIXct(strptime(Tilt_Noise_Temp_HDC_relevant$DateTime_UTC, format = "%Y-%m-%d %H", tz = "UTC"))
Tilt_Noise_Temp_HS8_relevant$DateTime_UTC <- as.POSIXct(strptime(Tilt_Noise_Temp_HS8_relevant$DateTime_UTC, format = "%Y-%m-%d %H", tz = "UTC"))
Tilt_Noise_Temp_NW3_relevant$DateTime_UTC <- as.POSIXct(strptime(Tilt_Noise_Temp_NW3_relevant$DateTime_UTC, format = "%Y-%m-%d %H", tz = "UTC"))

# Remove rows with hourly pings and detections: the focus is on tilt, noise and temperature now. 
Tilt_Noise_Temp_HDC_relevant <- subset(Tilt_Noise_Temp_HDC_relevant, Description %in% c('Battery', 'Tilt angle', 'Temperature', 'Average noise'))
Tilt_Noise_Temp_HDC_relevant <- Tilt_Noise_Temp_HDC_relevant[, !(names(Tilt_Noise_Temp_HDC_relevant) %in% c('Units'))] # Also remove the Units column: add those in the description column names

Tilt_Noise_Temp_HS8_relevant <- subset(Tilt_Noise_Temp_HS8_relevant, Description %in% c('Battery', 'Tilt angle', 'Temperature', 'Average noise'))
Tilt_Noise_Temp_HS8_relevant <- Tilt_Noise_Temp_HS8_relevant[, !(names(Tilt_Noise_Temp_HS8_relevant) %in% c('Units'))] # Also remove the Units column: add those in the description column names

Tilt_Noise_Temp_NW3_relevant <- subset(Tilt_Noise_Temp_NW3_relevant, Description %in% c('Battery', 'Tilt angle', 'Temperature', 'Average noise'))
Tilt_Noise_Temp_NW3_relevant <- Tilt_Noise_Temp_NW3_relevant[, !(names(Tilt_Noise_Temp_NW3_relevant) %in% c('Units'))] 

# Reshape the data frame to get tilt, environmental noise and temperature as column names
Tilt_Noise_Temp_HDC_relevant <- Tilt_Noise_Temp_HDC_relevant %>% 
  pivot_wider(names_from = Description, values_from = Data)

Tilt_Noise_Temp_HS8_relevant <- Tilt_Noise_Temp_HS8_relevant %>% 
  pivot_wider(names_from = Description, values_from = Data)

Tilt_Noise_Temp_NW3_relevant <- Tilt_Noise_Temp_NW3_relevant %>% 
  pivot_wider(names_from = Description, values_from = Data)

# Reshaping is finished. Now rename the columns, including the receiver name. 

# Battery
colnames(Tilt_Noise_Temp_HDC_relevant)[colnames(Tilt_Noise_Temp_HDC_relevant) == "Battery"] <- "Battery_HDC" # HDC
colnames(Tilt_Noise_Temp_HS8_relevant)[colnames(Tilt_Noise_Temp_HS8_relevant) == "Battery"] <- "Battery_HS8" # HS8
colnames(Tilt_Noise_Temp_NW3_relevant)[colnames(Tilt_Noise_Temp_NW3_relevant) == "Battery"] <- "Battery_NW3" # NW3

# Tilt
colnames(Tilt_Noise_Temp_HDC_relevant)[colnames(Tilt_Noise_Temp_HDC_relevant) == "Tilt angle"] <- "Tilt_HDC" # HDC
colnames(Tilt_Noise_Temp_HS8_relevant)[colnames(Tilt_Noise_Temp_HS8_relevant) == "Tilt angle"] <- "Tilt_HS8" # HS8
colnames(Tilt_Noise_Temp_NW3_relevant)[colnames(Tilt_Noise_Temp_NW3_relevant) == "Tilt angle"] <- "Tilt_NW3" # NW3

# Temperature
colnames(Tilt_Noise_Temp_HDC_relevant)[colnames(Tilt_Noise_Temp_HDC_relevant) == "Temperature"] <- "Temp_HDC" # HDC
colnames(Tilt_Noise_Temp_HS8_relevant)[colnames(Tilt_Noise_Temp_HS8_relevant) == "Temperature"] <- "Temp_HS8" # HS8
colnames(Tilt_Noise_Temp_NW3_relevant)[colnames(Tilt_Noise_Temp_NW3_relevant) == "Temperature"] <- "Temp_NW3" # NW3

# Noise
colnames(Tilt_Noise_Temp_HDC_relevant)[colnames(Tilt_Noise_Temp_HDC_relevant) == "Average noise"] <- "Noise_HDC" # HDC
colnames(Tilt_Noise_Temp_HS8_relevant)[colnames(Tilt_Noise_Temp_HS8_relevant) == "Average noise"] <- "Noise_HS8" # HS8
colnames(Tilt_Noise_Temp_NW3_relevant)[colnames(Tilt_Noise_Temp_NW3_relevant) == "Average noise"] <- "Noise_NW3" # NW3

# Remove receiver name column, implement in other column name
Tilt_Noise_Temp_HDC_relevant <- Tilt_Noise_Temp_HDC_relevant[, !(names(Tilt_Noise_Temp_HDC_relevant) %in% c('Receiver'))] 
Tilt_Noise_Temp_HS8_relevant <- Tilt_Noise_Temp_HS8_relevant[, !(names(Tilt_Noise_Temp_HS8_relevant) %in% c('Receiver'))] 
Tilt_Noise_Temp_NW3_relevant <- Tilt_Noise_Temp_NW3_relevant[, !(names(Tilt_Noise_Temp_NW3_relevant) %in% c('Receiver'))] 

# Merge the receiver information files together to create a complete dataframe
Receiver_info_total <- Tilt_Noise_Temp_HS8_relevant %>% # Merge HS8 with NW3
  full_join(Tilt_Noise_Temp_NW3_relevant, by = "DateTime_UTC")

Receiver_info_total <- Receiver_info_total %>% # Add HDC
  full_join(Tilt_Noise_Temp_HDC_relevant, by = "DateTime_UTC")


## 5. CREATE A COMPLETE ENV. VARIABLE DATASET

# First discharge with water height NWW
Enviro_combined <- Discharge_relevant %>%
  full_join(Water_height_NWW_relevant, by = "DateTime_UTC")

# Add water height HVS
Enviro_combined <- Enviro_combined %>%
  full_join(Water_height_HVS_relevant, by = "DateTime_UTC")

# Add sluice opening
Enviro_combined <- Enviro_combined %>%
  full_join(Hefhoogte_relevant, by = "DateTime_UTC")

# Add chloride
Enviro_combined <- Enviro_combined %>%
  full_join(Chloride_HVS_relevant, by = "DateTime_UTC")

# Add temperature
Enviro_combined <- Enviro_combined %>%
  full_join(Temperature_HVS_relevant, by = "DateTime_UTC")

# Add wind
Enviro_combined <- Enviro_combined %>%
  full_join(Wind_HVS_NWW_relevant, by = "DateTime_UTC")


## 6. CREATE RELEVANT SLUICE MANAGEMENT DATAFRAMES

HVS_management_Apr <- subset(Enviro_combined, DateTime_UTC > "2023-04-10 09:00:00" & DateTime_UTC < "2023-04-24 09:00:00" )
HVS_management_Jul <- subset(Enviro_combined, DateTime_UTC > "2023-07-10 09:00:00" & DateTime_UTC < "2023-07-24 09:00:00" )
HVS_management_Dec <- subset(Enviro_combined, DateTime_UTC > "2023-12-10 09:00:00" & DateTime_UTC < "2023-12-24 09:00:00" ) 

# Make a relevant dataset for LOBITH discharge
relevant_time_min_utc <- as.POSIXct("2023-01-26 11:59:00", tz = "UTC") 
relevant_time_max_utc <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")
Discharge_Lobith_relevant <- Discharge_Lobith %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Discharge_Lobith_relevant <- Discharge_Lobith_relevant %>%
  filter(DateTime_UTC < relevant_time_max_utc)

Discharge_Lobith_relevant <- Discharge_Lobith_relevant %>% # Select only the relevant columns of this newly created dataset
  dplyr::select("DateTime_UTC", "Discharge_Lobith_m3_s")

# Merge discharge Lobith with total sluice opening HVS m2 
Lobith_sluice_opening <- Discharge_Lobith_relevant %>%
  full_join(Hefhoogte_relevant,  by = "DateTime_UTC")

# Take 2 week period to show an example of how sluice management depends on discharge Lobith
Lobith_sluice_opening_Mar <- subset(Lobith_sluice_opening, DateTime_UTC > "2023-03-10 09:00:00" & DateTime_UTC < "2023-03-24 09:00:00" )

# --> Chapter 2  of 'Env_Variables_HVS_NWW_PLOTS_FINAL.R' script for visualization  

## 7. CREATE 1 HOURLY BINS FOR THE ENV. VARIABLES. 

# Set time limits for the hourly bins.
relevant_time_min_utc_1hour <- as.POSIXct("2023-01-26 22:59:00", tz = "UTC") 
relevant_time_max_utc_1hour <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")  
Enviro_combined_hours <- Enviro_combined %>%
  filter(DateTime_UTC > relevant_time_min_utc_1hour)
Enviro_combined_hours <- Enviro_combined_hours %>%
  filter(DateTime_UTC < relevant_time_max_utc_1hour)

# Create the hourly keys for the 1 hour bins.
Enviro_combined_hours <- mutate(Enviro_combined_hours, Hour = format(DateTime_UTC, "%Y-%m-%d %H:00:00")) 

# Calculate the average of each env. variable for each hour.
Enviro_combined_hours_average <- Enviro_combined_hours %>% 
  dplyr::group_by(Hour) %>%
  dplyr::summarise(Avg_Discharge_HVS_m3_s = mean(Discharge_HVS_m3_s, na.rm = TRUE),
            Avg_Discharge_NWW_m3_s = mean(Discharge_NWW_m3_s, na.rm = TRUE),
            Avg_Waterheight_NWW_cm = mean(Waterheight_NWW_cm, na.rm = TRUE),
            Avg_Waterheight_Stellendam_btn_cm = mean(Waterheight_Stellendam_btn_cm, na.rm = TRUE),
            Avg_Waterheight_Hellevoetsluis_cm = mean(Waterheight_Hellevoetsluis_cm, na.rm = TRUE),
            Avg_Delta_Waterheight_HVS_cm = mean(Delta_Waterheight, na.rm = TRUE),
            Avg_sluice_opening_HVS_cm = mean(Total_sluice_opening_HVS_cm, na.rm = TRUE),
            Avg_sluice_opening_HVS_m2 = mean(Total_sluice_opening_HVS_m2, na.rm = TRUE),
            Avg_Chloride_stelldbnn_200cm = mean(Chloride_stelldbnn_200cm, na.rm = TRUE), 
            Avg_Chloride_stelldbtn_200cm = mean(Chloride_stelldbtn_200cm, na.rm = TRUE),
            Avg_Temp_stelldbnn_200cm = mean(Temp_stelldbnn_200cm, na.rm = TRUE),
            Avg_Temp_stelldbtn_200cm = mean(Temp_stelldbtn_200cm, na.rm = TRUE),
            Avg_WindSnelheid_HVS_m_s = mean(WindSnelheid_HVS_m_s, na.rm = TRUE),
            Avg_WindRichting_HVS_graad = mean(WindRichting_HVS_graad, na.rm = TRUE),
            Avg_WindSnelheid_HoekvanHolland_m_s = mean(WindSnelheid_HoekvanHolland_m_s, na.rm = TRUE),
            Avg_WindRichting_HoekvanHolland_graad = mean(WindRichting_HoekvanHolland_graad, na.rm = TRUE))

# Round average values to 1 decimal.
Enviro_combined_hours_average$Avg_Discharge_HVS_m3_s <- round(Enviro_combined_hours_average$Avg_Discharge_HVS_m3_s, digits = 1)
Enviro_combined_hours_average$Avg_Discharge_NWW_m3_s <- round(Enviro_combined_hours_average$Avg_Discharge_NWW_m3_s, digits = 1)
Enviro_combined_hours_average$Avg_Waterheight_NWW_cm <- round(Enviro_combined_hours_average$Avg_Waterheight_NWW_cm, digits = 1) 
Enviro_combined_hours_average$Avg_Waterheight_Stellendam_btn_cm <- round(Enviro_combined_hours_average$Avg_Waterheight_Stellendam_btn_cm, digits = 1) 
Enviro_combined_hours_average$Avg_Waterheight_Hellevoetsluis_cm <- round(Enviro_combined_hours_average$Avg_Waterheight_Hellevoetsluis_cm, digits = 1) 
Enviro_combined_hours_average$Avg_Delta_Waterheight_HVS_cm <- round(Enviro_combined_hours_average$Avg_Delta_Waterheight_HVS_cm, digits = 1) 
Enviro_combined_hours_average$Avg_sluice_opening_HVS_cm <- round(Enviro_combined_hours_average$Avg_sluice_opening_HVS_cm, digits = 1) 
Enviro_combined_hours_average$Avg_sluice_opening_HVS_m2 <- round(Enviro_combined_hours_average$Avg_sluice_opening_HVS_m2, digits = 1) 
Enviro_combined_hours_average$Avg_Chloride_stelldbnn_200cm <- round(Enviro_combined_hours_average$Avg_Chloride_stelldbnn_200cm, digits = 1)
Enviro_combined_hours_average$Avg_Chloride_stelldbtn_200cm <- round(Enviro_combined_hours_average$Avg_Chloride_stelldbtn_200cm, digits = 1)
Enviro_combined_hours_average$Avg_Temp_stelldbnn_200cm <- round(Enviro_combined_hours_average$Avg_Temp_stelldbnn_200cm, digits = 1)
Enviro_combined_hours_average$Avg_Temp_stelldbtn_200cm <- round(Enviro_combined_hours_average$Avg_Temp_stelldbtn_200cm, digits = 1)
Enviro_combined_hours_average$Avg_WindSnelheid_HVS_m_s <- round(Enviro_combined_hours_average$Avg_WindSnelheid_HVS_m_s, digits = 1)
Enviro_combined_hours_average$Avg_WindRichting_HVS_graad <- round(Enviro_combined_hours_average$Avg_WindRichting_HVS_graad, digits = 1)
Enviro_combined_hours_average$Avg_WindSnelheid_HoekvanHolland_m_s <- round(Enviro_combined_hours_average$Avg_WindSnelheid_HoekvanHolland_m_s, digits = 1)
Enviro_combined_hours_average$Avg_WindRichting_HoekvanHolland_graad <- round(Enviro_combined_hours_average$Avg_WindRichting_HoekvanHolland_graad, digits = 1)

# Convert the wind direction values in Enviro_combined_hours_average to the four points of compass.
convert_to_compass <- function(degrees) {
  ifelse((degrees >= 315 | degrees <= 44.9), "North", # Use 44.9 instead of 45, otherwise some values can be both North and East
         ifelse((degrees >= 45 & degrees <= 134.9), "East",
                ifelse((degrees >= 135 & degrees <= 224.9), "South",
                       ifelse((degrees >= 225 & degrees <= 314.9), "West", NA))))
}

Enviro_combined_hours_average <- Enviro_combined_hours_average %>%
  mutate(Avg_Wind_Direction_HVS_compass = convert_to_compass(Avg_WindRichting_HVS_graad))
Enviro_combined_hours_average <- Enviro_combined_hours_average %>%
  mutate(Avg_Wind_Direction_HoekvanHolland_compass = convert_to_compass(Avg_WindRichting_HoekvanHolland_graad))


# Bind the Enviro_combined_hours_average with the receiver info. 

  # This couldn't been done earlier because the tilt data is per definition given per hour

# Steps below are to ensure that merging occurs adequately 
colnames(Receiver_info_total)[colnames(Receiver_info_total) == "DateTime_UTC"] <- "Hour" 
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Receiver_info_total$Hour <- as.POSIXct(Receiver_info_total$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Merge combined variable set with tilt, noise, temp and battery to create the total env. variable dataset
Enviro_combined_hours_average <- Enviro_combined_hours_average %>%
  full_join(Receiver_info_total, by = "Hour")

# Calculate delta tilt for HDC, HS8 and NW3 in this dataset
Enviro_combined_hours_average$Tilt_HDC <- as.numeric(as.character(Enviro_combined_hours_average$Tilt_HDC)) # partly provided as character strings by RWS. NAs are introduced. Deal with that later
Enviro_combined_hours_average$Tilt_HS8 <- as.numeric(as.character(Enviro_combined_hours_average$Tilt_HS8))
Enviro_combined_hours_average$Tilt_NW3 <- as.numeric(as.character(Enviro_combined_hours_average$Tilt_NW3))

# Calculate the absolute tilt (Provided tilt (degrees) - 180)
Enviro_combined_hours_average <- Enviro_combined_hours_average %>% # HD-C
  mutate(Tilt_HDC_delta = 180 - Tilt_HDC)

Enviro_combined_hours_average <- Enviro_combined_hours_average %>% # HS8
  mutate(Tilt_HS8_delta = 180 - Tilt_HS8)

Enviro_combined_hours_average <- Enviro_combined_hours_average %>% # NWW
  mutate(Tilt_NW3_delta = 180 - Tilt_NW3)

# --> Chapter 3  of 'Env_Var_HVS_NWW_PLOTS_FINAL.R' script for visualization


## 8. SELECT DATA FROM THE ENV. VARIABLE DATASET UNTILL DECEMBER FOR ENV. VARIABLE CORRELATION. 
  # This is done because discharge data was only available untill December. 

# Take a subset of Enviro_combined_hours_1hour_average until December 31: no discharge HVS data available after this point 
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
December_limit_utc <- as.POSIXct("2023-12-31 23:00:00", tz = "UTC")  
Enviro_combined_hours_average_till_December2023 <- Enviro_combined_hours_average %>% # First make sure the enviro combined data has undergone the exact same time selection as the Enviro_combined_hours_average data
  filter(Hour < December_limit_utc)

# Put the values in a numeric format. 
Enviro_combined_hours_average_till_December2023$Avg_Discharge_HVS_m3_s <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_Discharge_HVS_m3_s)
Enviro_combined_hours_average_till_December2023$Avg_Discharge_NWW_m3_s <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_Discharge_NWW_m3_s)
Enviro_combined_hours_average_till_December2023$Avg_Delta_Waterheight_HVS_cm <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_Delta_Waterheight_HVS_cm)
Enviro_combined_hours_average_till_December2023$Avg_sluice_opening_HVS_m2 <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_sluice_opening_HVS_m2)
Enviro_combined_hours_average_till_December2023$Avg_Chloride_stelldbnn_200cm <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_Chloride_stelldbnn_200cm)
Enviro_combined_hours_average_till_December2023$Avg_Temp_stelldbnn_200cm <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_Temp_stelldbnn_200cm)
Enviro_combined_hours_average_till_December2023$Avg_WindSnelheid_HVS_m_s <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_WindSnelheid_HVS_m_s)
Enviro_combined_hours_average_till_December2023$Avg_WindSnelheid_HoekvanHolland_m_s <- as.numeric(Enviro_combined_hours_average_till_December2023$Avg_WindSnelheid_HoekvanHolland_m_s)

# --> Chapter 4 & 5  of 'Env_Var_HVS_NWW_PLOTS_FINAL.R' script for visualization


## 9. MERGE DETECTION EFFICIENCY WITH THE TOTAL ENV. VARIABLE SHEET

# HVS. Create 1-hour bins
HVS_DE_1hour_relevant <- one_hourly_observed_expected_detections_HVS %>% # Rename column name to ensure proper merging
  dplyr::rename(Hour = hour)
HVS_DE_1hour_relevant$Hour <- as.POSIXct(HVS_DE_1hour_relevant$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Merge complete environmental variable sheet with DE HVS sheet
Enviro_combined_DE_1hour_HVS <- HVS_DE_1hour_relevant  %>%
  full_join(Enviro_combined_hours_average, by = "Hour")

# Insert observation column with the row numbers: facilitates individual row removal
Enviro_combined_DE_1hour_HVS <- Enviro_combined_DE_1hour_HVS %>%
  dplyr::mutate(obs = row_number()) %>% # Insert observation values
  dplyr::select(obs, everything()) # Put this obs column as the first column

# Insert the Spui / Kier column for this HVS dataset
Getij_Spui_Kier_1hour$Hour <- as.POSIXct(Getij_Spui_Kier_1hour$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Enviro_combined_DE_1hour_HVS <- full_join(Enviro_combined_DE_1hour_HVS, Getij_Spui_Kier_1hour, by = "Hour")
Enviro_combined_DE_1hour_HVS <- dplyr::select(Enviro_combined_DE_1hour_HVS, -c(Spui, Kier))

# Add a value for closed in the spui_kier column when the sluice opening = 0 in the DE HVS dataframe
Enviro_combined_DE_1hour_HVS$Avg_sluice_opening_HVS_m2 <- as.numeric(as.character(Enviro_combined_DE_1hour_HVS$Avg_sluice_opening_HVS_m2)) 
Enviro_combined_DE_1hour_HVS$Spui_Kier[Enviro_combined_DE_1hour_HVS$Avg_sluice_opening_HVS_m2 == 0] <- "Closed" # Introduce closed values in spui/kier data
#ID_closed_sluice <- which(Enviro_combined_DE_1hour_HVS$Spui_Kier == "Closed") # Check if closed values are indeed inserted
#print(ID_closed_sluice)
Enviro_combined_DE_1hour_HVS <- dplyr::rename(Enviro_combined_DE_1hour_HVS, Spui_Kier_Closed = Spui_Kier) # Rename column name to SKC(closed)

# NWW. Create 1-hour bins
NWW_DE_1hour_relevant <- one_hourly_observed_expected_detections_NWW %>% # Rename column name to ensure proper merging
  dplyr::rename(Hour = hour)
NWW_DE_1hour_relevant$Hour <- as.POSIXct(NWW_DE_1hour_relevant$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Merge complete environmental variable sheet with DE NWW sheet
Enviro_combined_DE_1hour_NWW <- NWW_DE_1hour_relevant  %>%
  full_join(Enviro_combined_hours_average, by = "Hour")

# Insert observation column with the row numbers: facilitates individual row removal
Enviro_combined_DE_1hour_NWW <- Enviro_combined_DE_1hour_NWW %>%
  dplyr::mutate(obs = row_number()) %>% # Insert observation values
  dplyr::select(obs, everything()) # Put this obs column as the first column

# 10. CLARIFY NAMES OF TOTAL ENV. VARIABLE DATA SET + SELECT RECEIVERS FOR ENV.VAR - DETECTION EFFICIENCY DATA SETS

# Useful datasets 
Env_variables_total <- Enviro_combined_hours_average # Total env. variable dataframe
DE_HVS_total <- HVS_DE_1hour_relevant # Total DE dataset for HVS
DE_NWW_total <- NWW_DE_1hour_relevant # Total DE dataset for NWW
Env_DE_HVS_total <- Enviro_combined_DE_1hour_HVS # Total Env.var - DE sheet for HVS
Env_DE_NWW_total <- Enviro_combined_DE_1hour_NWW # Total Env.var - DE sheet for NWW

# Select relevant columns per dataset 
Env_variables_total <- Env_variables_total[, c("Hour", "Avg_Discharge_HVS_m3_s","Avg_Discharge_NWW_m3_s" , "Avg_Waterheight_Hellevoetsluis_cm", "Avg_Waterheight_Stellendam_btn_cm", "Avg_Waterheight_NWW_cm", "Avg_Delta_Waterheight_HVS_cm", "Avg_WindSnelheid_HVS_m_s", "Avg_WindSnelheid_HoekvanHolland_m_s" , "Avg_Wind_Direction_HVS_compass", "Avg_Wind_Direction_HoekvanHolland_compass" ,"Avg_sluice_opening_HVS_cm", "Avg_sluice_opening_HVS_m2", "Avg_Temp_stelldbnn_200cm", "Avg_Temp_stelldbtn_200cm" ,"Avg_Chloride_stelldbnn_200cm", "Avg_Chloride_stelldbtn_200cm" , "Battery_HDC", "Battery_HS8", "Battery_NW3", "Temp_HDC", "Temp_HS8", "Temp_NW3"  , "Noise_HDC", "Noise_HS8", "Noise_NW3" ,"Tilt_HDC_delta" , "Tilt_HS8_delta" , "Tilt_NW3_delta")]
Env_DE_HVS_total <- Env_DE_HVS_total[, c("Hour", "Receiver_location", "Transmitter_location", "expected_n", "n" ,"det_eff", "Avg_Discharge_HVS_m3_s", "Avg_Waterheight_Hellevoetsluis_cm", "Avg_Waterheight_Stellendam_btn_cm", "Avg_WindSnelheid_HVS_m_s", "Avg_Wind_Direction_HVS_compass"  , "Avg_sluice_opening_HVS_m2", "Spui_Kier_Closed", "Avg_Temp_stelldbnn_200cm", "Avg_Temp_stelldbtn_200cm" ,"Avg_Chloride_stelldbnn_200cm", "Avg_Chloride_stelldbtn_200cm" , "Battery_HDC", "Battery_HS8", "Temp_HDC", "Temp_HS8", "Noise_HDC", "Noise_HS8", "Tilt_HDC_delta" , "Tilt_HS8_delta")]
Env_DE_NWW_total <- Env_DE_NWW_total[, c("Hour","Receiver_location", "Transmitter_location", "expected_n", "n" , "det_eff", "Avg_Discharge_NWW_m3_s", "Avg_Waterheight_NWW_cm", "Avg_WindSnelheid_HoekvanHolland_m_s", "Avg_Wind_Direction_HoekvanHolland_compass",  "Battery_NW3", "Temp_NW3", "Noise_NW3", "Tilt_NW3_delta")]

# Rename column names

# Total env. variable dataframe. RENAME DELTA WATERHEIGHT AS WELL 
Env_variables_total <- dplyr::rename(Env_variables_total, DateTime_UTC = Hour)
Env_variables_total <- dplyr::rename(Env_variables_total, Discharge_HVS = Avg_Discharge_HVS_m3_s)
Env_variables_total <- dplyr::rename(Env_variables_total, Discharge_NWW = Avg_Discharge_NWW_m3_s)
Env_variables_total <- dplyr::rename(Env_variables_total, Water_Height_HVSr = Avg_Waterheight_Hellevoetsluis_cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Water_Height_HVSs = Avg_Waterheight_Stellendam_btn_cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Water_Height_NWW = Avg_Waterheight_NWW_cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Wind_Speed_HVS = Avg_WindSnelheid_HVS_m_s)
Env_variables_total <- dplyr::rename(Env_variables_total, Wind_Speed_NWW = Avg_WindSnelheid_HoekvanHolland_m_s)
Env_variables_total <- dplyr::rename(Env_variables_total, Wind_Direction_HVS = Avg_Wind_Direction_HVS_compass)
Env_variables_total <- dplyr::rename(Env_variables_total, Wind_Direction_NWW = Avg_Wind_Direction_HoekvanHolland_compass)
Env_variables_total <- dplyr::rename(Env_variables_total, Sluice_Opening_cm = Avg_sluice_opening_HVS_cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Sluice_Opening_m2 = Avg_sluice_opening_HVS_m2)
Env_variables_total <- dplyr::rename(Env_variables_total, Temp_HVSr = Avg_Temp_stelldbnn_200cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Temp_HVSs = Avg_Temp_stelldbtn_200cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Chloride_HVSr = Avg_Chloride_stelldbnn_200cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Chloride_HVSs = Avg_Chloride_stelldbtn_200cm)
Env_variables_total <- dplyr::rename(Env_variables_total, Tilt_HDC = Tilt_HDC_delta)
Env_variables_total <- dplyr::rename(Env_variables_total, Tilt_HS8 = Tilt_HS8_delta)
Env_variables_total <- dplyr::rename(Env_variables_total, Tilt_NW3 = Tilt_NW3_delta)

# Total detection efficiency files for HVS and NWW 
DE_HVS_total <- dplyr::rename(DE_HVS_total, DateTime_UTC = Hour)
DE_HVS_total <- dplyr::rename(DE_HVS_total, DE = det_eff)
DE_NWW_total <- dplyr::rename(DE_NWW_total, DateTime_UTC = Hour)
DE_NWW_total <- dplyr::rename(DE_NWW_total, DE = det_eff)

# HVS 
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, DateTime_UTC = Hour)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, DE = det_eff)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Discharge_HVS = Avg_Discharge_HVS_m3_s)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Water_Height_HVSr = Avg_Waterheight_Hellevoetsluis_cm)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Water_Height_HVSs = Avg_Waterheight_Stellendam_btn_cm)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Wind_Speed_HVS = Avg_WindSnelheid_HVS_m_s)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Wind_Direction_HVS = Avg_Wind_Direction_HVS_compass)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Sluice_Opening = Avg_sluice_opening_HVS_m2)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Temp_HVSr = Avg_Temp_stelldbnn_200cm)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Temp_HVSs = Avg_Temp_stelldbtn_200cm)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Chloride_HVSr = Avg_Chloride_stelldbnn_200cm)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Chloride_HVSs = Avg_Chloride_stelldbtn_200cm)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Tilt_HDC = Tilt_HDC_delta)
Env_DE_HVS_total <- dplyr::rename(Env_DE_HVS_total, Tilt_HS8 = Tilt_HS8_delta)

# NWW 
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, DateTime_UTC = Hour)
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, DE = det_eff)
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, Discharge_NWW = Avg_Discharge_NWW_m3_s)
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, Water_Height_NWW = Avg_Waterheight_NWW_cm)
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, Wind_Speed_NWW = Avg_WindSnelheid_HoekvanHolland_m_s)
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, Wind_Direction_NWW = Avg_Wind_Direction_HoekvanHolland_compass)
Env_DE_NWW_total <- dplyr::rename(Env_DE_NWW_total, Tilt_NW3 = Tilt_NW3_delta)

# Select specific transmitter - receiver combinations

# HVS river.  HD-C <-> HD-B & HD-D
Env_DE_HDC <- subset(Env_DE_HVS_total, Transmitter_location == "Haringvliet HD-C trans") # HD-C as transmitter# Remove periods before range testing and transition from low -> high power output
Low_PO_range_exl_HDC <- as.POSIXct("2023-04-05 14:00:00", tz = "UTC") # Low PO HDC (and HDG) from 2023-01-26 - 2023-03-15, and range test from 2023-03-15 09:00:00 - 2023-04-05 09:00:00
Env_DE_HDC <- subset(Env_DE_HDC, DateTime_UTC > Low_PO_range_exl_HDC)

# Now select relevant receivers around HD-C 
Env_DE_HDC_HDB <- subset(Env_DE_HDC, Receiver_location == "Haringvliet HD-B") # HD-B as receiver 
Env_DE_HDC_HDD <- subset(Env_DE_HDC, Receiver_location == "Haringvliet HD-D") # HD-D as receiver
Env_DE_HDC_HDE <- subset(Env_DE_HDC, Receiver_location == "Haringvliet HD-E") # HD-E as receiver

# HVS sea. HS6 <-> HS8.
Env_DE_HS6 <- subset(Env_DE_HVS_total, Transmitter_location == "Slijkgat HS6 trans") # Slijkgat HS6 as transmitter
Env_DE_HS6_HS8 <- subset(Env_DE_HS6, Receiver_location == "Slijkgat HS8") #  Slijkgat HS8 receiver 

# NWW. NW2 -> NW3.
Env_DE_NW2  <- subset(Env_DE_NWW_total, Transmitter_location == "NieuweWaterweg NW2 trans") # NW2 as transmitter
Env_DE_NW2_NW3  <- subset(Env_DE_NW2, Receiver_location == "NieuweWaterweg NW3") # NW3 as receiver

# Remove  irrelevant columns for HVS datasets. No need for the NWW dataset 
Env_DE_HDC_HDB <- select(Env_DE_HDC_HDB, -c(Battery_HS8, Temp_HS8, Noise_HS8, Tilt_HS8))
Env_DE_HDC_HDD <- select(Env_DE_HDC_HDD, -c(Battery_HS8, Temp_HS8, Noise_HS8, Tilt_HS8))
Env_DE_HDC_HDE <- select(Env_DE_HDC_HDE, -c(Battery_HS8, Temp_HS8, Noise_HS8, Tilt_HS8))
Env_DE_HS6_HS8 <- select(Env_DE_HS6_HS8, -c(Battery_HDC, Temp_HDC, Noise_HDC, Tilt_HDC))

# -->  Visualize data Env_variables_total in '' script
# -->  Visualize data 'Env_DE_HVS_total' & 'Env_DE_NWW_total' in 'Env_Var_DE_HVS_NWW_Plots_FINAL.R' script

# Keep all the dataframes needed for the Plots script 
#datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Getij_Spui_Kier_1hour', 'Enviro_combined_DE_1hour_HVS_HDC_HDB_SKC')
datasets_data_exploration <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW', 'Getij_Spui_Kier_data_UTC_total', 'Getij_Spui_Kier_1hour', 'Env_variables_total', 'DE_HVS_total','DE_NWW_total', 'Env_DE_HVS_total', 'Env_DE_NWW_total', 'Env_DE_HDC_HDB', 'Env_DE_HDC_HDB_Apr','Env_DE_HDC_HDB_Jul', 'Env_DE_HDC_HDB_Dec' ,'Env_DE_HDC_HDD', 'Env_DE_HDC_HDD_Apr', 'Env_DE_HDC_HDD_Jul', 'Env_DE_HDC_HDD_Dec', 'Env_DE_HDC_HDE', 'Env_DE_HS6_HS8', 'Env_DE_HS6_HS8_Apr' , 'Env_DE_HS6_HS8_Jul', 'Env_DE_HS6_HS8_Dec', 'Env_DE_NW2_NW3', 'Env_DE_NW2_NW3_Apr', 'Env_DE_NW2_NW3_Jul', 'Env_DE_NW2_NW3_Dec')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_data_exploration)
rm(list = clean_workspace)








