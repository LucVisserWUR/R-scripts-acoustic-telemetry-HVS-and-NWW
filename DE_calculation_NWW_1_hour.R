title: "1 hourly DE calculation for the NWW study area"
Author: "Luc Visser"
date: "2024-21-04"

# Clean workspace
# rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)

#load correct packages
library(tidyr)
library(dplyr)
library(ggtext)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(openxlsx)

##  BIN CALCULATIONS
Detections2023_2024_bins <- read.csv(file = "Detections2023_2024_total_conversed_trans_names.csv") #(Dataset was created in 'DetectionEfficiency_HVS_Graphs_Luc_Backup.R' script)

#set correct data names & types and clarify that transmitter data should be considered as 
Detections2023_2024_bins$Transmitter_location <- as.character(Detections2023_2024_bins$Transmitter_location)
Detections2023_2024_bins$DateTime_UTC <- as.POSIXct(Detections2023_2024_bins$DateTime_UTC, format = '%Y-%m-%d %H:%M:%S', tz="UTC" )

#select relevant columns & remove columns that only contain NAs
Detections2023_2024_bins <- Detections2023_2024_bins %>% 
  dplyr::select(c("DateTime_UTC", "Receiver_location", "Transmitter_location"))

# Arrange by time
Detections2023_2024_bins <- Detections2023_2024_bins %>% 
  arrange(DateTime_UTC, Transmitter_location)

# Select only relevant timepoints
relevant_time_min_utc <- as.POSIXct("2023-01-26 23:00:00", tz = "UTC") 
relevant_time_max_utc <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")
Detections2023_2024_bins <- Detections2023_2024_bins %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Detections2023_2024_bins <- Detections2023_2024_bins %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# Round the minimum and maximum date times to the nearest full hour
start_time <- round.POSIXt(min(Detections2023_2024_bins$DateTime_UTC), units = "hours")
end_time <- round.POSIXt(max(Detections2023_2024_bins$DateTime_UTC), units = "hours")

# Create a sequence of 1 hourly intervals rounded to the nearest full hour
hr_key_1hour <- data.frame(hour = seq.POSIXt(start_time, end_time, by = 'hour'))
hr_key_1hour <- head(hr_key_1hour, 8500) # This added an extra row but we want it to match out detections so we'll keep it to 600

# Check if transmitter- and receiver names are written correctly
Transmitter_names_check <- read.csv(file = "Detections2023_2024_total_conversed_trans_names.csv")
Transmitter_names_check_unique <- as.data.frame(unique(Detections2023_2024_bins$Transmitter_location))
Receiver_names_check_unique <- as.data.frame(unique(Detections2023_2024_bins$Receiver_location))

# Subset detections for each station pair NWW Trans-Rec pairs
Detections2023_2024BeerKanaal_B1 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "BeerKanaal B1" & Transmitter_location == "BeerKanaal B1 trans") # BeerKanaal B1
Detections2023_2024CalandKanaal_CA4 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "CalandKanaal CA4" & Transmitter_location == "CalandKanaal CA4 trans") # Calandkanaal CA4
Detections2023_2024NW3 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NieuweWaterweg NW3" & Transmitter_location == "NieuweWaterweg NW3 trans") # Nieuwe waterweg NW3
Detections2023_2024NW2 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NieuweWaterweg NW2" & Transmitter_location == "NieuweWaterweg NW2 trans") # NieuweWaterweg NW2
Detections2023_2024MaasmondMaas5 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Maasmond Maas 5" & Transmitter_location == "Maasmond Maas 5 trans") # Maasmond Maas5
Detections2023_2024MaasmondCA2_NW1 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Maasmond CA2-NW1" & Transmitter_location == "Maasmond CA2-NW1 trans") # Maasmond CA2-NW1
Detections2023_2024MaasmondMaas4 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Maasmond Maas 4" & Transmitter_location == "Maasmond Maas 4 trans") # Maasmond Maas 4
Detections2023_2024NW8 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NieuweWaterweg NW8" & Transmitter_location == "NieuweWaterweg NW8 trans") # NieuweWaterweg NW8
Detections2023_2024NW25 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NieuweWaterweg NW25" & Transmitter_location == "NieuweWaterweg NW25 trans") # NieuweWaterweg NW25

# Count one-hourly detections for each station pair 
B1_count_1hour <- Detections2023_2024BeerKanaal_B1  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
B1_count_1hour$Transmitter_location <- "BeerKanaal B1 trans"
CA4_count_1hour <- Detections2023_2024CalandKanaal_CA4  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
CA4_count_1hour$Transmitter_location <- "CalandKanaal CA4 trans"
NW3_count_1hour <- Detections2023_2024NW3  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
NW3_count_1hour$Transmitter_location <- "NieuweWaterweg NW3 trans"
NW2_count_1hour <- Detections2023_2024NW2  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
NW2_count_1hour$Transmitter_location <- "NieuweWaterweg NW2 trans"
Maas5_count_1hour <- Detections2023_2024MaasmondMaas5  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
Maas5_count_1hour$Transmitter_location <- "Maasmond Maas 5 trans"
CA2NW1_count_1hour <- Detections2023_2024MaasmondCA2_NW1  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
CA2NW1_count_1hour$Transmitter_location <- "Maasmond CA2-NW1 trans"
Maas4_count_1hour <- Detections2023_2024MaasmondMaas4  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
Maas4_count_1hour$Transmitter_location <- "Maasmond Maas 4 trans"
NW8_count_1hour <- Detections2023_2024NW8  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
NW8_count_1hour$Transmitter_location <- "NieuweWaterweg NW8 trans"
NW25_count_1hour <- Detections2023_2024NW25  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
NW25_count_1hour$Transmitter_location <- "NieuweWaterweg NW25 trans"

# 2.3 Merge data into one reference data set
expected_one_hourly_detections <- bind_rows(B1_count_1hour, CA4_count_1hour, NW3_count_1hour, NW2_count_1hour, Maas5_count_1hour, CA2NW1_count_1hour, Maas4_count_1hour, NW8_count_1hour, NW25_count_1hour)
expected_total_detections <- bind_rows(Detections2023_2024BeerKanaal_B1, Detections2023_2024CalandKanaal_CA4, Detections2023_2024NW3, Detections2023_2024NW2, Detections2023_2024MaasmondMaas5, Detections2023_2024MaasmondCA2_NW1, Detections2023_2024MaasmondMaas4,Detections2023_2024NW8, Detections2023_2024NW25)

# Load the transmitter information file and link important data such as transmitter owner and power output to expected detections
Transmitterinformation2023_2024 <- read.csv(file = "Transmitterinformation2023_2024.csv")
Transmitterinformation2023_2024_relevant <- subset(Transmitterinformation2023_2024, select = c("Transmitter_location", "Owner", "Power_setting"))
expected_one_hourly_detections <- merge(expected_one_hourly_detections, Transmitterinformation2023_2024_relevant, by = "Transmitter_location")

# create reference list of all receiver-tag combos. You select a certain set of receivers as well, but only for the DE ~ distance plots! All receivers are taken into account in the Abacus plots
Receiver_location <- c("BeerKanaal B1", "CalandKanaal CA4", "NieuweWaterweg NW3", "NieuweWaterweg NW2", "Maasmond Maas 5", "Maasmond CA2-NW1", "Maasmond Maas 4", "NieuweWaterweg NW8", "NieuweWaterweg NW25", "Maasvlakte-Maasgeul Maas1")
Transmitter_location <- c("BeerKanaal B1 trans", "CalandKanaal CA4 trans", "NieuweWaterweg NW3 trans", "NieuweWaterweg NW2 trans", "Maasmond Maas 5 trans", "Maasmond CA2-NW1 trans", "Maasmond Maas 4 trans", "NieuweWaterweg NW8 trans", "NieuweWaterweg NW25 trans")
receiver_tag_combos <- expand.grid(Transmitter_location, Receiver_location)
receiver_tag_combos <- plyr::rename(receiver_tag_combos, c("Var1" = "Transmitter_location"))
receiver_tag_combos <- plyr::rename(receiver_tag_combos, c("Var2" = "Receiver_location"))

# Join the expected number of detections with the created receiver-transmitter combo list

# 1 hour bins
expected_one_hourly_detections_expand <- expected_one_hourly_detections |>
  full_join(receiver_tag_combos) %>% 
  select(hour, Receiver_location, Transmitter_location, n, Owner, Power_setting)|>
  arrange(hour)

# Rename n column to expected n
expected_one_hourly_detections_expand <- plyr::rename(expected_one_hourly_detections_expand, c("n" = "expected_n")) # Now we have a fully expanded dataset to compare to

# Bin and tally observed detections, then join with expected values

# 1 hour bins
one_hourly_observed_detections <- Detections2023_2024_bins %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour, Receiver_location, Transmitter_location) %>% 
  tally()
one_hourly_observed_detections <- one_hourly_observed_detections %>% 
  ungroup() # This is necessary or the complete() function will not work


# Select only NWW locations

# 1 hour bins
selection_NWW <- c("BeerKanaal B1 trans","CalandKanaal CA4 trans", "NieuweWaterweg NW3 trans", "NieuweWaterweg NW2 trans", "Maasmond Maas 5 trans", "Maasmond CA2-NW1 trans", "Maasmond Maas 4 trans", "NieuweWaterweg NW8 trans", "NieuweWaterweg NW25 trans")
subset_one_hourly_observed_detections_Tran <- one_hourly_observed_detections[one_hourly_observed_detections$Transmitter_location %in% selection_NWW, ] 
selection_NWW_Receivers <- c("BeerKanaal B1", "CalandKanaal CA4", "NieuweWaterweg NW3", "NieuweWaterweg NW2", "Maasmond Maas 5", "Maasmond CA2-NW1", "Maasmond Maas 4", "NieuweWaterweg NW8", "NieuweWaterweg NW25", "Maasvlakte-Maasgeul Maas1")
subset_one_hourly_observed_detections_Tran_Rec <- subset_one_hourly_observed_detections_Tran[subset_one_hourly_observed_detections_Tran$Receiver_location %in% selection_NWW_Receivers,]

# Merge the observed detection data with the expected detections

# 1 hour bins
one_hourly_observed_expected_detections_NWW <- subset_one_hourly_observed_detections_Tran_Rec |> 
  full_join(expected_one_hourly_detections_expand)|> 
  arrange(hour, Transmitter_location) |>
  mutate(n = ifelse(is.na(n), 0, n))

# Calculate detection efficiency from obs/expected detection counts

# 1 hour bins
one_hourly_observed_expected_detections_NWW <- one_hourly_observed_expected_detections_NWW %>% 
  mutate(det_eff = (n/expected_n)*100)

one_hourly_observed_expected_detections_NWW <- one_hourly_observed_expected_detections_NWW %>%
  mutate(det_eff = ifelse(det_eff > 100, 100, det_eff))

one_hourly_observed_expected_detections_NWW$det_eff <- round(one_hourly_observed_expected_detections_NWW$det_eff, 1) # Round det eff data to 1 decimal

# Clean working environment except for the relevant binned datasets
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)

