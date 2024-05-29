title: "1 hourly DE calculatoin for the HVS study area"
Author: "Luc Visser"
date: "2024-21-04"

# Clean workspace
rm(list = ls())library(ggpubr)
library(openxlsx)
library(dplyr)
library(ggtext)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(openxlsx)

##  BIN CALCULATIONS
Detections2023_2024_bins <- read.csv(file = "Detections2023_2024_total_conversed_trans_names.csv") #(Created in 'Abacus_DE_time_distance_HVS_NWW.R' script)

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

# Add unique identifier <- Worked initially but not anymore. Leave out for now but might use it later
#Detections2023_2024_bins <- Detections2023_2024_bins %>% 
  #mutate(obs = 1:n())

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
Detections2023_2024NoordPampusP1 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P1" & Transmitter_location == "NoordPampus P1 trans") # NoordPampus P1
Detections2023_2024NoordPampusP4 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P4" & Transmitter_location == "NoordPampus P4 trans") # NoordPampus P4
Detections2023_2024NoordPampusP6 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P6" & Transmitter_location == "NoordPampus P6 trans") # NoordPampus P6
Detections2023_2024NoordPampusP7 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P7" & Transmitter_location == "NoordPampus P7 trans") # NoordPampus P7
Detections2023_2024NoordPampusP8 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P8" & Transmitter_location == "NoordPampus P8 trans") # NoordPampus P8
Detections2023_2024NoordPampusP9 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P9" & Transmitter_location == "NoordPampus P9 trans") # NoordPampus P9
Detections2023_2024NoordPampusP10 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "NoordPampus P10" & Transmitter_location == "NoordPampus P10 trans") # NoordPampys P10
Detections2023_2024SlijkgatHS2 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Slijkgat HS2" & Transmitter_location == "Slijkgat HS2 trans") # Slijkgat HS2
Detections2023_2024SlijkgatHS4 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Slijkgat HS4" & Transmitter_location == "Slijkgat HS4 trans") # Slijkgat HS4
Detections2023_2024SlijkgatHS6 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Slijkgat HS6" & Transmitter_location == "Slijkgat HS6 trans") # Slijkgat HS6
Detections2023_2024SlijkgatHS8 <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Slijkgat HS8" & Transmitter_location == "Slijkgat HS8 trans") # Slijkgat HS8
Detections2023_2024HaringvlietHDC <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Haringvliet HD-C" & Transmitter_location == "Haringvliet HD-C trans") # Haringvliet HD-C
Detections2023_2024HaringvlietHDG <- Detections2023_2024_bins %>% 
  filter(Receiver_location == "Haringvliet HD-G" & Transmitter_location == "Haringvliet HD-G trans") # Haringvliet HD-G

# Count one-hourly detections for each station pair 
P1_count_1hour <- Detections2023_2024NoordPampusP1  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
P1_count_1hour$Transmitter_location <- "NoordPampus P1 trans"
P4_count_1hour <- Detections2023_2024NoordPampusP4  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
P4_count_1hour$Transmitter_location <- "NoordPampus P4 trans"
P6_count_1hour <- Detections2023_2024NoordPampusP6  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
P6_count_1hour$Transmitter_location <- "NoordPampus P6 trans"
P7_count_1hour <- Detections2023_2024NoordPampusP7  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally() 
P7_count_1hour$Transmitter_location <- "NoordPampus P7 trans"
P8_count_1hour <- Detections2023_2024NoordPampusP8  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
P8_count_1hour$Transmitter_location <- "NoordPampus P8 trans"
P9_count_1hour <- Detections2023_2024NoordPampusP9  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
P9_count_1hour$Transmitter_location <- "NoordPampus P9 trans"
P10_count_1hour <- Detections2023_2024NoordPampusP10  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
P10_count_1hour$Transmitter_location <- "NoordPampus P10 trans"
HS2_count_1hour <- Detections2023_2024SlijkgatHS2  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
HS2_count_1hour$Transmitter_location <- "Slijkgat HS2 trans"
HS4_count_1hour <- Detections2023_2024SlijkgatHS4  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
HS4_count_1hour$Transmitter_location <- "Slijkgat HS4 trans"
HS6_count_1hour <- Detections2023_2024SlijkgatHS6  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
HS6_count_1hour$Transmitter_location <- "Slijkgat HS6 trans"
HS8_count_1hour <- Detections2023_2024SlijkgatHS8  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
HS8_count_1hour$Transmitter_location <- "Slijkgat HS8 trans"
HDC_count_1hour <- Detections2023_2024HaringvlietHDC  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
HDC_count_1hour$Transmitter_location <- "Haringvliet HD-C trans"
HDG_count_1hour <- Detections2023_2024HaringvlietHDG  %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour) %>% 
  tally()
HDG_count_1hour$Transmitter_location <- "Haringvliet HD-G trans"

# 2.3 Merge data into one reference data set
expected_one_hourly_detections <- bind_rows(P1_count_1hour, P4_count_1hour, P6_count_1hour, P7_count_1hour, P8_count_1hour, P9_count_1hour, P10_count_1hour, HS2_count_1hour, HS4_count_1hour, HS6_count_1hour, HS8_count_1hour, HDC_count_1hour, HDG_count_1hour)
expected_total_detections <- bind_rows(Detections2023_2024NoordPampusP1, Detections2023_2024NoordPampusP4, Detections2023_2024NoordPampusP6, Detections2023_2024NoordPampusP7, Detections2023_2024NoordPampusP8, Detections2023_2024NoordPampusP9, Detections2023_2024NoordPampusP10, Detections2023_2024SlijkgatHS2, Detections2023_2024SlijkgatHS4, Detections2023_2024SlijkgatHS6, Detections2023_2024SlijkgatHS8, Detections2023_2024HaringvlietHDC, Detections2023_2024HaringvlietHDG)

# Load the transmitter information file and link important data such as transmitter owner and power output to expected detections
Transmitterinformation2023_2024 <- read.csv(file = "Transmitterinformation2023_2024.csv")
Transmitterinformation2023_2024_relevant <- subset(Transmitterinformation2023_2024, select = c("Transmitter_location", "Owner", "Power_setting"))
expected_one_hourly_detections <- merge(expected_one_hourly_detections, Transmitterinformation2023_2024_relevant, by = "Transmitter_location")

# create reference list of all receiver-tag combos. You select a certain set of receivers as well, but only for the DE ~ distance plots! All receivers are taken into account in the Abacus plots
Receiver_location <- c("NoordPampus P1", "NoordPampus P4", "NoordPampus P6", "NoordPampus P7", "NoordPampus P8", "NoordPampus P9", "NoordPampus P10", "Slijkgat HS2", "Slijkgat HS4", "Slijkgat HS6", "Slijkgat HS8", "Haringvliet HD-B", "Haringvliet HD-C", "Haringvliet HD-D", "Haringvliet HD-E", "Haringvliet HD-F", "Haringvliet HD-G", "Haringvliet HD-H", "Haringvliet HD-I", "Goereesesluis ZOUT", "Goereesesluis ZOET")
Transmitter_location <- c("NoordPampus P1 trans", "NoordPampus P4 trans", "NoordPampus P6 trans", "NoordPampus P7 trans", "NoordPampus P8 trans", "NoordPampus P9 trans", "NoordPampus P10 trans", "Slijkgat HS2 trans", "Slijkgat HS4 trans", "Slijkgat HS6 trans", "Slijkgat HS8 trans", "Haringvliet HD-C trans","Haringvliet HD-G trans")
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

#################################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#################################

# Bin and tally observed detections, then join with expected values

# 1 hour bins
one_hourly_observed_detections <- Detections2023_2024_bins %>% 
  mutate(hour = lubridate::floor_date(DateTime_UTC, '1 hour')) %>% 
  group_by(hour, Receiver_location, Transmitter_location) %>% 
  tally()
one_hourly_observed_detections <- one_hourly_observed_detections %>% 
  ungroup() # This is necessary or the complete() function will not work

# Select only HVS locations

# 1 hour bins
selection_HVS <- c("NoordPampus P1 trans", "NoordPampus P4 trans", "NoordPampus P6 trans", "NoordPampus P7 trans", "NoordPampus P8 trans", "NoordPampus P9 trans", "NoordPampus P10 trans", "Slijkgat HS2 trans", "Slijkgat HS4 trans", "Slijkgat HS6 trans", "Slijkgat HS8 trans", "Haringvliet HD-C trans","Haringvliet HD-G trans")
subset_one_hourly_observed_detections_Tran <- one_hourly_observed_detections[one_hourly_observed_detections$Transmitter_location %in% selection_HVS, ] 
selection_HVS_Receivers <- c("NoordPampus P1", "NoordPampus P4", "NoordPampus P6", "NoordPampus P7", "NoordPampus P8", "NoordPampus P9", "NoordPampus P10", "Slijkgat HS2", "Slijkgat HS4", "Slijkgat HS6", "Slijkgat HS8", "Haringvliet HD-B", "Haringvliet HD-C", "Haringvliet HD-D", "Haringvliet HD-E", "Haringvliet HD-F", "Haringvliet HD-G", "Haringvliet HD-H", "Haringvliet HD-I", "Goereesesluis ZOUT", "Goereesesluis ZOET")
subset_one_hourly_observed_detections_Tran_Rec <- subset_one_hourly_observed_detections_Tran[subset_one_hourly_observed_detections_Tran$Receiver_location %in% selection_HVS_Receivers,]


#################################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#################################


# Merge the observed detection data with the expected detections

# 1 hour bins
one_hourly_observed_expected_detections_HVS <- subset_one_hourly_observed_detections_Tran_Rec |> 
  full_join(expected_one_hourly_detections_expand)|> 
  arrange(hour, Transmitter_location) |>
  mutate(n = ifelse(is.na(n), 0, n))

# Calculate detection efficiency from obs/expected detection counts

# 1 hour bins
one_hourly_observed_expected_detections_HVS <- one_hourly_observed_expected_detections_HVS %>% 
  mutate(det_eff = (n/expected_n)*100)

one_hourly_observed_expected_detections_HVS <- one_hourly_observed_expected_detections_HVS %>%
  mutate(det_eff = ifelse(det_eff > 100, 100, det_eff))

#one_hourly_observed_expected_detections_HVS <- one_hourly_observed_expected_detections_HVS %>%  # <- Worked initially but not anymore. Leave out for now but might use it later
  #mutate(obs = 1:n()) %>% 
  #select(obs, hour, Receiver_location, Transmitter_location, n, expected_n, det_eff, Owner, Power_setting)

one_hourly_observed_expected_detections_HVS$det_eff <- round(one_hourly_observed_expected_detections_HVS$det_eff, 1) # Round det eff data to 1 decimal

# Clean working environment except for the relevant binned datasets
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW')
objects_in_workspace <- ls() # the enviro_combined_hours_averaged dataset is included here as you might be working in the environmental_variables sheet as well and you don't want to lose this specific dataset
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)

# Write final excel and csv files of these worksheets
#write.csv(one_hourly_observed_expected_detections_HVS, file = "DE_HVS_1hour_FINAL.csv", row.names = FALSE)
#write.xlsx(one_hourly_observed_expected_detections_HVS, file = "DE_HVS_1hour_FINAL.xlsx")
#write.csv(one_hourly_observed_expected_detections_NWW, file = "DE_NWW_1hour_FINAL.csv", row.names = FALSE)
#write.xlsx(one_hourly_observed_expected_detections_NWW, file = "DE_NWW_1hour_FINAL.xlsx")

