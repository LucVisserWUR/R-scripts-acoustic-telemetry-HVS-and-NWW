#title: "Detection efficiency HVs graphs BACKUP VERSION"
#Author: "Luc Visser"
#date: "2024-04-03"

#load correct packages
library(tidyr)
library(dplyr)
library(ggtext)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(openxlsx)

# Clean workspace 
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)
knitr::opts_chunk$set(echo = TRUE)

# Chapters
# 1. Load and format detection data set
# 2. Link the detection data set to inter-receiver distance to order Y-axis of abacus plots
# 3. Create abacus plots of the detection data set
# 4. Plot detection efficiency (%) over distance for the complete time period from Jan 2023 - Jan 2024
# 5. Plot detection efficiency (%) over two-week time periods for short distance receiver pairs (to check detection efficiency over time)

## 1. LOAD AND FORMAT DETECTION DATASET

Detections2023_2024_total <- read.csv(file = "Detections2023_2024.csv")
Receiverinformation2023_2024 <- read.csv(file = "Receiverinformation2023_2024.csv") # To check the receiver info when needed
Transmitterinformation2023_2024 <- read.csv(file = "Transmitterinformation2023_2024.csv") # To check the Transmitter info when needed

#Format the total detection dataset correctly
str(Detections2023_2024_total)
Detections2023_2024_total <- plyr::rename(Detections2023_2024_total, c("Date.and.Time..UTC." = "DateTime_UTC"))
Detections2023_2024_total$DateTime_UTC <- as.POSIXct(Detections2023_2024_total$DateTime_UTC, format = '%Y-%m-%d %H:%M:%S', tz="UTC" )
Detections2023_2024_total <- plyr::rename(Detections2023_2024_total, c("Receiver" = "Receiver_name"))
Detections2023_2024_total <- plyr::rename(Detections2023_2024_total, c("Station.Name" = "Receiver_location"))
Detections2023_2024_total <- plyr::rename(Detections2023_2024_total, c("Transmitter" = "Transmitter_name"))
Detections2023_2024_total$Transmitter_name <- as.character(Detections2023_2024_total$Transmitter_name)

# Rename transmitter names to transmitter locations for clarification
# All rows that contain transmitter name that is not in HVS or NWW network, such as transmitters in fish, will be removed
Transmitter_name_conversion <- read.csv(file = "Transmitter_name_location_conversion.csv")
Detections2023_2024_total_conversed_trans_names <- merge(Transmitter_name_conversion, Detections2023_2024_total, by = "Transmitter_name")

# Select detections within the relevant time period; after deploying receivers and before Januaru 2024 read-out moment
relevant_time_min_utc <- as.POSIXct("2023-01-26 23:00:00", tz = "UTC") 
relevant_time_max_utc <- as.POSIXct("2024-01-25 09:00:00", tz = "UTC")
Detections2023_2024_total_conversed_trans_names <- Detections2023_2024_total_conversed_trans_names %>%
  filter(DateTime_UTC > relevant_time_min_utc)
Detections2023_2024_total_conversed_trans_names <- Detections2023_2024_total_conversed_trans_names %>%
  filter(DateTime_UTC < relevant_time_max_utc)

# Create a csv file of the detection dataset with transformed names to use for calculation of DE in other scripts
write.xlsx(Detections2023_2024_total_conversed_trans_names, "Detections2023_2024_total_conversed_trans_names.xlsx") 
write.csv(Detections2023_2024_total_conversed_trans_names, "Detections2023_2024_total_conversed_trans_names.csv") 

#Select only relevant columns from total_detection sheet for the abacus plots
Detections2023_2024_total_abacus <- Detections2023_2024_total_conversed_trans_names %>%
  select(DateTime_UTC, Transmitter_location, Receiver_location)

## 2. LINK THE DECECTION DATASET TO INTER-RECEIVER DISTANCE TO ORDER Y-AXIS OF ABACUS PLOTS

# Import the csv file created with the script 'Inter_receiver_distances_correct' 
Receiver_distances_abacus <- read.csv(file = "Receiver_distances_matrix_correct.csv") # Load distances to link 
colnames(Receiver_distances_abacus)[1] <- "Receiver_location" # Rename  first column name
colnames(Receiver_distances_abacus) <- gsub("\\.", " ", colnames(Receiver_distances_abacus)) # Replace the dots in distance column names with blank spaces

# Rename certain column names in the distances Receiver_distances_abacus to achieve successful merging with Detections2023_2024_total_abacus
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-B` = `Haringvliet HD B`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-C` = `Haringvliet HD C`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-D` = `Haringvliet HD D`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-E` = `Haringvliet HD E`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-F` = `Haringvliet HD F`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-G` = `Haringvliet HD G`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-H` = `Haringvliet HD H`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Haringvliet HD-I` = `Haringvliet HD I`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Hartelkanaal SB-B` = `Hartelkanaal SB B`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Hartelkanaal SB-E` = `Hartelkanaal SB E`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Maasvlakte-Maasgeul Maas1` = `Maasvlakte Maasgeul Maas1`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Maasvlakte-Maasgeul MV N` = `Maasvlakte Maasgeul MV N`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Maasvlakte-Maasgeul MV A` = `Maasvlakte Maasgeul MV A`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Maasvlakte-Maasgeul MVB` = `Maasvlakte Maasgeul MVB`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Maasvlakte-Maasgeul MVC` = `Maasvlakte Maasgeul MVC`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Noordzee HP-WNB 15A` = `Noordzee HP WNB 15A`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Oude Maas BB-C` = `Oude Maas BB C`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`Oude Maas BB-E` = `Oude Maas BB E`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`AMER - NOORDERGAT` = `AMER   NOORDERGAT`)
Receiver_distances_abacus <- Receiver_distances_abacus %>% dplyr::rename(`AMER - ZUIDERGAT` = `AMER   ZUIDERGAT`)

# Turn Receiver_distances_abacus dataset into a long format
Receiver_distances_abacus_long <- pivot_longer(Receiver_distances_abacus, 
                                               cols = -Receiver_location, 
                                               names_to = "Transmitter_location", 
                                               values_to = "Distance")

Receiver_distances_abacus_long <- Receiver_distances_abacus_long[, c("Transmitter_location", "Receiver_location", "Distance")] # put in right orde
Receiver_distances_abacus_long$Transmitter_location <- paste(Receiver_distances_abacus_long$Transmitter_location, "trans", sep = " ") # Change transmitter_location name to ensure proper merging with detection file

# Create a new column that combines names of transmitter and receiver for distance matrix and detection dataset
Receiver_distances_abacus_long <- Receiver_distances_abacus_long %>%
  mutate(Trans_Rec_Combo = paste(Transmitter_location, Receiver_location, sep = "_"))
Receiver_distances_abacus_long <- subset(Receiver_distances_abacus_long, select = -c(Transmitter_location, Receiver_location)) # Remove tran and rec column to avoid duplication of these columns after merging

Detections2023_2024_total_abacus <- Detections2023_2024_total_abacus %>%
  mutate(Trans_Rec_Combo = paste(Transmitter_location, Receiver_location, sep = "_"))

# Merge total detection data set with distances. 
Detections2023_2024_total_abacus_distances <- merge(Detections2023_2024_total_abacus, Receiver_distances_abacus_long, 
                                                    by = c("Trans_Rec_Combo"))

Detections2023_2024_total_abacus_distances <- subset(Detections2023_2024_total_abacus_distances, select = - Trans_Rec_Combo) # Remove tran and rec column to avoid duplication of these columns after merging
Detections2023_2024_total_abacus_distances <- arrange(Detections2023_2024_total_abacus_distances, DateTime_UTC) # Arrange on time

## 3. CREATE ABACUS PLOTS OF THE DETECTION DATASET

#Create subsets for sync transmitters of interest
Detections_HS6_trans <- subset(Detections2023_2024_total_abacus_distances, Transmitter_location == "Slijkgat HS6 trans") # Slijkgat HS6
Detections_P9_trans<- subset(Detections2023_2024_total_abacus_distances, Transmitter_location == "NoordPampus P9 trans") # NoordPampus P9
Detections_HDC_trans <- subset(Detections2023_2024_total_abacus_distances, Transmitter_location == "Haringvliet HD-C trans") # Haringvliet HD-C
Detections_HDG_trans <- subset(Detections2023_2024_total_abacus_distances, Transmitter_location == "Haringvliet HD-G trans") # Haringvliet HD-G
Detections_NW2_trans <- subset(Detections2023_2024_total_abacus_distances, Transmitter_location == "NieuweWaterweg NW2 trans") # NieuweWaterweg NW2
Detections_B1_trans <- subset(Detections2023_2024_total_abacus_distances, Transmitter_location == "BeerKanaal B1 trans") # BeerKanaal B1

# Arrange receiver locations on distance and create Abacus plots of the sync transmitter subsets
Detections_B1_trans$Receiver_location <- factor(Detections_B1_trans$Receiver_location, levels = unique(Detections_B1_trans$Receiver_location[order(-Detections_B1_trans$Distance)]))
ggplot(Detections_B1_trans, aes(x = DateTime_UTC, y = Receiver_location)) +
  #geom_rect(xmin = as.POSIXct("2023-01-26 16:10:00"), xmax = as.POSIXct("2023-03-15 09:00:00"), # Only for HD-C and HD-G. Indicates the period of low power output
  #ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.1)+
  #geom_vline(xintercept = as.numeric(as.POSIXct("2023-03-15 09:00:00")), linetype = "dashed", color = "red") + # Only for HD-C and HD-G. Range test took place form 03-15 till 04-05 2023 
  #geom_vline(xintercept = as.numeric(as.POSIXct("2023-04-05 09:00:00")), linetype = "dashed", color = "red") +  # Only for HD-C and HD-G. Range test took place form 03-15 till 04-05 2023
  geom_point(size=0.5) +
  labs(title = "Detections of sync transmitter BeerKanaal B1 between Jan 2023 - Jan 2024 ",
       x = "Time (Month, Year)",
       y = "Receiver location") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
        axis.title.x = element_text(face = "bold", size = 12),  
        axis.title.y = element_text(face = "bold", size = 12))

## 4. PLOT detection efficiency (%) OVER DISTANCE FOR THE COMPLETE TIME PERIOD FROM JAN 2023 - JAN 2024

# Load one_hourly_observed_expected_detections_HVS and one_hourly_observed_expected_detections_NWW in DE_calculations_1hour_HVS / DE_calculations_1hour_NWW and get 'Receiver_distances_abacus_long' from above in this script
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW', 'Receiver_distances_abacus_long')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)

# Shorten name to make easier to comprehend
Det_eff_HVS_1hour <- one_hourly_observed_expected_detections_HVS
Det_eff_NWW_1hour <- one_hourly_observed_expected_detections_NWW

# Create extra Trans _ Rec column in DE data to enable merging with distance data
# HVS
Det_eff_HVS_1hour <- Det_eff_HVS_1hour %>%
  mutate(Trans_Rec_Combo = paste(Transmitter_location, Receiver_location, sep = "_"))

# NWW
Det_eff_NWW_1hour <- Det_eff_NWW_1hour %>%
  mutate(Trans_Rec_Combo = paste(Transmitter_location, Receiver_location, sep = "_"))


# Merge DE data set with distances and format datasets afterwards
# HVS
Det_eff_HVS_1hour_distances <- merge(Det_eff_HVS_1hour, Receiver_distances_abacus_long, 
                                     by = c("Trans_Rec_Combo"))
Det_eff_HVS_1hour_distances <- subset(Det_eff_HVS_1hour_distances, select = - Trans_Rec_Combo) # Remove tran and rec column to avoid duplication of these columns after merging
Det_eff_HVS_1hour_distances <- arrange(Det_eff_HVS_1hour_distances, hour) # Arrange on time

# NWW
Det_eff_NWW_1hour_distances <- merge(Det_eff_NWW_1hour, Receiver_distances_abacus_long, 
                                     by = c("Trans_Rec_Combo"))
Det_eff_NWW_1hour_distances <- subset(Det_eff_NWW_1hour_distances, select = - Trans_Rec_Combo) 
Det_eff_NWW_1hour_distances <- arrange(Det_eff_NWW_1hour_distances, hour) # Arrange on time

# Make a list of relevant receivers for HVS river and HVS sea
# You don't want signal transmitted over the dam (e.g. between HS8 and HD-D) to be included as these will cause weird DE~distance relationships 
HVS_river_receivers <- c("Haringvliet HD-B", "Haringvliet HD-C", "Haringvliet HD-D", "Haringvliet HD-E", "Haringvliet HD-F", "Haringvliet HD-G", "Haringvliet HD-H")
HVS_sea_receivers <- c("Slijkgat HS2", "Slijkgat HS4", "Slijkgat HS6", "Slijkgat HS8", "NoordPampus P1", "NoordPampus P4", "NoordPampus P6", "NoordPampus P7", "NoordPampus P8", "NoordPampus P9", "NoordPampus P10")
NWW_receivers <- c("NieuweWaterweg NW2", "NieuweWaterweg NW3", "CalandKanaal CA4", "BeerKanaal B1", "Maasmond Maas 5", "Maasmond CA2-NW1", "Maasmond Maas 4")

# Make subsets of HVS river, HVS sea and NWW with only relevant receiver
Det_eff_HVS_river_1hour_distances <- Det_eff_HVS_1hour_distances[Det_eff_HVS_1hour_distances$Receiver_location %in% HVS_river_receivers, ]
Det_eff_HVS_sea_1hour_distances <- Det_eff_HVS_1hour_distances[Det_eff_HVS_1hour_distances$Receiver_location %in% HVS_sea_receivers, ]
Det_eff_NWW_1hour_distances <- Det_eff_NWW_1hour_distances[Det_eff_NWW_1hour_distances$Receiver_location %in% NWW_receivers, ]

# Select relevant sync transmitters from the merged DE distance datasets
Det_eff_HVS_1hour_distances_HDC <- subset(Det_eff_HVS_river_1hour_distances, Transmitter_location == "Haringvliet HD-C trans") # HVS river
Det_eff_HVS_1hour_distances_HDG <- subset(Det_eff_HVS_river_1hour_distances, Transmitter_location == "Haringvliet HD-G trans")
Det_eff_HVS_1hour_distances_HS6 <- subset(Det_eff_HVS_sea_1hour_distances, Transmitter_location == "Slijkgat HS6 trans") # HVS sea
Det_eff_HVS_1hour_distances_P9 <- subset(Det_eff_HVS_sea_1hour_distances, Transmitter_location == "NoordPampus P9 trans") 
Det_eff_NWW_1hour_distances_NW2 <- subset(Det_eff_NWW_1hour_distances, Transmitter_location == "NieuweWaterweg NW2 trans") # NWW
Det_eff_NWW_1hour_distances_B1 <- subset(Det_eff_NWW_1hour_distances, Transmitter_location == "BeerKanaal B1 trans") 

# Create a table of distances for the different receivers surrounding the sync transmitter to add in the graph for clarity
Receiver_distances_table <- Det_eff_NWW_1hour_distances_B1 %>%
  dplyr::select(c(Receiver_location, Distance))
unique_receiver_distances_table <- aggregate(Distance ~ Receiver_location, data = Receiver_distances_table, FUN = unique)

# Calculate the mean DE (%) per distance to incorporate in the DE(%) vs distance graph
mean_det_eff_per_distance_1hour <- aggregate(det_eff ~ Distance, data = Det_eff_NWW_1hour_distances_B1, FUN = mean)
mean_det_eff_per_distance_1hour$Distance <- as.numeric(mean_det_eff_per_distance_1hour$Distance)
mean_det_eff_per_distance_1hour$det_eff <- as.numeric(mean_det_eff_per_distance_1hour$det_eff)

# Plot DE ~ distance between 26 Jan 2023 - 26 Jan 2024
Det_eff_NWW_1hour_distances_B1$Distance <- as.numeric(Det_eff_NWW_1hour_distances_B1$Distance)
Det_eff_NWW_1hour_distances_B1$det_eff <- as.numeric(Det_eff_NWW_1hour_distances_B1$det_eff )
plot(Det_eff_NWW_1hour_distances_B1$Distance, Det_eff_NWW_1hour_distances_B1$det_eff,
     xlab = "Distance (m)", ylab = "Detection Efficiency (Per hour, %)", 
     main = "Detection efficiency of BeerKanaal B1 over distance to surrounding receivers between Jan 2023 - Jan 2024")+
  points(mean_det_eff_per_distance_1hour$Distance, mean_det_eff_per_distance_1hour$det_eff, col = "red", pch = 13)+
  lines(mean_det_eff_per_distance_1hour$Distance, mean_det_eff_per_distance_1hour$det_eff, col = "red")+
  legend("topright", legend = "Mean detection efficiency between Jan 2023 - Jan 2024 ", col = "red", text.col = "red", bty = "o", text.font = 2)+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
        axis.title.x = element_text(face = "bold", size = 12),  
        axis.title.y = element_text(face = "bold", size = 12))


## 5. PLOT detection efficiency (%) OVER TWO-WEEK TIME PERIODS FOR SHORT DISTANCE RECEIVER PAIRS (TO CHECK detection efficiency OVER TIME)

# Load these DE dataframes from the 'DE_calculation_HVS_1_hour' and 'DE_calculation_NWW_1_hour datasets'
datasets_to_keep <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep)
rm(list = clean_workspace)

# Shorten name to make easier to comprehend
Det_eff_HVS_1hour <- one_hourly_observed_expected_detections_HVS
Det_eff_NWW_1hour <- one_hourly_observed_expected_detections_NWW

# Set data of the DE datasets in correct format
Det_eff_HVS_1hour$hour <- as.POSIXct(Det_eff_HVS_1hour$hour, format = '%Y-%m-%d %H:%M:%S', tz="UTC" )
Det_eff_NWW_1hour$hour <- as.POSIXct(Det_eff_NWW_1hour$hour, format = '%Y-%m-%d %H:%M:%S', tz="UTC" )

# Select relevant two-week periods for DE ~ time graphs

# HVS
Det_eff_HVS_1hour_Apr <- subset(Det_eff_HVS_1hour, hour > "2023-04-10 09:00:00" & hour < "2023-04-24 09:00:00" )  
Det_eff_HVS_1hour_Jul <- subset(Det_eff_HVS_1hour, hour > "2023-07-10 09:00:00" & hour < "2023-07-24 09:00:00" )
Det_eff_HVS_1hour_Dec <- subset(Det_eff_HVS_1hour, hour > "2023-12-10 09:00:00" & hour < "2023-12-24 09:00:00" )

# NWW
Det_eff_NWW_1hour_Apr <- subset(Det_eff_NWW_1hour, hour > "2023-04-10 09:00:00" & hour < "2023-04-24 09:00:00" )  
Det_eff_NWW_1hour_Jul <- subset(Det_eff_NWW_1hour, hour > "2023-07-10 09:00:00" & hour < "2023-07-24 09:00:00" )
Det_eff_NWW_1hour_Dec <- subset(Det_eff_NWW_1hour, hour > "2023-12-10 09:00:00" & hour < "2023-12-24 09:00:00" )

# Select specific sync transmitters receivers for the DE ~ time graphs: HD-C, HS6, NW2

# HVS river
Det_eff_HDC_Apr <- subset(Det_eff_HVS_1hour_Apr, Transmitter_location == "Haringvliet HD-C trans")
Det_eff_HDC_Jul <- subset(Det_eff_HVS_1hour_Jul, Transmitter_location == "Haringvliet HD-C trans")
Det_eff_HDC_Dec <- subset(Det_eff_HVS_1hour_Dec, Transmitter_location == "Haringvliet HD-C trans") # <- HD-C / HD-G data after 10-10-2023 not valid(ated)

# HVS sea
Det_eff_HS6_Apr <- subset(Det_eff_HVS_1hour_Apr, Transmitter_location == "Slijkgat HS6 trans")
Det_eff_HS6_Jul <- subset(Det_eff_HVS_1hour_Jul, Transmitter_location == "Slijkgat HS6 trans")
Det_eff_HS6_Dec <- subset(Det_eff_HVS_1hour_Dec, Transmitter_location == "Slijkgat HS6 trans")

# NWW
Det_eff_NW2_Apr <- subset(Det_eff_NWW_1hour_Apr, Transmitter_location == "NieuweWaterweg NW2 trans")
Det_eff_NW2_Jul <- subset(Det_eff_NWW_1hour_Jul, Transmitter_location == "NieuweWaterweg NW2 trans")
Det_eff_NW2_Dec <- subset(Det_eff_NWW_1hour_Dec, Transmitter_location == "NieuweWaterweg NW2 trans")

# Select specific receivers for the DE ~ time graphs: HD-C - HD-B & HD-D, HS6 - HS8, NW2 - NW3

# HVS river
Det_eff_HDCtrans_HDE_Apr <- subset(Det_eff_HDC_Apr, Receiver_location == "Haringvliet HD-E")
Det_eff_HDCtrans_HDD_Apr <- subset(Det_eff_HDC_Apr, Receiver_location == "Haringvliet HD-D")
Det_eff_HDCtrans_HDB_Jul <- subset(Det_eff_HDC_Jul, Receiver_location == "Haringvliet HD-B")
Det_eff_HDCtrans_HDD_Jul <- subset(Det_eff_HDC_Jul, Receiver_location == "Haringvliet HD-D")
Det_eff_HDCtrans_HDB_Dec <- subset(Det_eff_HDC_Dec, Receiver_location == "Haringvliet HD-B") # <- HD-C / HD-G data after 10-10-2023 not valid(ated)
Det_eff_HDCtrans_HDD_Dec <- subset(Det_eff_HDC_Dec, Receiver_location == "Haringvliet HD-D") # <- HD-C / HD-G data after 10-10-2023 not valid(ated)

# HVS sea
Det_eff_HS6trans_HS8_Apr <- subset(Det_eff_HS6_Apr, Receiver_location == "Slijkgat HS8")
Det_eff_HS6trans_HS8_Jul <- subset(Det_eff_HS6_Jul, Receiver_location == "Slijkgat HS8")
Det_eff_HS6trans_HS8_Dec <- subset(Det_eff_HS6_Dec, Receiver_location == "Slijkgat HS8")

# NWW
Det_eff_NW2trans_NW3_Apr <- subset(Det_eff_NW2_Apr, Receiver_location == "NieuweWaterweg NW3")
Det_eff_NW2trans_NW3_Jul <- subset(Det_eff_NW2_Jul, Receiver_location == "NieuweWaterweg NW3")
Det_eff_NW2trans_NW3_Dec <- subset(Det_eff_NW2_Dec, Receiver_location == "NieuweWaterweg NW3")

# Plot DE over the selected two-week periods for HVS river, HVS sea and NWW
ggplot() +
  geom_point() +   
  geom_line(data = Det_eff_HDCtrans_HDE_Apr, aes(x = hour, y = det_eff), color = "red") +    
  #geom_line(data = Det_eff_HDCtrans_HDB_Dec, aes(x = hour, y = det_eff), color = "blue") +
  geom_point(data = Det_eff_HDCtrans_HDE_Apr, aes(x = hour, y = det_eff), color = "red") +    
  #geom_point(data = Det_eff_HDCtrans_HDB_Dec, aes(x = hour, y = det_eff), color = "blue") +
  scale_color_manual(values = colours) +
  labs(title = "Detection efficiency between transmitter Haringvliet HD-C and receiver HD-E (403m) in December 2023",
       x = "Time (Days)",
       y = "Detection efficiency (Per hour, %)")+ 
  scale_x_datetime(date_breaks = "2 days") +  
  ylim(0, 100) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  
        axis.title.x = element_text(face = "bold", size = 12),  
        axis.title.y = element_text(face = "bold", size = 12))+
  guides(color = guide_legend(override.aes = list(color = "black")))




