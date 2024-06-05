title: "Environmental factors analysis for HVS and NWW  networks" 
Author: "Luc Visser"
date: "2024-20-04"

# Load correct packages
library(tidyr) # CONTINUE AT LINE 1400, CHAPTER 12 WITH KIER/SPUI/CLOSED DATA
library(dplyr)
library(ggtext)
library(lubridate)
library(ggplot2)
library(viridis)
library(ggpubr)
library(openxlsx)
library(readxl)
library(GGally) # To create the ggpairs in chapter 10
library(patchwork) # To patch graphs to each other, used in chapter 11
library(corrplot) # To create correlation plots in chapter 10. Not really used eventually
library(readr) # Read in csv files while maintaining proper DateTime format incl times for 00:00:00 times. Used in o.a. Chapter 12 when reading the Spui_Kier_closed_hour_bins file
library(TukeyC)
library(gridExtra) # Plot multiple graphs in one. Used in Chapter 12 to merge multiple bargraphs of Spui / Kier / Closed together
library(mgcv) # GAM analysis, chapter 7

#rm(list = ls())
datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_SKC','Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec_SKC', 'Getij_Spui_Kier_1hour')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep_complete)
rm(list = clean_workspace)
knitr::opts_chunk$set(echo = TRUE)

# Chapters:
# Plots 1. Analysis of 10 min discharge data over time. 
# Plots 2. Analysis of sluice management with 10min water height, sluice opening and discharge data.  
# Plots 3. Analysis of 1 hourly env. variables against time. 
# Plots 4. Correlation between env. variables using ggpairs 
# Plots 5. Analysis of specific interactions within the receiver information data 

## Plots 1. ANALYSIS OF 10 MIN DISCHARGE DATA OVER TIME

datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Enviro_combined_DE_1hour_HVS_HDC_HDB_SKC', 'Getij_Spui_Kier_1hour')
#datasets_to_keep_Plots_1 <- c('Discharge_HVS', 'Discharge_relevant')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep_complete)
rm(list = clean_workspace)

# Create Abacus plot of HVS discharge over complete time period before 2023 (2016 - 2023)
ggplot(Discharge_HVS, aes(x = DateTime_UTC, y = Discharge_HVS_m3_s)) +
  geom_point(size=0.5) +
  labs(title = "Abacus Plot of Discharge at Haringvlietsluizen between 2016 - 2023",
       x = "DateTime_UTC",
       y = "Total discharge Haringvliet sluizen (m3/s)") +
  theme_minimal() 

# Create Abacus plot of discharge over the considered time period 2023 (Jan 2023 - Jan 2024)

# HVS
ggplot(Discharge_relevant, aes(x = DateTime_UTC, y = Discharge_HVS_m3_s)) +
  geom_point(size=0.5) +
  labs(title = "Abacus Plot of discharge at Haringvlietsluizen in between Jan 2023 - Jan 2024",
       x = "DateTime_UTC",
       y = "Total discharge Haringvliet sluizen measured every 10 minutes (m3/s)") +
  theme_minimal() 

# NWW
ggplot(Discharge_relevant, aes(x = DateTime_UTC, y = Discharge_NWW_m3_s)) +
  geom_point(size=0.5) +
  labs(title = "Abacus Plot of Discharge at Hoek van Holland (proximate to NWW) in 2023",
       x = "DateTime_UTC",
       y = "Total discharge Hoek van Holland measured every 10 minutes (m3/s)") +
  theme_minimal()


## Plots 2. ANALYSIS OF SLUICE MANAGEMENT WITH 10MIN WATERHEIGHT, SLUICE OPENING AND DISCHARGE DATA

datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Enviro_combined_DE_1hour_HVS_HDC_HDB_SKC', 'Getij_Spui_Kier_1hour')
#datasets_to_keep_Plots_2 <- c('HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep_complete)
rm(list = clean_workspace)

# 2.1. Plot sluice management variables over time

# HVS management April 2023: Delta_waterheight & Discharge over time
ggplot(data = HVS_management_Apr, aes(x = DateTime_UTC)) +
  geom_line(aes(y = Delta_Waterheight, color = "Delta_waterheight")) +
  geom_line(aes(y = Discharge_HVS_m3_s, color = "Discharge_HVS_m3_s")) +
  scale_color_manual(values = c("red", "blue")) +
  xlab("Time (Year - Month - Day)")+
  ylab("Delta waterheight (River - Sea (cm))") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Discharge (m3 / s)"))+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%Y-%m-%d")+
  theme(axis.text.x = element_text(margin = margin(t = 10)))+
  theme(axis.text.y = element_text(margin = margin(t = 10)))+
  ggtitle("HVS Discharge and delta waterheight (river-sea) over time in April 2023")

# HVS management April 2023: Delta_waterheight & Total sluice height (m2) over time
ggplot(data = HVS_management_Apr, aes(x = DateTime_UTC)) +
  geom_line(aes(y = Delta_Waterheight, color = "Delta waterheight")) +
  geom_line(aes(y = Total_sluice_opening_HVS_m2, color = "Total sluice opening HVS (m2)")) +
  scale_color_manual(values = c("red", "blue")) +
  xlab("Time (Year - Month - Day)")+
  ylab("Delta waterheight (River - Sea (cm))") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Total sluice opening HVS (m2)"))+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%Y-%m-%d")+
  theme(axis.text.x = element_text(margin = margin(t = 10)))+
  theme(axis.text.y = element_text(margin = margin(t = 10)))+
  ggtitle("HVS total sluice height and delta waterheight (river-sea) over time in April 2023")

# HVS management July 2023: Delta_waterheight & Discharge over time
ggplot(data = HVS_management_Jul, aes(x = DateTime_UTC)) +
  geom_line(aes(y = Delta_Waterheight, color = "Delta_waterheight")) +
  geom_line(aes(y = Discharge_HVS_m3_s, color = "Discharge_HVS_m3_s")) +
  scale_color_manual(values = c("red", "blue")) +
  xlab("Time (Year - Month - Day)")+
  ylab("Delta waterheight (River - Sea (cm))") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Discharge (m3 / s)"))+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%Y-%m-%d")+
  theme(axis.text.x = element_text(margin = margin(t = 10)))+
  theme(axis.text.y = element_text(margin = margin(t = 10)))+
  ggtitle("HVS Discharge and delta waterheight (river-sea) over time in July 2023")

# HVS management July 2023: Delta_waterheight & Total sluice height (m2) over time
ggplot(data = HVS_management_Jul, aes(x = DateTime_UTC)) +
  geom_line(aes(y = Delta_Waterheight, color = "Delta waterheight")) +
  geom_line(aes(y = Total_sluice_opening_HVS_m2, color = "Total sluice opening HVS (m2)")) +
  scale_color_manual(values = c("red", "blue")) +
  xlab("Time (Year - Month - Day)")+
  ylab("Delta waterheight (River - Sea (cm))") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Total sluice opening HVS (m2)"))+
  scale_x_datetime(date_breaks = "2 days", date_labels = "%Y-%m-%d")+
  theme(axis.text.x = element_text(margin = margin(t = 10)))+
  theme(axis.text.y = element_text(margin = margin(t = 10)))+
  ggtitle("HVS total sluice height and delta waterheight (river-sea) over time in July 2023")


# 2.2. Analyse the relation between discharge Lobith and Sluice opening

# Plot Lobith discharge and total sluice opening m2 over time
ggplot(data = Lobith_sluice_opening, aes(x = DateTime_UTC)) +
  geom_line(aes(y = Discharge_Lobith_m3_s, color = "Discharge Lobith")) +
  geom_line(aes(y = Total_sluice_opening_HVS_m2, color = "Total sluice opening HVS")) +
  scale_color_manual(values = c("red", "blue")) +
  xlab("Time (Year - Month )")+
  ylab("Discharge Lobith (m3 / s)") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Total sluice opening HVS (m2)"))+
  scale_x_datetime(date_breaks = "3 months", date_labels = "%Y-%m")+
  theme(axis.text.x = element_text(margin = margin(t = 10)))+
  theme(axis.text.y = element_text(margin = margin(t = 10)))+
  ggtitle("Discharge Lobith and total sluice opening at HVS waterheight over time from Jan 2023 - Jan 2024")


## Plots 3.ANALYSIS OF 1 HOURLY ENV. VARIABLES OVER TIME

datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Enviro_combined_DE_1hour_HVS_HDC_HDB_SKC', 'Getij_Spui_Kier_1hour')
#datasets_to_keep_Plots_3 <- c('Enviro_combined_hours_average')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep_complete)
rm(list = clean_workspace)

#  NWW waterheight (cm) over time (Jan 2023 - Jan 2024)
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S") # Convert Hour column to POSIXct format to ensure proper x-axis labelling
ggplot(Enviro_combined_hours_average, aes(x = Hour, y = Avg_Waterheight_NWW_cm)) +
  geom_point(size=0.5) +
  labs(title = "Waterheight (averaged over hour) at Hoek van Holland (NWW) between Jan 2023 and Jan 2024",
       x = "Time (Month)",
       y = "Waterheight at Hoek van Holland (cm)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months")

#  Delta waterheight (River - Sea) (cm) HVS over time (Jan 2023 - Jan 2024)
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S") # Convert Hour column to POSIXct format to ensure proper x-axis labelling
ggplot(Enviro_combined_hours_average, aes(x = Hour, y = Avg_Delta_Waterheight_HVS_cm)) +
  geom_point(size=0.5) +
  labs(title = "Delta waterheight (averaged over hour) at Haringvlietsluizen between Jan 2023 and Jan 2024",
       x = "Time (Month)",
       y = "Delta waterheight (River - Sea) Haringvliet sluizen (cm)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months")

# HVS Waterheight sea (Stellendam buiten) over time (Jan 2023 - Jan 2024)
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S") # Convert Hour column to POSIXct format to ensure proper x-axis labelling
ggplot(Enviro_combined_hours_average, aes(x = Hour, y = Avg_Waterheight_Stellendam_btn_cm)) +
  geom_point(size=0.5) +
  labs(title = "Waterheight (averaged over hour) at Stellendam buiten, Haringvlietsluizen between Jan 2023 and Jan 2024",
       x = "Time (Month)",
       y = "Waterheight Stellendam buiten, Haringvliet sluizen (cm)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months")

# HVS Waterheight river (Hellevoetsluis) over time (Jan 2023 - Jan 2024)
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S") # Convert Hour column to POSIXct format to ensure proper x-axis labelling
ggplot(Enviro_combined_hours_average, aes(x = Hour, y = Avg_Waterheight_Hellevoetsluis_cm)) +
  geom_point(size=0.5) +
  labs(title = "Waterheight (averaged over hour) at Hellevoetsluis, Haringvlietsluizen between Jan 2023 and Jan 2024",
       x = "Time (Month)",
       y = "Waterheight Hellevoetsluis, Haringvliet sluizen (cm)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months")

# Sluice opening over time(Jan 2023 - Jan 2024)
Enviro_combined_hours_average$Hour <- as.POSIXct(Enviro_combined_hours_average$Hour, format = "%Y-%m-%d %H:%M:%S") # Convert Hour column to POSIXct format to ensure proper x-axis labelling
ggplot(Enviro_combined_hours_average, aes(x = Hour, y = Avg_sluice_opening_HVS_cm)) +
  geom_point(size=0.5) +
  labs(title = "Total sluice opening (cm, averaged over hour) at Haringvlietsluizen between Jan 2023 and Jan 2024",
       x = "Time (Month)",
       y = "Total sluice opening Haringvliet sluizen (cm)") +
  theme_minimal() +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months")


## Plots 4. CORRELATION BETWEEN ENV. VARIABLES USING GGPAIRS

datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Enviro_combined_DE_1hour_HVS_HDC_HDB_SKC', 'Getij_Spui_Kier_1hour')
#datasets_to_keep_Plots_4 <- c('Enviro_combined_hours_average_till_December2023')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep_complete)
rm(list = clean_workspace)

# Correlation plot 1. Sluice Management HVS: sluice_opening, Delta_Waterheight, Discharge. 
ggpairs1_HVS_inside_Sluice_Management <- c("Hour", "Avg_Discharge_HVS_m3_s", "Avg_Waterheight_Hellevoetsluis_cm", "Avg_sluice_opening_HVS_m2")
HVS_CorPlot_1_SluiceManagement <- na.omit(Enviro_combined_hours_average_till_December2023[ggpairs1_HVS_inside_Sluice_Management]) # 8130 / 8136 rows still there

#Rename column names in this specific plot and plot them in a collaborated correlation plot
colnames(HVS_CorPlot_1_SluiceManagement) <- c("Hour", "Discharge_(m3/s)", "Waterheight_(cm)", "Sluice_Opening_(m2)")
ggpairs(HVS_CorPlot_1_SluiceManagement, columns = c("Discharge_(m3/s)", "Waterheight_(cm)", "Sluice_Opening_(m2)"))+
  ggtitle("Correlation plot sluice management factors HVS")

# Correlation plot 2.1. HVS river: temperature and chloride ~ sluice opening & discharge
ggpairs2_Temp_Chlor_HVS_river <- c("Hour", "Avg_sluice_opening_HVS_m2", "Avg_WindSnelheid_HVS_m_s", "Avg_Chloride_stelldbnn_200cm" ,"Avg_Temp_stelldbnn_200cm")
HVS_river_CorPlot_2.1_TempChloride <- na.omit(Enviro_combined_hours_average_till_December2023[ggpairs2_Temp_Chlor_HVS_river]) # 8130 / 8136 rows still there

#Rename column names in this specific plot and plot them in a collaborated correlation plot
colnames(HVS_river_CorPlot_2.1_TempChloride) <- c("Hour", "Sluice_Opening_(m2)", "Windspeed_(m/s)", "Choride_200cm", "Temperature_200cm")
ggpairs(HVS_river_CorPlot_2.1_TempChloride, columns = c("Sluice_Opening_(m2)", "Windspeed_(m/s)", "Choride_200cm", "Temperature_200cm"))+
  ggtitle("Correlation plot chloride and temperature: HVS river")

# Correlation plot 2.2. HVS sea: temperature and chloride ~ sluice opening & discharge
ggpairs2_Temp_Chlor_HVS_sea <- c("Hour", "Avg_sluice_opening_HVS_m2", "Avg_WindSnelheid_HVS_m_s" , "Avg_Chloride_stelldbtn_200cm", "Avg_Temp_stelldbtn_200cm")
HVS_sea_CorPlot_2.2_TempChloride <- na.omit(Enviro_combined_hours_average_till_December2023[ggpairs2_Temp_Chlor_HVS_sea]) # 8130 / 8136 rows still there

#Rename column names in this specific plot and plot them in a collaborated correlation plot
colnames(HVS_sea_CorPlot_2.2_TempChloride) <- c("Hour", "Sluice_Opening_(m2)", "Windspeed_(m/s)", "Choride_200cm", "Temperature_200cm")
ggpairs(HVS_sea_CorPlot_2.2_TempChloride, columns = c("Sluice_Opening_(m2)", "Windspeed_(m/s)", "Choride_200cm", "Temperature_200cm"))+
  ggtitle("Correlation plot chloride and temperature: HVS sea")

# Correlation plot 2.3. NWW: temperature and chloride ~ sluice opening & discharge
ggpairs2_Temp_NWW <- c("Hour", "Avg_Discharge_NWW_m3_s" , "Avg_WindSnelheid_HoekvanHolland_m_s", "Temp_C_NW3")
NWW_CorPlot_2.3_TempChloride <- na.omit(Enviro_combined_hours_average_till_December2023[ggpairs2_Temp_NWW]) 

#Rename column names in this specific plot and plot them in a collaborated correlation plot
NWW_CorPlot_2.3_TempChloride$Temp_C_NW3 <- as.numeric(NWW_CorPlot_2.3_TempChloride$Temp_C_NW3)
colnames(NWW_CorPlot_2.3_TempChloride) <- c("Hour", "Discharge_(m3/s)", "Windspeed_(m/s)", "Temperature_NW3")
ggpairs(NWW_CorPlot_2.3_TempChloride, columns = c("Discharge_(m3/s)", "Windspeed_(m/s)", "Temperature_NW3"))+
  ggtitle("Correlation plot temperature: NWW")

# Correlation plot 3.1. HVS river: tilt and noise ~ sluice opening & windspeed

# Part of tilt and noise data provided in character format. Need special attention. Formatting is done below.
# First check the data
str(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HDC) # tilt data in character format
str(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HDC) # noise data in character format
str(Enviro_combined_hours_average_till_December2023$Temp_C_HDC) # HDC temperature data in character format

# Try to convert this character format into numeric, try more explicitely
Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HDC <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HDC))
Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HDC <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HDC))
Enviro_combined_hours_average_till_December2023$Temp_C_HDC <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Temp_C_HDC))

# NA values were introduced by coercion, which was inevitable. Check how many and where they were introduced
# Tilt
NA_count_HVS_river_Tilt <- sum(is.na(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HDC))
print(NA_count_HVS_river_Tilt) #339
NA_identification_HVS_river_Tilt <- which(is.na(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HDC))
print(NA_identification_HVS_river_Tilt) # 

# Noise
NA_count_HVS_river_Noise <- sum(is.na(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HDC))
print(NA_count_HVS_river_Noise) #2
NA_identification_HVS_river_Noise <- which(is.na(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HDC))
print(NA_identification_HVS_river_Noise) # 1647 , 6159. 

# Temperature HDC
NA_count_HVS_river_Temp_HDC <- sum(is.na(Enviro_combined_hours_average_till_December2023$Temp_C_HDC))
print(NA_count_HVS_river_Temp_HDC) # 339
NA_identification_HVS_river_Temp_HDC <- which(is.na(Enviro_combined_hours_average_till_December2023$Temp_C_HDC))
print(NA_identification_HVS_river_Temp_HDC) # Exactly same rows as seen for HDC tilt 

# Considering that these character values occured in the same rows for each variable, remove these rows.(339 / 8146)*100 = 4.2% 7797 rows remaining
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC <- Enviro_combined_hours_average_till_December2023[complete.cases(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HDC, 
                                                                                                                                      Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HDC, Enviro_combined_hours_average_till_December2023$Temp_C_HDC), ]


# Calculate Delta tilt (180- the value tilt is now) value now that all the undesirable rows were removed                                                                                                                       
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC <- Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC %>%
  mutate(Tilt_angle_degree_HDC_delta = 180 - Tilt_angle_degree_HDC)

# Plot tilt and noise against windspeed and discharge at HVS river
ggpairs3_HVS_river_Tilt_Noise <- c("Hour", "Avg_sluice_opening_HVS_m2", "Avg_WindSnelheid_HVS_m_s" , "Tilt_angle_degree_HDC_delta","Avg_noise_mV_HDC")
HVS_river_CorPlot_3.1_TiltNoise <- na.omit(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC[ggpairs3_HVS_river_Tilt_Noise]) # 7676 / 7797 rows still there

#Rename column names in this specific plot and plot them in a collaborated correlation plot
colnames(HVS_river_CorPlot_3.1_TiltNoise) <- c("Hour", "Sluice_Opening(m2)", "Windspeed_(m/s)", "Tilt_HDC_(Degr)", "Noise_HDC_(mV)")
ggpairs(HVS_river_CorPlot_3.1_TiltNoise, columns = c("Sluice_Opening(m2)", "Windspeed_(m/s)", "Tilt_HDC_(Degr)", "Noise_HDC_(mV)"))+
  ggtitle("Correlation plot tilt and noise factors HVS river")

# Correlation plot 3.2. HVS sea: tilt and noise ~ sluice opening & windspeed

# Check the format of the data again like for HD-C
str(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HS8) # tilt data in character format
str(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS8) # noise data in character format
str(Enviro_combined_hours_average_till_December2023$Temp_C_HS8) # HDC temperature data in character format

# Try to convert this character format into numeric, try more explicitely
Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HS8 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HS8))
Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS8 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS8))
Enviro_combined_hours_average_till_December2023$Temp_C_HS8 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Temp_C_HS8))

# Check if NA values were introduced 
# Tilt
NA_count_HVS_sea_Tilt <- sum(is.na(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HS8))
print(NA_count_HVS_sea_Tilt) #0

# Noise
NA_count_HVS_sea_Noise <- sum(is.na(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS8))
print(NA_count_HVS_sea_Noise) #0

# Temperature HDC
NA_count_HVS_sea_Temp_HS8 <- sum(is.na(Enviro_combined_hours_average_till_December2023$Temp_C_HS8))
print(NA_count_HVS_sea_Temp_HS8) # 0

# No NA values introduced, check if data is now in correct numerical format
str(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_HS8) # tilt data in numeric format
str(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS8) # noise data in numeric format
str(Enviro_combined_hours_average_till_December2023$Temp_C_HS8) # Temperature data in numeric format

# Calculate Delta tilt (180- the value tilt is now) value now that all the undesirable rows were removed                                                                                                                       
Enviro_combined_hours_average_till_December2023 <- Enviro_combined_hours_average_till_December2023 %>%
  mutate(Tilt_angle_degree_HS8_delta = 180 - Tilt_angle_degree_HS8)

# Plot tilt and noise against windspeed and discharge at HVS sea
ggpairs3_HVS_sea_Tilt_Noise <- c("Hour", "Avg_sluice_opening_HVS_m2", "Avg_WindSnelheid_HVS_m_s" , "Tilt_angle_degree_HS8_delta", "Avg_noise_mV_HS8")
HVS_sea_CorPlot_3.2_TiltNoise <- na.omit(Enviro_combined_hours_average_till_December2023[ggpairs3_HVS_sea_Tilt_Noise]) 

#Rename column names in this specific plot and plot them in a collaborated correlation plot
colnames(HVS_sea_CorPlot_3.2_TiltNoise) <- c("Hour", "Sluice_Opening(m2)", "Windspeed_(m/s)", "Tilt_HS8_(Degr)", "Noise_HS8_(mV)")
ggpairs(HVS_sea_CorPlot_3.2_TiltNoise, columns = c("Sluice_Opening(m2)", "Windspeed_(m/s)", "Tilt_HS8_(Degr)", "Noise_HS8_(mV)"))+
  ggtitle("Correlation plot tilt and noise factors HVS sea")

# Correlation plot 3.3. NWW: tilt and noise ~ sluice discharge & windspeed
str(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_NW3) # tilt data in character format
str(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_NW3) # noise data in character format
str(Enviro_combined_hours_average_till_December2023$Temp_C_NW3) # Temperature data in character format

# Try to convert this character format into numeric, try more explicitely
Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_NW3 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_NW3))
Enviro_combined_hours_average_till_December2023$Avg_noise_mV_NW3 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_NW3))
Enviro_combined_hours_average_till_December2023$Temp_C_NW3 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Temp_C_NW3))

# No NA values were introduced by coercion, you can see that below
# Tilt
NA_count_NWW_Tilt <- sum(is.na(Enviro_combined_hours_average_till_December2023$Tilt_angle_degree_NW3))
print(NA_count_NWW_Tilt) # 0

# Noise
NA_count_NWW_Noise <- sum(is.na(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_NW3))
print(NA_count_NWW_Noise) # 0

# Temperature NW2
NA_count_NWW_Temp_NW3 <- sum(is.na(Enviro_combined_hours_average_till_December2023$Temp_C_NW3))
print(NA_count_NWW_Temp_NW3) # 0

# Tilt and noise are in numeric format without NA values being introduced. Plot tilt and noise against windspeed and discharge at HVS sea

# Calculate Delta tilt (180- the value tilt is now) value now that all the undesirable rows were removed                                                                                                                       
Enviro_combined_hours_average_till_December2023 <- Enviro_combined_hours_average_till_December2023 %>%
  mutate(Tilt_angle_degree_NW3_delta = 180 - Tilt_angle_degree_NW3)

# First select columns of interest and remove all NA values introduced by coercion
ggpairs3_NWW_Tilt_Noise <- c("Hour", "Avg_Discharge_NWW_m3_s", "Avg_WindSnelheid_HoekvanHolland_m_s" , "Tilt_angle_degree_NW3_delta", "Avg_noise_mV_NW3")
NWW_CorPlot_3.3_TiltNoise <- na.omit(Enviro_combined_hours_average_till_December2023[ggpairs3_NWW_Tilt_Noise]) 

#Rename column names in this specific plot and plot them in a collaborated correlation plot
colnames(NWW_CorPlot_3.3_TiltNoise) <- c("Hour", "Discharge(m3/s)", "Windspeed_(m/s)","Tilt_NW3_(Degr)", "Noise_NW3_(mV)")
ggpairs(NWW_CorPlot_3.3_TiltNoise, columns = c("Discharge(m3/s)", "Windspeed_(m/s)", "Tilt_NW3_(Degr)", "Noise_NW3_(mV)"))+
  ggtitle("Correlation plot tilt and noise factors NWW")


## Plots 5. ANALYSIS OF SPECIFIC INTERACTIONS WITHIN THE RECEIVER INFORMATION DATA 

datasets_to_keep_complete <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW','Discharge_HVS', 'Discharge_relevant', 'HVS_management_Apr', 'HVS_management_Jul', 'HVS_management_Dec', 'Lobith_sluice_opening', 'Enviro_combined_hours_average', 'Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023' , 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023' , 'Enviro_combined_DE_1hour_HVS_HDC_HDD', 'Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec', 'Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Apr2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Jul2023', 'Enviro_combined_DE_1hour_HVS_HDC_HDD_Dec2023', 'Spui_Kier_info_Apr2023', 'Spui_Kier_info_Jul2023', 'Spui_Kier_info_Dec2023', 'Getij_Spui_Kier_data_UTC_total', 'Enviro_combined_DE_1hour_HVS_HDC_HDB_SKC', 'Getij_Spui_Kier_1hour')
#datasets_to_keep_Plots_5 <- c('Enviro_combined_hours_average_till_December2023', 'Enviro_combined_hours_average_till_December2023_Apr2023', 'Enviro_combined_hours_average_till_December2023_Jul2023', 'Enviro_combined_hours_average_till_December2023_Dec2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023', 'Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_to_keep_complete)
rm(list = clean_workspace)

# YOU COULD ALSO USE THIS FILE: Enviro_combined_hours_average INSTEAD OF THE DECEMBER ONE?

# 5.1. Compare noise between HVS river, HVS sea and NWW

# Clarify the time zone of the Hour data again & put noise values to numeric to ensure adequate plotting
Enviro_combined_hours_average_till_December2023$Hour <- as.POSIXct(Enviro_combined_hours_average_till_December2023$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC$Hour <- as.POSIXct(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC$Avg_noise_mV_HDC <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC$Avg_noise_mV_HDC))
Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS6 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_HS6))
Enviro_combined_hours_average_till_December2023$Avg_noise_mV_NW2 <- as.numeric(as.character(Enviro_combined_hours_average_till_December2023$Avg_noise_mV_NW2))

# Set minimum and maximum time points to create relevant 2-week time periods 
Time_period_April_min <- as.POSIXct("2023-04-10 09:00:00", tz = "UTC") # April 2023
Time_period_April_max <- as.POSIXct("2023-04-24 09:00:00", tz = "UTC") 
Time_period_July_min <- as.POSIXct("2023-07-10 09:00:00", tz = "UTC") # July 2023
Time_period_July_max <- as.POSIXct("2023-07-24 09:00:00", tz = "UTC") 
Time_period_December_min <- as.POSIXct("2023-12-10 09:00:00", tz = "UTC") # December 2023
Time_period_December_max <- as.POSIXct("2023-12-24 09:00:00", tz = "UTC") 

# Create subsets of the data set showing the relevant two-week periods in April, July and December 2023

# HS6 and NW2 as relevant receivers
Enviro_combined_hours_average_till_December2023_Apr2023 <- subset(Enviro_combined_hours_average_till_December2023,  Hour > Time_period_April_min & Hour < Time_period_April_max) 
Enviro_combined_hours_average_till_December2023_Jul2023 <- subset(Enviro_combined_hours_average_till_December2023, Hour > Time_period_July_min  & Hour < Time_period_July_max) 
Enviro_combined_hours_average_till_December2023_Dec2023 <- subset(Enviro_combined_hours_average_till_December2023, Hour > Time_period_December_min & Hour < Time_period_December_max)

# HDC as relevant receiver. This is done seperately because some rows were removed because of NA introduced by coercion only for the HDC values
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023 <- subset(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC, Hour > Time_period_April_min & Hour < Time_period_April_max)
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Jul2023 <- subset(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC, Hour > Time_period_July_min & Hour < Time_period_July_max)
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Dec2023 <-subset(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC, Hour > Time_period_December_min & Hour < Time_period_December_max)

# Put values in Hour columns of the newly created datasets in correct DateTime format
Enviro_combined_hours_average_till_December2023_Apr2023$Hour <- as.POSIXct(Enviro_combined_hours_average_till_December2023_Apr2023$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023$Hour <- as.POSIXct(Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023$Hour, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# First plot noise of HVS river, HVS sea and NWW for each relevant 2-week time period

# April 2023. 
# Retrieved from the HVS graph backup dataset. TRY-OUT
ggplot() +
  geom_line(data = Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023, aes(x = Hour, y = Avg_noise_mV_HDC), color = "blue") +
  geom_line(data = Enviro_combined_hours_average_till_December2023_Apr2023, aes(x = Hour, y = Avg_noise_mV_HS6), color = "red") +
  geom_line(data = Enviro_combined_hours_average_till_December2023_Apr2023, aes(x = Hour, y = Avg_noise_mV_NW2), color = "green") +
  geom_label(data = Enviro_combined_hours_average_till_December2023_Tilt_Noise_Temp_HDC_Apr2023, aes(x = max(Hour), y = 750, label = "Haringvliet HD-C"), 
             fill = "white", color = "blue", hjust = 1, vjust = 0.5, size = 4, label.r = unit(0.5, "lines"))+ 
  geom_label(data = Enviro_combined_hours_average_till_December2023_Apr2023, aes(x = max(Hour), y = 650, label = "Slijkgat HS6"), 
             fill = "white", color = "red", hjust = 1, vjust = 0.5, size = 4, label.r = unit(0.5, "lines"))+ 
  geom_label(data = Enviro_combined_hours_average_till_December2023_Apr2023, aes(x = max(Hour), y = 550, label = "NieuweWaterweg NW2"), 
             fill = "white", color = "green", hjust = 1, vjust = 0.5, size = 4, label.r = unit(0.5, "lines"))+ 
  labs(title = "Noise measured at receiver HDC, HS6 and NW2 in April 2023",
       x = "Time",
       y = "Average noise (Per hour, mV)") +
  ylim(0, 800) + 
  theme_minimal() +
  theme(plot.title = element_textbox_simple(face = "bold", color = "black", size = 16, fill = "white", box.color = "black"))


# 5.2. Make box plots of tilt and noise in which HVS river, HVS sea and NWW are compared
str(Enviro_combined_DE_1hour_HVS_HDC_HDD$Tilt_angle_degree_HDC_delta) # numeric
str(Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec$Tilt_angle_degree_HS8_delta) # numeric
str(Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec$Tilt_angle_degree_NW3_delta) # numeric

ggplot(Enviro_combined_DE_1hour_HVS_HDC_HDD, aes(x = "", y = Tilt_angle_degree_HDC_delta)) +
  geom_boxplot() +  
  labs(x = "", y = "Tilt angle (degrees)") +  
  ggtitle("Boxplot of HD-C Tilt Angle")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# In order to get the tilt and noise of the three locations in one boxplot, we have to create a column to indicate the location 
Enviro_combined_DE_1hour_HVS_HDC_HDD <- Enviro_combined_DE_1hour_HVS_HDC_HDD %>%
  mutate(Location = "HDC")
Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec <- Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec %>%
  mutate(Location = "HS8")
Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec <- Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec %>%
  mutate(Location = "NW3")

# Create a dataframe that contains all the tilts with their source
Tilt_boxplot_HVS_NWW_combined <- bind_rows(
  Enviro_combined_DE_1hour_HVS_HDC_HDD %>% select(Tilt_angle_degree_HDC_delta, Location),
  Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec %>% select(Tilt_angle_degree_HS8_delta, Location),
  Enviro_combined_DE_1hour_NWW_NW2Trans_NW3Rec %>% select(Tilt_angle_degree_NW3_delta, Location)
)

# Plot the boxplot comparing tilt between HVS river, HVS sea and NWW
ggplot(Tilt_boxplot_HVS_NWW_combined, aes(x = Location)) +
  geom_boxplot(aes(y = Tilt_angle_degree_HDC_delta), fill = "blue", alpha = 0.5) +
  geom_boxplot(aes(y = Tilt_angle_degree_HS8_delta), fill = "red", alpha = 0.5) +
  geom_boxplot(aes(y = Tilt_angle_degree_NW3_delta), fill = "green", alpha = 0.5) +
  labs(x = "Location", y = "Tilt Angle (degrees)") +
  ggtitle("Boxplot comparing the tilt angle between HVS river, HVS sea and NWW ")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 5.3. Compare battery lifetime between all mentioned receivers

# 5.4. Compare Temperature RWS with temperature Receiver
Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec$Avg_Temp_stelldbtn_200cm <- as.numeric((Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec$Avg_Temp_stelldbtn_200cm))
Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec$Temp_C_HS8 <- as.numeric((Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec$Temp_C_HS8))
ggplot(Enviro_combined_DE_1hour_HVS_HS6Trans_HS8Rec, aes(x = Avg_Temp_stelldbtn_200cm, y = Temp_C_HS8)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE) +  
  stat_cor(method = "pearson", label.x = 25, label.y = 25, size = 5) +
  labs(x = "HVS temperature at 2m depth (Celcius)", y = "Temperature measured at HS8 (Celcius)") +  
  ggtitle("The temperature measured at HS8 over the average hourly temperature at 200m of HVS sea")+
  scale_x_continuous(limits = c(0, 30))+
  scale_y_continuous(limits = c(0, 30))



