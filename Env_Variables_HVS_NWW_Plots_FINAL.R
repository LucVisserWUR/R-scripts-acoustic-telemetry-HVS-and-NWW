title: "Environmental factor ~ DE HVS and NWW DATA EXPLORATION FINAL" 
Author: "Luc Visser"
date: "2024-28-05"

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
library(car) # To check homogeneity of variances using Levene's test
library(TukeyC)
library(grid) # Add titles to collective grid plots
library(gridExtra) # Plot multiple graphs in one. Used in Chapter 12 to merge multiple bargraphs of Spui / Kier / Closed together
library(mgcv) # GAM analysis, chapter 7
library(multcomp) # To compare averages of boxplots & get compact letter display (cld) 
library(multcompView) 
library(emmeans) # To compare averages of boxplots & get compact letter display (cld) 

# Chapters 
# 1. Check fundamental calculations within the date
# 2. Check the distribution of environmental variables
# 3. Explore the relations between Env. variables and DE 
# Create only 0 models (because of 0 deflated models) boxplots
# Create boxplots for the set amount of expected n for the different environmental variables
# 4. Validate DE between &  within areas to judge if you can simply seperate areas
# 5. Validate DE over the different months in the year to see the temporal effect 
# 6. Statistical analysis (BUT THIS INFORMATION IS NOT USED)

# 3. EXPLORE THE RELATIONS BETWEEN ENV. VARIABLES AND DE 

# Clean workspace
datasets_data_exploration <- c('one_hourly_observed_expected_detections_HVS', 'one_hourly_observed_expected_detections_NWW', 'Getij_Spui_Kier_data_UTC_total', 'Getij_Spui_Kier_1hour', 'Env_variables_total', 'DE_HVS_total','DE_NWW_total', 'Env_DE_HVS_total', 'Env_DE_NWW_total', 'Env_DE_HDC_HDB', 'Env_DE_HDC_HDD', 'Env_DE_HDC_HDE', 'Env_DE_HS6_HS8', 'Env_DE_NW2_NW3',  'Env_DE_HVS_NWW_total')
objects_in_workspace <- ls()
clean_workspace <- setdiff(objects_in_workspace, datasets_data_exploration)
rm(list = clean_workspace)

# First introduce the column for month for both the HVS and NWW dataset. 

# HVS
Env_DE_HVS_total$DateTime_UTC_YearMonth <- format(Env_DE_HVS_total$DateTime_UTC, "%m/%Y")
Env_DE_HVS_total <- Env_DE_HVS_total %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# NWW
Env_DE_NWW_total$DateTime_UTC_YearMonth <- format(Env_DE_NWW_total$DateTime_UTC, "%m/%Y")
Env_DE_NWW_total <- Env_DE_NWW_total %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# HDC - HDB
Env_DE_HDC_HDB$DateTime_UTC_YearMonth <- format(Env_DE_HDC_HDB$DateTime_UTC, "%m/%Y")
Env_DE_HDC_HDB <- Env_DE_HDC_HDB %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# HDC - HDD 
Env_DE_HDC_HDD$DateTime_UTC_YearMonth <- format(Env_DE_HDC_HDD$DateTime_UTC, "%m/%Y")
Env_DE_HDC_HDD <- Env_DE_HDC_HDD %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# HDC - HDE
Env_DE_HDC_HDE$DateTime_UTC_YearMonth <- format(Env_DE_HDC_HDE$DateTime_UTC, "%m/%Y")
Env_DE_HDC_HDE <- Env_DE_HDC_HDE %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# HS-6 - HS-8
Env_DE_HS6_HS8$DateTime_UTC_YearMonth <- format(Env_DE_HS6_HS8$DateTime_UTC, "%m/%Y")
Env_DE_HS6_HS8 <- Env_DE_HS6_HS8 %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# NW2 - NW3
Env_DE_NW2_NW3$DateTime_UTC_YearMonth <- format(Env_DE_NW2_NW3$DateTime_UTC, "%m/%Y")
Env_DE_NW2_NW3 <- Env_DE_NW2_NW3 %>%
  dplyr::select(DateTime_UTC_YearMonth, everything())

# GENERAL ANALYSIS

# Occurence of DE = 0
hist(Env_DE_HDC_HDB$DE) 
Count_DE_0_HDC_HDB <- sum(Env_DE_HDC_HDB$DE == 0) # 1541 0 detections / 7006 
hist(Env_DE_HDC_HDD$DE)
Count_DE_0_HDC_HDD <- sum(Env_DE_HDC_HDD$DE == 0) # 5169 0 detections / 7006 
hist(Env_DE_HDC_HDE$DE)
Count_DE_0_HDC_HDD <- sum(Env_DE_HDC_HDE$DE == 0) # 3726 0 detections / 7006 
hist(Env_DE_HS6_HS8$DE)
Count_DE_0_HS6_HS8 <- sum(Env_DE_HS6_HS8$DE == 0) # 1318 0 detections / 8722
hist(Env_DE_NW2_NW3$DE)
Count_DE_0_NW2_NW3 <- sum(Env_DE_NW2_NW3$DE == 0) # 1300 0 detections / 8722

# Occurence of different values for expected_n
hist(Env_DE_HDC_HDB$expected_n) 
Count_exp_6_HDC_HDB <- sum(Env_DE_HDC_HDB$expected_n == 6) # 5789 times expected n = 6 / 7006  
hist(Env_DE_HDC_HDD$expected_n)
Count_exp_6_HDC_HDD <- sum(Env_DE_HDC_HDD$expected_n == 6) # 5789 times expected n = 6 / 7006 
hist(Env_DE_HS6_HS8$expected_n)
Count_exp_6_HS6_HS8 <- sum(Env_DE_HS6_HS8$expected_n == 6) # 7206 times expected n = 6 / 8722
hist(Env_DE_NW2_NW3$expected_n)
Count_exp_6_NW2_NW3 <- sum(Env_DE_NW2_NW3$expected_n == 6) # 7155 times expected n = 6 / 8722

# Check if n_expected != 6 values are distributed equally throughout each month. NOT WORKING YET!
Env_DE_HDC_HDB$expected_n <- as.factor(Env_DE_HDC_HDB$expected_n)
ggplot(data = subset(Env_DE_HDC_HDB, expected_n != 6), aes(x = factor(DateTime_UTC_YearMonth), y = frequency(expected_n) , fill = factor(expected_n))) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(x = "Month (2023)", y = "Frequency", fill = "expected_n") +
  scale_fill_discrete(name = "Exoected n") +
  theme_minimal()+
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 4)) 

# CREATE ONE COLLECTIVE DATAFRAME THAT CONTAINS DE ~ ENV. VARIABLE DATA FOR EVERY CONSIDERED TRANS - REC COMBINATION

# Insert ID column to later be able to merch dataset together
Env_DE_HDC_HDB$ID <- "HDC_HDB"
Env_DE_HDC_HDB <- Env_DE_HDC_HDB %>%
  dplyr::select(ID, everything())
Env_DE_HDC_HDD$ID <- "HDC_HDD"
Env_DE_HDC_HDD <- Env_DE_HDC_HDD %>%
  dplyr::select(ID, everything())
Env_DE_HDC_HDE$ID <- "HDC_HDE"
Env_DE_HDC_HDE <- Env_DE_HDC_HDE %>%
  dplyr::select(ID, everything())
Env_DE_HS6_HS8$ID <- "HS6_HS8"
Env_DE_HS6_HS8 <- Env_DE_HS6_HS8 %>%
  dplyr::select(ID, everything())
Env_DE_NW2_NW3$ID <- "NW2_NW3"
Env_DE_NW2_NW3 <- Env_DE_NW2_NW3 %>%
  dplyr::select(ID, everything())

# Select only 6 expected transmitted signals for the sync transmitter
Env_DE_HDC_HDB_exp6 <- subset(Env_DE_HDC_HDB,expected_n == 6) 
Env_DE_HDC_HDD_exp6 <- subset(Env_DE_HDC_HDD,expected_n == 6) 
Env_DE_HDC_HDE_exp6 <- subset(Env_DE_HDC_HDE,expected_n == 6) 
Env_DE_HS6_HS8_exp6 <- subset(Env_DE_HS6_HS8,expected_n == 6) 
Env_DE_NW2_NW3_exp6 <- subset(Env_DE_NW2_NW3,expected_n == 6) 

# First remove Env. variable columns you don't need
Env_DE_HDC_HDB_exp6 <- Env_DE_HDC_HDB_exp6 %>%
  dplyr::select(-c(Water_Height_HVSs, Temp_HVSs, Chloride_HVSs))
Env_DE_HDC_HDD_exp6 <- Env_DE_HDC_HDD_exp6 %>%
  dplyr::select(-c(Water_Height_HVSs, Temp_HVSs, Chloride_HVSs))
Env_DE_HDC_HDE_exp6 <- Env_DE_HDC_HDE_exp6 %>%
  dplyr::select(-c(Water_Height_HVSs, Temp_HVSs, Chloride_HVSs))
Env_DE_HS6_HS8_exp6 <- Env_DE_HS6_HS8_exp6 %>%
  dplyr::select(-c(Water_Height_HVSr, Temp_HVSr, Chloride_HVSr))

# Now rename column names to create general names for env. variables that allow merging
Env_DE_HDC_HDB_exp6 <- Env_DE_HDC_HDB_exp6 %>% 
  dplyr::rename(Discharge =  Discharge_HVS, Waterheight = Water_Height_HVSr, Windspeed = Wind_Speed_HVS, Wind_Direction = Wind_Direction_HVS, Temp = Temp_HVSr, Chloride = Chloride_HVSr, Battery = Battery_HDC, Temp_rec = Temp_HDC, Noise = Noise_HDC, Tilt = Tilt_HDC)
Env_DE_HDC_HDD_exp6 <- Env_DE_HDC_HDD_exp6 %>% 
  dplyr::rename(Discharge =  Discharge_HVS, Waterheight = Water_Height_HVSr, Windspeed = Wind_Speed_HVS, Wind_Direction = Wind_Direction_HVS, Temp = Temp_HVSr, Chloride = Chloride_HVSr, Battery = Battery_HDC, Temp_rec = Temp_HDC, Noise = Noise_HDC, Tilt = Tilt_HDC)
Env_DE_HDC_HDE_exp6 <- Env_DE_HDC_HDE_exp6 %>% 
  dplyr::rename(Discharge =  Discharge_HVS, Waterheight = Water_Height_HVSr, Windspeed = Wind_Speed_HVS, Wind_Direction = Wind_Direction_HVS, Temp = Temp_HVSr, Chloride = Chloride_HVSr, Battery = Battery_HDC, Temp_rec = Temp_HDC, Noise = Noise_HDC, Tilt = Tilt_HDC)
Env_DE_HS6_HS8_exp6 <- Env_DE_HS6_HS8_exp6 %>% 
  dplyr::rename(Discharge =  Discharge_HVS, Waterheight = Water_Height_HVSs, Windspeed = Wind_Speed_HVS, Wind_Direction = Wind_Direction_HVS, Temp = Temp_HVSs, Chloride = Chloride_HVSs, Battery = Battery_HS8, Temp_rec = Temp_HS8, Noise = Noise_HS8, Tilt = Tilt_HS8)
Env_DE_NW2_NW3_exp6 <- Env_DE_NW2_NW3_exp6 %>% 
  dplyr::rename(Discharge =  Discharge_NWW, Waterheight = Water_Height_NWW, Windspeed = Wind_Speed_NWW, Wind_Direction = Wind_Direction_NWW , Battery = Battery_NW3, Temp_rec = Temp_NW3, Noise = Noise_NW3, Tilt = Tilt_NW3)

# Place columns that are not available for the NWW area in the back of the data frame to ensure proper merging
Env_DE_HDC_HDB_exp6 <- Env_DE_HDC_HDB_exp6 %>% # HDC - HDB
  dplyr::select(-c(Temp, Chloride, Sluice_Opening, Spui_Kier_Closed), everything(), Temp, Chloride, Sluice_Opening, Spui_Kier_Closed)
Env_DE_HDC_HDD_exp6 <- Env_DE_HDC_HDD_exp6 %>% # HDC - HDD
  dplyr::select(-c(Temp, Chloride, Sluice_Opening, Spui_Kier_Closed), everything(), Temp, Chloride, Sluice_Opening, Spui_Kier_Closed)
Env_DE_HDC_HDE_exp6 <- Env_DE_HDC_HDE_exp6 %>% # HDC - HDE
  dplyr::select(-c(Temp, Chloride, Sluice_Opening, Spui_Kier_Closed), everything(), Temp, Chloride, Sluice_Opening, Spui_Kier_Closed)
Env_DE_HS6_HS8_exp6 <- Env_DE_HS6_HS8_exp6 %>% # HS6 - HS8
  dplyr::select(-c(Temp, Chloride, Sluice_Opening, Spui_Kier_Closed), everything(), Temp, Chloride, Sluice_Opening, Spui_Kier_Closed)

# Create these columns for the NWW dataframes as well and fill them with NA values 
Env_DE_NW2_NW3_exp6$Sluice_Opening <- NA
Env_DE_NW2_NW3_exp6$Spui_Kier_Closed <- NA
Env_DE_NW2_NW3_exp6$Temp <- NA
Env_DE_NW2_NW3_exp6$Chloride <- NA

# Now merge all datasets together. r bins arranged for Date/Time 
Env_DE_HVS_NWW_total <- rbind(Env_DE_HDC_HDB_exp6, Env_DE_HDC_HDD_exp6) %>% arrange(DateTime_UTC)
Env_DE_HVS_NWW_total <- rbind(Env_DE_HVS_NWW_total, Env_DE_HDC_HDE_exp6) %>% arrange(DateTime_UTC)
Env_DE_HVS_NWW_total <- rbind(Env_DE_HVS_NWW_total, Env_DE_HS6_HS8_exp6) %>% arrange(DateTime_UTC)
Env_DE_HVS_NWW_total <- rbind(Env_DE_HVS_NWW_total, Env_DE_NW2_NW3_exp6) %>% arrange(DateTime_UTC)

# Put all the numeric variables into correct format
Env_DE_HVS_NWW_total$Discharge < as.numeric(Env_DE_HVS_NWW_total$Discharge)
Env_DE_HVS_NWW_total$Waterheight <-  as.numeric(Env_DE_HVS_NWW_total$Waterheight)
Env_DE_HVS_NWW_total$Windspeed <-  as.numeric(Env_DE_HVS_NWW_total$Windspeed)
Env_DE_HVS_NWW_total$Temp <-  as.numeric(Env_DE_HVS_NWW_total$Temp)
Env_DE_HVS_NWW_total$Chloride <-  as.numeric(Env_DE_HVS_NWW_total$Chloride)
Env_DE_HVS_NWW_total$Battery <- as.numeric(as.character(Env_DE_HVS_NWW_total$Battery))
Env_DE_HVS_NWW_total$Temp_rec <- as.numeric(as.character(Env_DE_HVS_NWW_total$Temp_rec))
Env_DE_HVS_NWW_total$Noise <- as.numeric(as.character(Env_DE_HVS_NWW_total$Noise))
Env_DE_HVS_NWW_total$Tilt <- as.numeric(as.character(Env_DE_HVS_NWW_total$Tilt))

# Select only data until December 31 2023: Discharge data is only available up untill that date, and waterheigh & receiver info are also unavailable during large periods of January 2024
Env_DE_HVS_NWW_total <- Env_DE_HVS_NWW_total %>% filter(DateTime_UTC_YearMonth != "01/2024")

# Create csv and excel file of the data.frame just created
Total_Env_DE_HVS_NWW_DF <- as.data.frame(Env_DE_HVS_NWW_total)
#write.xlsx(Total_Env_DE_HVS_NWW_DF, "Total_Env_DE_HVS_NWW_DF.xlsx", rowNames = FALSE)
#write.csv(Total_Env_DE_HVS_NWW_DF, "Total_Env_DE_HVS_NWW_DF.csv", rowNames = FALSE)

# FIRST MAKE DATASET TO COMPARE ONLY DE = 0 VALUES WITH EACHOTHER

# Create bargraph over months for each time DE was 0
Env_DE_HVS_NWW_total_DE_0 <- Env_DE_HVS_NWW_total %>% filter(DE == 0) # Select only DE with value = 0 
Env_DE_HVS_NWW_total_DE_0 <- Env_DE_HVS_NWW_total_DE_0 %>% # Calculate the frequency of those values occurring per month
  group_by(DateTime_UTC_YearMonth, ID) %>% 
  dplyr::summarise(DE_count = n())
Env_DE_HVS_NWW_total_DE_0 <- Env_DE_HVS_NWW_total_DE_0 %>% filter(DateTime_UTC_YearMonth != "01/2024") # Remove Jan 2024. Not much data for any of the receivers in that month

# Visualize in barplot
ggplot(data = subset(Env_DE_HVS_NWW_total_DE_0[!(Env_DE_HVS_NWW_total_DE_0$ID %in% c("HDC_HDB", "HDC_HDD")), ]), aes(x = factor(DateTime_UTC_YearMonth), y = DE_count, fill = factor(ID))) +
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  labs(x = "Month (2023)", y = "DE = 0  Count (number)", fill = "ID") +
  scale_fill_discrete(name = "ID") +
  theme_minimal()+
  ggtitle("Number of 0 hourly detections per month for transmitter-receiver combinations of the HVSriver, HVSsea and NWW areas")

# THE PROBABILITY ON DE = 0 VALUES FOR DIFFERENT ENVIRONMENTAL VARIABLES
Env_DE_HVS_NWW_total_DE_0 <- subset(Env_DE_HVS_NWW_total, DE == 0)
Env_DE_HVS_NWW_total_DE_0_Apr <- subset(Env_DE_HVS_NWW_total_DE_0, DateTime_UTC_YearMonth == "04/2023")
Env_DE_HVS_NWW_total_DE_0_Apr <- Env_DE_HVS_NWW_total_DE_0_Apr[complete.cases(Env_DE_HVS_NWW_total_DE_0_Apr$Discharge), ] # Remove NA values for discharge 
Env_DE_HVS_NWW_total_DE_0_Apr <- Env_DE_HVS_NWW_total_DE_0_Apr[complete.cases(Env_DE_HVS_NWW_total_DE_0_Apr$ID), ] # Remove NA values for ID

# ANALYSE CONTINUOUS ENV. VARIABLES OVER THE POSSIBLE DE BINS 

# OVER COMPLETE AVAILABLE TIME PERIOD

# HVS river
Env_DE_HDC_HDE_tot <- subset(Env_DE_HVS_NWW_total, ID == "HDC_HDE")
Tilt_HDC_limit <-  as.POSIXct("2023-10-10 00:00:00", tz = "UTC") # Set limit till October 10, the read out date after which tilt HDC was altered again
Env_DE_HDC_HDE_tot$Tilt[Env_DE_HDC_HDE_tot$DateTime_UTC > Tilt_HDC_limit] <- NA

# Analyse DE ~ env. variables between HDC - HDB and HDD between 05-04-2023 - 31-12-2023 
Bar_DE_HDC_HDB_HDE <- ggplot(Env_DE_HDC_HDE_tot, aes(x = factor(DE))) +
  geom_bar(position = "dodge", color = "black", fill="red") +
  scale_y_continuous(limits = c(0, 5000), breaks = seq(0, 5000, by = 500)) +
  labs(x = "Detection efficiency (%)",  y = "Frequency", title = "DE HDC-HDE April-Dec 2023") +
  #scale_fill_manual(values = c("white", "red"),  labels = c("HDC - HDB ", "HDC - HDD"), name = "HDC - receiver combination")+
  theme(axis.text.x = element_text(size = 8))
print(Bar_DE_HDC_HDB_HDE)

# Discharge
Discharge_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = abs(Discharge), fill = factor(ID)))+   
  geom_boxplot()+
  labs(y = "Discharge (m3/s)", x = "Detection efficiency(%)") +
  ggtitle("Average discharge per detection efficiency HDC")+
  theme(axis.text.x = element_text(size = 8))
#print(Discharge_HDC)
anova_DI_HVSr <- aov(abs(Discharge) ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_DI_HVSr) #(F = 190.4, df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_DI_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_DI_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= b , 2= a, 3= bc , 4= c , 5= d , 6= e , 7= f)

# Waterheight
Waterheight_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Waterheight))+  
  geom_boxplot()+
  labs(y = "Waterheight (cm)", x = "Detection efficiency(%)") +
  ggtitle("Avg. Waterheight per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))
#scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) 
print(Waterheight_HDC)
anova_WH_HVSr <- aov(Waterheight ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_WH_HVSr) #(F = 68.22, df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_WH_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_WH_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= a , 2=b, 3= b , 4= c , 5= d , 6=d , 7= d)
print(cld_HSD_WH_HVSr)

# Windspeed
WindSpeed_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Windspeed))+  
  geom_boxplot()+
  labs(y = "Windspeed (m/s)", x = "Detection efficiency(%)") +
  ggtitle("Avg. Windspeed per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))
print(WindSpeed_HDC)
anova_WS_HVSr <- aov(Windspeed ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_WS_HVSr) #(F = 0.936  , df= 6, p = <0.467 (NS))
emms <- emmeans(anova_WS_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_WS_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= a , 2= a, 3= a , 4= a , 5= a , 6= a , 7= a)
print(cld_HSD_WS_HVSr)

# Sluice opening
SluiceOpening_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Sluice_Opening))+  
  geom_boxplot()+
  labs(y = "Sluice opening (m2)" , x = "Detection efficiency(%)") +
  ggtitle("Avg. Sluice Opening per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, by = 400)) 
print(SluiceOpening_HDC)
anova_SO_HVSr <- aov(Sluice_Opening ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_SO_HVSr) #(F = 133.4, df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_SO_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_SO_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= a , 2= a, 3= ab , 4= b , 5= c , 6= d , 7= e)
print(cld_HSD_SO_HVSr)

# Temperature
Temperature_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Temp_rec))+  
  geom_boxplot()+
  labs(y = "Temperature (Celcius)" , x = "Detection efficiency(%)") +
  ggtitle("Avg. temperature per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))
print(Temperature_HDC)
anova_Temp_HVSr <- aov(Temp_rec ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_Temp_HVSr) #(F =  175.8, df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_Temp_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Temp_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= e , 2= e, 3= d , 4= c , 5= b , 6= a , 7= a)
print(cld_HSD_Temp_HVSr)

# Chloride
Chloride_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Chloride))+  
  geom_boxplot()+
  labs(y = "Chloride (mg/L)" , x = "Detection efficiency(%)") +
  ggtitle("Avg. Chloride per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 30)) 
print(Chloride_HDC)
anova_Chlo_HVSr <- aov(Chloride ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_Chlo_HVSr) #(F =  25.77, df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_Chlo_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Chlo_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= de , 2= e, 3= cd , 4= cd , 5= bc , 6= ab , 7= a)
print(cld_HSD_Chlo_HVSr)

# Noise
Noise_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Noise))+  
  geom_boxplot()+
  labs(y = "Noise (mV)" , x = "Detection efficiency(%)") +
  ggtitle("Avg. Noise per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 40)) 
print(Noise_HDC)
anova_Noise_HVSr <- aov(Noise ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_Noise_HVSr) #(F =  8.104 , df= 6, p = <9.72e-09 *** (0 ‘***’))
emms <- emmeans(anova_Noise_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Noise_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= b , 2= a, 3= a , 4= ab , 5= ab , 6= ab , 7= ab)
print(cld_HSD_Noise_HVSr)

# Tilt
Tilt_HDC <- ggplot(Env_DE_HDC_HDE_tot, aes(x = (factor(DE)), y = Tilt))+  
  geom_boxplot()+
  labs(y = "Tilt (degrees)" , x = "Detection efficiency(%)") +
  ggtitle("Avg. Tilt per DE HDC-HDE")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5)) 
print(Tilt_HDC)
anova_Tilt_HVSr <- aov(Tilt ~ factor(DE), data = Env_DE_HDC_HDE_tot)
summary(anova_Tilt_HVSr) #(F =  17.01  , df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Tilt_HVSr, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Tilt_HVSr <- cld(emms, alpha = 0.05, Letters = letters) # (1= b , 2= a, 3= a , 4= a , 5= ab , 6= ab , 7= ab)
print(cld_HSD_Tilt_HVSr)

# Combine the plots into one figure
Env_DE_HDC_Complete <- grid.arrange(Bar_DE_HDC_HDB_HDE, SluiceOpening_HDC , Waterheight_HDC, WindSpeed_HDC , Temperature_HDC, Chloride_HDC, Noise_HDC,  Tilt_HDC, ncol = 3, nrow=3)#+
#ggtitle("Average values for env. variabels per DE between HD-C - HD-D for Apr 15 - 20")
print(Env_DE_HDC_Complete)

# SEPERATE PLOTS FOR SLUICE MANAGEMENT TYPE AND WIND DIRECTION

# SLUICE MANAGEMENT

# Average DE per sluice management type for four different sluice opening bins from 0 - 600 m2

# Change column name for discharging as sluice management type
Env_DE_HVS_NWW_total$Spui_Kier_Closed[Env_DE_HVS_NWW_total$Spui_Kier_Closed == "Spui"] <- "Discharge"

# SLUICE MANAGEMENT TYPE HVS River
# Select only HVS river data and rows where the sluice management is discharge of kier
Env_DE_HDC_HDE <- subset(Env_DE_HVS_NWW_total, ID %in% c("HDC_HDE"))
Env_DE_HDC_HDE_DKC <- subset(Env_DE_HDC_HDE, Spui_Kier_Closed %in% c("Closed", "Discharge", "Kier"))

# Clarify that sluice management type should be read as factors. Only then will they be plotted in the right order on the final x axis
Env_DE_HDC_HDE_DKC$Spui_Kier_Closed <- factor(Env_DE_HDC_HDE_DKC$Spui_Kier_Closed,
                                              levels = c("Closed" , "Discharge", "Kier"))

# Create bins for Sluice openign (SO) to plot bars for wind direction against. Bin size = 200 m2, max = 800 m2 based on max sluice opening during kier management
Env_DE_HDC_HDE_SO_0_150 <- subset(Env_DE_HDC_HDE_DKC, 
                                  Sluice_Opening >= 0 & Sluice_Opening <= 150)
Env_DE_HDC_HDE_SO_150_300 <- subset(Env_DE_HDC_HDE_DKC, 
                                    Sluice_Opening > 150 & Sluice_Opening <= 300)
Env_DE_HDC_HDE_SO_300_450 <- subset(Env_DE_HDC_HDE_DKC, 
                                    Sluice_Opening > 300 & Sluice_Opening <= 450)
Env_DE_HDC_HDE_SO_450_600 <- subset(Env_DE_HDC_HDE_DKC, 
                                    Sluice_Opening > 450 & Sluice_Opening <= 600)


# Create groups for each of the sluice opening bins and calculate standard error (SE)

# Sluice opening (SO) = 0- 150 m2
SO_0_150_HDC_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_0_150, FUN = mean)
SE_SO_0_150_HDC <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_0_150, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_0_150_HDC <- rename(SE_SO_0_150_HDC, DE_se = DE) 
SO_0_150_HDC_groups <- merge(SO_0_150_HDC_groups, SE_SO_0_150_HDC, by = "Spui_Kier_Closed")

# Sluice opening (SO) = 150-300 m2
SO_150_300_HDC_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_150_300, FUN = mean)
SE_SO_150_300_HDC <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_150_300, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_150_300_HDC <- rename(SE_SO_150_300_HDC, DE_se = DE) 
SO_150_300_HDC_groups <- merge(SO_150_300_HDC_groups, SE_SO_150_300_HDC, by = "Spui_Kier_Closed")

# Sluice opening (SO) = 300-450 m2
SO_300_450_HDC_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_300_450, FUN = mean)
SE_SO_300_450_HDC <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_300_450, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_300_450_HDC <- rename(SE_SO_300_450_HDC, DE_se = DE) 
SO_300_450_HDC_groups <- merge(SO_300_450_HDC_groups, SE_SO_300_450_HDC, by = "Spui_Kier_Closed")

# Sluice opening (SO) = 450-600 m2
SO_450_600_HDC_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600, FUN = mean)
SE_SO_450_600_HDC <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_450_600_HDC <- rename(SE_SO_450_600_HDC, DE_se = DE) 
SO_450_600_HDC_groups <- merge(SO_450_600_HDC_groups, SE_SO_450_600_HDC, by = "Spui_Kier_Closed")

# Create bar graphs for each sluice management type over different sluice opening bins

# Sluice opening (SO) 0 - 150 m2
SO_0_150_HDC_BarPlot <- ggplot(SO_0_150_HDC_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_0_150_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "Detection efficiency in HVS river over different types of sluice management", x = "0 - 150 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_SO_0_150$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = 1.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

#print(SO_0_150_HDC_BarPlot)

# Sluice opening (SO) 150 - 300 m2
SO_150_300_HDC_BarPlot <- ggplot(SO_150_300_HDC_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_150_300_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "150 - 300 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_SO_150_300$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = -0.75, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

#print(SO_150_300_HDC_BarPlot)

# Sluice opening (SO) 300 - 450 m2
SO_300_450_HDC_BarPlot <- ggplot(SO_300_450_HDC_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_300_450_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "300 - 450 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_SO_300_450$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = 2.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

#print(SO_300_450_HDC_BarPlot)

# Sluice opening (SO) 450 - 600 m2
SO_450_600_HDC_BarPlot <- ggplot(SO_450_600_HDC_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_450_600_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "450 -600 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_SO_450_600$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = 2.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

#print(SO_450_600_HDC_BarPlot)

# Combine the plots for effect wind direction in the HVS river area. 
Sluice_management_HDC_BarPlot_combined <- grid.arrange(SO_0_150_HDC_BarPlot, SO_150_300_HDC_BarPlot, SO_300_450_HDC_BarPlot, SO_450_600_HDC_BarPlot, ncol = 2, nrow =2)

# Statistics of sluice magagment type HVS river
ggplot(Env_DE_HDC_HDE_SO_450_600, aes(sample = DE)) + # Normality violated
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Spui_Kier_Closed)
leveneTest(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600) # Homogeneity of variances violated

# Perform kruskal-wallis test instead
kruskal_SO_HVSr <- kruskal.test(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600)
print(kruskal_SO_HVSr)
pairwise_SO_HVSr <- pairwise.wilcox.test(Env_DE_HDC_HDE_SO_450_600$DE, Env_DE_HDC_HDE_SO_450_600$Spui_Kier_Closed, p.adjust.method = "holm")
print(pairwise_SO_HVSr)
p_SO_HVSr <- pairwise_SO_HVSr$p_SO_HVSr
Labels_SO_HVSr <- multcompLetters(p_values)
print(Labels_SO_HVSr$Letters)

# Summary statistical results
# Env_DE_HDC_HDE_SO_0_150. C =a, D = a, K= b.  X2 = 40.884, df = 2, p-value = 1.325e-09***
# Env_DE_HDC_HDE_SO_150_300. D = a, K= b.  X2 = 64.736, df = 2, p-value = 8.562e-16
# Env_DE_HDC_HDE_SO_300_450. D = a, K= b.  X2 = 10.449, df = 2, p-value = 0.001227
# Env_DE_HDC_HDE_SO_450_600. D = a, K= b.  X2 = 10.416, df = 2, p-value = 0.001227


# WIND DIRECTION

# WIND DIRECTION HVS RIVER
Env_DE_HDC_HDE <- subset(Env_DE_HVS_NWW_total, ID %in% c("HDC_HDE"))
summary(Env_DE_HDC_HDE$Windspeed) # min = 0. max = 19.6 Use bins of 4 m/s with 20 as max.

# Clarify that wind direction should be read as factors. Only then will they be plotted in the right order on the final x axis
Env_DE_HDC_HDE$Wind_Direction <- factor(Env_DE_HDC_HDE$Wind_Direction,
                                        levels = c("North", "East", "South", "West"))

# Create bins for windspeed to plot bars for wind direction against. Bin size = 4 m/s, max = 20 m/s.
Env_DE_HDC_HDE_Wind_0_4 <- subset(Env_DE_HDC_HDE, Windspeed <= 4)
Env_DE_HDC_HDE_Wind_4_8 <- subset(Env_DE_HDC_HDE, 
                                  Windspeed > 4 & Windspeed <= 8)
Env_DE_HDC_HDE_Wind_8_12 <- subset(Env_DE_HDC_HDE, 
                                   Windspeed > 8 & Windspeed <= 12)
Env_DE_HDC_HDE_Wind_12_16 <- subset(Env_DE_HDC_HDE, 
                                    Windspeed > 12 & Windspeed <= 16)
Env_DE_HDC_HDE_Wind_16_20 <- subset(Env_DE_HDC_HDE, 
                                    Windspeed > 16 & Windspeed <= 20)

# Create groups for compass wind directions for each of the distance bins and calculate standard error (SE)

# Windspeed = 0-4 m/s
WindSpeed_0_4_HDC_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_0_4, FUN = mean)
SE_WindSpeed_0_4_HDC <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_0_4, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_0_4_HDC <- rename(SE_WindSpeed_0_4_HDC, DE_se = DE) 
WindSpeed_0_4_HDC_groups <- merge(WindSpeed_0_4_HDC_groups, SE_WindSpeed_0_4_HDC, by = "Wind_Direction")

# Windspeed = 4-8 m/s
WindSpeed_4_8_HDC_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_4_8, FUN = mean)
SE_WindSpeed_4_8_HDC <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_4_8, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_4_8_HDC <- rename(SE_WindSpeed_4_8_HDC, DE_se = DE) 
WindSpeed_4_8_HDC_groups <- merge(WindSpeed_4_8_HDC_groups, SE_WindSpeed_4_8_HDC, by = "Wind_Direction")

# Windspeed = 8 -12 m/s
WindSpeed_8_12_HDC_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_8_12, FUN = mean)
SE_WindSpeed_8_12_HDC <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_8_12, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_8_12_HDC <- rename(SE_WindSpeed_8_12_HDC, DE_se = DE) 
WindSpeed_8_12_HDC_groups <- merge(WindSpeed_8_12_HDC_groups, SE_WindSpeed_8_12_HDC, by = "Wind_Direction")

# Windspeed = 12 -16 m/s
WindSpeed_12_16_HDC_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_12_16, FUN = mean)
SE_WindSpeed_12_16_HDC <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_12_16, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_12_16_HDC <- rename(SE_WindSpeed_12_16_HDC, DE_se = DE) 
WindSpeed_12_16_HDC_groups <- merge(WindSpeed_12_16_HDC_groups, SE_WindSpeed_12_16_HDC, by = "Wind_Direction")

# Windspeed = 16 -20 m/s
WindSpeed_16_20_HDC_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_16_20, FUN = mean)
SE_WindSpeed_16_20_HDC <- aggregate(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_16_20, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_16_20_HDC <- rename(SE_WindSpeed_16_20_HDC, DE_se = DE) 
WindSpeed_16_20_HDC_groups <- merge(WindSpeed_16_20_HDC_groups, SE_WindSpeed_16_20_HDC, by = "Wind_Direction")

# Create bar graphs for each sluice opening bin with Spui, Kier and Closed as bars

# Wind speed  = 0 - 4 m/s
WindSpeed_0_4_HDC_BarPlot <- ggplot(WindSpeed_0_4_HDC_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_0_4_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "Detection efficiency in the HVS river area over different wind directions", x = "0 - 4 m/s", y = "Average DE (% /hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_Wind_0_4$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -1.0, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_0_4_HDC_BarPlot)

# Wind speed  = 4 - 8 m/s
WindSpeed_4_8_HDC_BarPlot <- ggplot(WindSpeed_4_8_HDC_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_4_8_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "4 - 8 m/s", y = "Average DE (% /hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_Wind_4_8$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -0.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))
print(WindSpeed_4_8_HDC_BarPlot)

# Wind speed  = 8 - 12 m/s
WindSpeed_8_12_HDC_BarPlot <- ggplot(WindSpeed_8_12_HDC_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_8_12_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "8 - 12 m/s", y = "Average DE (% /hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_Wind_8_12$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -2, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_8_12_HDC_BarPlot)

# Wind speed  = 12 - 16 m/s
WindSpeed_12_16_HDC_BarPlot <- ggplot(WindSpeed_12_16_HDC_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_12_16_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "12 - 16 m/s", y = "Average DE (%)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_Wind_12_16$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -1.0, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_12_16_HDC_BarPlot)

# Wind speed  = 16 - 20 m/s
WindSpeed_16_20_HDC_BarPlot <- ggplot(WindSpeed_16_20_HDC_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_16_20_HDC_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "16 - 20 m/s", y = "Average DE (% /hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HDC_HDE_Wind_16_20$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = - 2.0, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size =13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_16_20_HDC_BarPlot)

# Combine the plots for effect wind direction in the HVS river area. # THIS ONE DOES NOT LOOK GOOD
Wind_Direction_HDC_BarPlot_combined <- grid.arrange(WindSpeed_0_4_HDC_BarPlot, WindSpeed_4_8_HDC_BarPlot, WindSpeed_8_12_HDC_BarPlot, WindSpeed_12_16_HDC_BarPlot, WindSpeed_16_20_HDC_BarPlot, ncol = 2, nrow =3)

# Statistics of wind direction HVS river
ggplot(Env_DE_HDC_HDE_Wind_16_20, aes(sample = DE)) + # Normality violated
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Wind_Direction)
leveneTest(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_16_20) # Homogeneity of variances violated

# Perform kruskal-wallis test instead
kruskal_Wind_HVSr <- kruskal.test(DE ~ Wind_Direction, data = Env_DE_HDC_HDE_Wind_16_20)
print(kruskal_Wind_HVSr)
pairwise_Wind_HVSr <- pairwise.wilcox.test(Env_DE_HDC_HDE_Wind_16_20$DE, Env_DE_HDC_HDE_Wind_16_20$Wind_Direction, p.adjust.method = "holm")
print(pairwise_Wind_HVSr)
p_values_Wind_HVSr <- pairwise_Wind_HVSr$p.value
Labels_Wind_HVSr <- multcompLetters(p_values_Wind_HVSr)
print(Labels_Wind_HVSr$Letters)

# Summary statistical results
# Env_DE_HDC_HDE_Wind_0_4 N =a, E = a. S = a, W= a. .X2 = 11.723 , df = 3, p-value = 0.008394
# Env_DE_HDC_HDE_Wind_4_8 N =a, E = a. S = b, W= c. X2 = 135.31 , df = 3, p-value = 2.2e-16
# Env_DE_HDC_HDE_Wind_8_12 N =a, E = a. S = b, W= c. a. X2 = 26.465 , df = 3, p-value = 7.622e-06
# Env_DE_HDC_HDE_Wind_12_16 N =a, E = a. S = b, W= b. X2 = 30.246 , df = 3, p-value = 1.225e-06
# Env_DE_HDC_HDE_Wind_16_20 N =a, E = a. S = a, W= a. X2 = , df = 3, p-value = 

# Perform kruskal-wallis test instead
kruskal_SO_HVSr <- kruskal.test(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600)
print(kruskal_SO_HVSr)
pairwise_SO_HVSr <- pairwise.wilcox.test(Env_DE_HDC_HDE_SO_450_600$DE, Env_DE_HDC_HDE_SO_450_600$Spui_Kier_Closed, p.adjust.method = "holm")
print(pairwise_SO_HVSr)
p_SO_HVSr <- pairwise_SO_HVSr$p_SO_HVSr
Labels_SO_HVSr <- multcompLetters(p_values)
print(Labels_SO_HVSr$Letters)















# HVS sea
Env_DE_HS6_HS8_tot <- subset(Env_DE_HVS_NWW_total, ID %in% c("HS6_HS8"))

# Analyse DE ~ env. variables between HDC - HDB and HDD between 05-04-2023 - 31-12-2023 
Bar_DE_HS6_HS8 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = factor(DE))) +
  geom_bar(position = "dodge", color = "black", fill = "red") +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
  labs(x = "Detection efficiency (%)",  y = "Frequency", title = "DE HS6 - HS8 Jan - Dec 2023") +
  theme(axis.text.x = element_text(size = 8))
print(Bar_DE_HS6_HS8)

# Discharge
Discharge_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = abs(Discharge)))+   
  geom_boxplot()+
  labs(y = "Discharge (m3/s)", x = "Detection efficiency(%)") +
  ggtitle("Avg discharge per DE HS6-HS8")+
  theme(axis.text.x = element_text(size = 8))
print(Discharge_HS6)
anova_DI_HVSs <- aov(abs(Discharge) ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_DI_HVSs) #(F = 15.38 , df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_DI_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_DI_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= c  , 2= ab , 3= a  , 4= a , 5= a, 6= ab  , 7= bc )
print(cld_HSD_DI_HVSs)

# Waterheight
Waterheight_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Waterheight))+  
  geom_boxplot()+
  labs(y = "Waterheight (cm)", x = "Detection efficiency(%)") +
  ggtitle("Avg waterheight per DE HS6 - HS8")+
  theme(axis.text.x = element_text(size = 8))
#print(Waterheight_HS6)
anova_WH_HVSs <- aov(Waterheight ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_WH_HVSs) #(F = 14.2   , df= 6, p = <3.82e-16 *** (0 ‘***’))
emms <- emmeans(anova_WH_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_WH_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= c , 2= c , 3= c , 4= bc  , 5= b , 6= ab  , 7= a )
print(cld_HSD_WH_HVSs)

# Windspeed
WindSpeed_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Windspeed))+  
  geom_boxplot()+
  labs(y = "Windspeed (m/s)", x = "Detection efficiency(%)") +
  ggtitle("Avg windspeed per DE HS6 - HS8")+
  theme(axis.text.x = element_text(size = 8))
#print(WindSpeed_HS6)
anova_WS_HVSs <- aov(Windspeed ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_WS_HVSs) #(F = 211.5, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_WS_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_WS_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= d , 2= c , 3= b , 4= ab , 5= a, 6= a , 7= a)
print(cld_HSD_WS_HVSs)

# Sluice opening
SluiceOpening_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Sluice_Opening))+  
  geom_boxplot()+
  labs(y = "Sluice opening (m2)" , x = "Detection efficiency(%)") +
  ggtitle("Avg sluice opening per DE HS6 - HS8")+
  scale_fill_manual(values = c("green", "red"),  labels = c("HDC - HDB ", "HDC - HDD"), name = "HDC - receiver combination")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200))
print(SluiceOpening_HS6)
anova_SO_HVSs <- aov(Sluice_Opening ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_SO_HVSs) #(F = 16.4 , df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_SO_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_SO_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= c  , 2= b , 3= ab , 4= a , 5= ab , 6= ab , 7= b). THIS LOOKS LIKE A STRANGE RESULT
print(cld_HSD_SO_HVSs)

# Temperature
Temperature_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Temp_rec))+  
  geom_boxplot()+
  labs(y = "Temperature (Celcius)" , x = "Detection efficiency(%)") +
  ggtitle("Avg temperature per DE HS6 - HS8")+
  scale_fill_manual(values = c("green", "red"),  labels = c("HDC - HDB ", "HDC - HDD"), name = "HDC - receiver combination")+
  theme(axis.text.x = element_text(size = 8))
print(Temperature_HS6)
anova_Temp_HVSs <- aov(Temp_rec ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_Temp_HVSs) #(F =  42.03, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Temp_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Temp_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= c  , 2= c, 3= d  , 4= d  , 5= c , 6= b , 7= a)
print(cld_HSD_Temp_HVSs)

# Chloride
Chloride_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Chloride))+  
  geom_boxplot()+
  labs(y = "Chloride (mg/L)" , x = "Detection efficiency(%)") +
  ggtitle("Avg chloride per DE HS6 - HS8")+
  theme(axis.text.x = element_text(size = 8))
print(Chloride_HS6)
anova_Chlo_HVSs <- aov(Chloride ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_Chlo_HVSs) #(F = 15.63, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Chlo_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Chlo_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= c , 2= c , 3= c  , 4= c  , 5= c , 6= b , 7= a )
print(cld_HSD_Chlo_HVSs)

# Noise
Noise_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Noise))+  
  geom_boxplot()+
  labs(y = "Noise (mV)" , x = "Detection efficiency(%)") +
  ggtitle("Avg noise per DE HS6 - HS8")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, by = 40)) 
# print(Noise_HS6)
anova_Noise_HVSs <- aov(Noise ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_Noise_HVSs) #(F = 57.8, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Noise_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Noise_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= d , 2= c, 3= b , 4= ab  , 5= a , 6= a  , 7= a)
print(cld_HSD_Noise_HVSs)

# Tilt
Tilt_HS6 <- ggplot(Env_DE_HS6_HS8_tot, aes(x = (factor(DE)), y = Tilt))+  
  geom_boxplot()+
  labs(y = "Tilt (degrees)" , x = "Detection efficiency(%)") +
  ggtitle("Avg Tilt per DE HS6 - HS8")+
  theme(axis.text.x = element_text(size = 8))+
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 2)) 
print(Tilt_HS6)
anova_Tilt_HVSs <- aov(Tilt ~ factor(DE), data = Env_DE_HS6_HS8_tot)
summary(anova_Tilt_HVSs) #(F =34.73, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Tilt_HVSs, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Tilt_HVSs <- cld(emms, alpha = 0.05, Letters = letters) # (1= c  , 2= b , 3= a , 4= a  , 5= a, 6= ab , 7= b)
print(cld_HSD_Tilt_HVSs)

# Combine the plots into one figure
Env_DE_HS6_Complete <- grid.arrange(Bar_DE_HS6_HS8, SluiceOpening_HS6, Waterheight_HS6 ,WindSpeed_HS6, Temperature_HS6, Chloride_HS6, Noise_HS6,  Tilt_HS6, ncol = 3, nrow=3)#+
#ggtitle("Average values for env. variabels per DE between HD-C - HD-D for Apr 15 - 20")
print(Env_DE_HS6_Complete)

# Make seperate plots for sluice management and wind direction

# Sluice management 


# Method 2: Average DE over different SO bins for each sluice management type

# Change column name for discharging as sluice management type
Env_DE_HVS_NWW_total$Spui_Kier_Closed[Env_DE_HVS_NWW_total$Spui_Kier_Closed == "Spui"] <- "Discharge"

# SLUICE MANAGEMENT TYPE HVS River
# Select only HVS river data and rows where the sluice management is discharge of kier
Env_DE_HS6_HS8 <- subset(Env_DE_HVS_NWW_total, ID %in% c("HS6_HS8"))
Env_DE_HS6_HS8_DKC <- subset(Env_DE_HS6_HS8, Spui_Kier_Closed %in% c("Closed", "Discharge", "Kier"))

# Clarify that sluice management type should be read as factors. Only then will they be plotted in the right order on the final x axis
Env_DE_HS6_HS8_DKC$Spui_Kier_Closed <- factor(Env_DE_HS6_HS8_DKC$Spui_Kier_Closed,
                                              levels = c("Closed" , "Discharge", "Kier"))

# Create bins for Sluice openign (SO) to plot bars for wind direction against. Bin size = 200 m2, max = 800 m2 based on max sluice opening during kier management
Env_DE_HS6_HS8_SO_0_150 <- subset(Env_DE_HS6_HS8_DKC, 
                                  Sluice_Opening >= 0 & Sluice_Opening <= 150)
Env_DE_HS6_HS8_SO_150_300 <- subset(Env_DE_HS6_HS8_DKC, 
                                    Sluice_Opening > 150 & Sluice_Opening <= 300)
Env_DE_HS6_HS8_SO_300_450 <- subset(Env_DE_HS6_HS8_DKC, 
                                    Sluice_Opening > 300 & Sluice_Opening <= 450)
Env_DE_HS6_HS8_SO_450_600 <- subset(Env_DE_HS6_HS8_DKC, 
                                    Sluice_Opening > 450 & Sluice_Opening <= 600)


# Create groups for sluice management types for each of the sluice opening bins and calculate standard error (SE)

# Sluice opening (SO) = 0- 150 m2
SO_0_150_HS6_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_0_150, FUN = mean)
SE_SO_0_150_HS6 <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_0_150, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_0_150_HS6 <- rename(SE_SO_0_150_HS6, DE_se = DE) 
SO_0_150_HS6_groups <- merge(SO_0_150_HS6_groups, SE_SO_0_150_HS6, by = "Spui_Kier_Closed")

# Sluice opening (SO) = 150-300 m2
SO_150_300_HS6_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_150_300, FUN = mean)
SE_SO_150_300_HS6 <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_150_300, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_150_300_HS6 <- rename(SE_SO_150_300_HS6, DE_se = DE) 
SO_150_300_HS6_groups <- merge(SO_150_300_HS6_groups, SE_SO_150_300_HS6, by = "Spui_Kier_Closed")

# Sluice opening (SO) = 300-450 m2
SO_300_450_HS6_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_300_450, FUN = mean)
SE_SO_300_450_HS6 <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_300_450, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_300_450_HS6 <- rename(SE_SO_300_450_HS6, DE_se = DE) 
SO_300_450_HS6_groups <- merge(SO_300_450_HS6_groups, SE_SO_300_450_HS6, by = "Spui_Kier_Closed")

# Sluice opening (SO) = 450-600 m2
SO_450_600_HS6_groups <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_450_600, FUN = mean)
SE_SO_450_600_HS6 <- aggregate(DE ~ Spui_Kier_Closed, data = Env_DE_HS6_HS8_SO_450_600, FUN = function(x) sd(x)/sqrt(length(x)))
SE_SO_450_600_HS6 <- rename(SE_SO_450_600_HS6, DE_se = DE) 
SO_450_600_HS6_groups <- merge(SO_450_600_HS6_groups, SE_SO_450_600_HS6, by = "Spui_Kier_Closed")

# Create bar graphs for each sluice management type over different sluice opening bins

# Sluice opening (SO) 0 - 150 m2
SO_0_150_HS6_BarPlot <- ggplot(SO_0_150_HS6_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_0_150_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "Detection efficiency in HVS sea over different types of sluice management", x = "0 - 150 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_SO_0_150$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = 1.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(SO_0_150_HS6_BarPlot)

# Sluice opening (SO) 150 - 300 m2
SO_150_300_HS6_BarPlot <- ggplot(SO_150_300_HS6_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_150_300_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "150 - 300 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_SO_150_300$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = -0.75, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(SO_150_300_HS6_BarPlot)

# Sluice opening (SO) 300 - 450 m2
SO_300_450_HS6_BarPlot <- ggplot(SO_300_450_HS6_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_300_450_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "300 - 450 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_SO_300_450$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = 2.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(SO_300_450_HS6_BarPlot)

# Sluice opening (SO) 450 - 600 m2
SO_450_600_HS6_BarPlot <- ggplot(SO_450_600_HS6_groups, aes(x = Spui_Kier_Closed, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = SO_450_600_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "450 -600 m2", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_SO_450_600$Spui_Kier_Closed)[as.character(Spui_Kier_Closed)])), 
            vjust = 2.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(SO_450_600_HS6_BarPlot)

# Combine the plots for effect wind direction in the HVS sea area. 
Sluice_management_HDC_BarPlot_combined <- grid.arrange(SO_0_150_HS6_BarPlot, SO_150_300_HS6_BarPlot, SO_300_450_HS6_BarPlot, SO_450_600_HS6_BarPlot, ncol = 2, nrow =2)

# Statistics of sluice magagment type HVS sea
ggplot(Env_DE_HDC_HDE_SO_450_600, aes(sample = DE)) + # Normality violated
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Spui_Kier_Closed)
leveneTest(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600) # Homogeneity of variances violated

# Perform kruskal-wallis test instead
kruskal_SO_HVSr <- kruskal.test(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600)
print(kruskal_SO_HVSr)
pairwise_SO_HVSr <- pairwise.wilcox.test(Env_DE_HDC_HDE_SO_450_600$DE, Env_DE_HDC_HDE_SO_450_600$Spui_Kier_Closed, p.adjust.method = "holm")
print(pairwise_SO_HVSr)
p_SO_HVSr <- pairwise_SO_HVSr$p_SO_HVSr
Labels_SO_HVSr <- multcompLetters(p_values)
print(Labels_SO_HVSr$Letters)

# Summary statistical results
# Env_DE_HDC_HDE_SO_0_150. C =a, D = a, K= b.  X2 = 40.884, df = 2, p-value = 1.325e-09***
# Env_DE_HDC_HDE_SO_150_300. D = a, K= b.  X2 = 64.736, df = 2, p-value = 8.562e-16
# Env_DE_HDC_HDE_SO_300_450. D = a, K= b.  X2 = 10.449, df = 2, p-value = 0.001227
# Env_DE_HDC_HDE_SO_450_600. D = a, K= b.  X2 = 10.416, df = 2, p-value = 0.001227


# Wind direction

# Method 2: Average DE over different windspeed bins for each wind direction 

# WIND DIRECTION HVS SEA
Env_DE_HS6_HS8 <- subset(Env_DE_HVS_NWW_total, ID %in% c("HS6_HS8"))
summary(Env_DE_HS6_HS8$Windspeed) # min = 0. max = 19.6. Use bins of 4 m/s with 20 as max.

# Clarify that wind direction should be read as factors. Only then will they be plotted in the right order on the final x axis
Env_DE_HS6_HS8$Wind_Direction <- factor(Env_DE_HS6_HS8$Wind_Direction,
                                        levels = c("North", "East", "South", "West"))

# Create bins for windspeed to plot bars for wind direction against. Bin size = 4 m/s, max = 20 m/s.
Env_DE_HS6_HS8_Wind_0_4 <- subset(Env_DE_HS6_HS8, Windspeed <= 4)
Env_DE_HS6_HS8_Wind_4_8 <- subset(Env_DE_HS6_HS8, 
                                  Windspeed > 4 & Windspeed <= 8)
Env_DE_HS6_HS8_Wind_8_12 <- subset(Env_DE_HS6_HS8, 
                                   Windspeed > 8 & Windspeed <= 12)
Env_DE_HS6_HS8_Wind_12_16 <- subset(Env_DE_HS6_HS8, 
                                    Windspeed > 12 & Windspeed <= 16)
Env_DE_HS6_HS8_Wind_16_20 <- subset(Env_DE_HS6_HS8, 
                                    Windspeed > 16 & Windspeed <= 20)

# Create groups for compass wind directions for each of the distance bins and calculate standard error (SE)

# Windspeed = 0-4 m/s
WindSpeed_0_4_HS6_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_0_4, FUN = mean)
SE_WindSpeed_0_4_HS6 <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_0_4, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_0_4_HS6 <- rename(SE_WindSpeed_0_4_HS6, DE_se = DE) 
WindSpeed_0_4_HS6_groups <- merge(WindSpeed_0_4_HS6_groups, SE_WindSpeed_0_4_HS6, by = "Wind_Direction")

# Windspeed = 4-8 m/s
WindSpeed_4_8_HS6_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_4_8, FUN = mean)
SE_WindSpeed_4_8_HS6 <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_4_8, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_4_8_HS6 <- rename(SE_WindSpeed_4_8_HS6, DE_se = DE) 
WindSpeed_4_8_HS6_groups <- merge(WindSpeed_4_8_HS6_groups, SE_WindSpeed_4_8_HS6, by = "Wind_Direction")

# Windspeed = 8 -12 m/s
WindSpeed_8_12_HS6_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_8_12, FUN = mean)
SE_WindSpeed_8_12_HS6 <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_8_12, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_8_12_HS6 <- rename(SE_WindSpeed_8_12_HS6, DE_se = DE) 
WindSpeed_8_12_HS6_groups <- merge(WindSpeed_8_12_HS6_groups, SE_WindSpeed_8_12_HS6, by = "Wind_Direction")

# Windspeed = 12 -16 m/s
WindSpeed_12_16_HS6_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_12_16, FUN = mean)
SE_WindSpeed_12_16_HS6 <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_12_16, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_12_16_HS6 <- rename(SE_WindSpeed_12_16_HS6, DE_se = DE) 
WindSpeed_12_16_HS6_groups <- merge(WindSpeed_12_16_HS6_groups, SE_WindSpeed_12_16_HS6, by = "Wind_Direction")

# Windspeed = 16 -20 m/s
WindSpeed_16_20_HS6_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_16_20, FUN = mean)
SE_WindSpeed_16_20_HS6 <- aggregate(DE ~ Wind_Direction, data = Env_DE_HS6_HS8_Wind_16_20, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_16_20_HS6 <- rename(SE_WindSpeed_16_20_HS6, DE_se = DE) 
WindSpeed_16_20_HS6_groups <- merge(WindSpeed_16_20_HS6_groups, SE_WindSpeed_16_20_HS6, by = "Wind_Direction")

# Create bar graphs for each wind direction over different windspeed bins
WindSpeed_0_4_HS6_BarPlot <- ggplot(WindSpeed_0_4_HS6_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_0_4_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "Detection efficiency in the HVS sea area over different wind directions", x = "0 - 4 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_Wind_0_4$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = 2.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_0_4_HS6_BarPlot)

# Wind speed  = 4 - 8 m/s
WindSpeed_4_8_HS6_BarPlot <- ggplot(WindSpeed_4_8_HS6_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_4_8_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "4 - 8 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_Wind_4_8$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = 1.8, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"), 
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_4_8_HS6_BarPlot)

# Wind speed  = 8 - 12 m/s
WindSpeed_8_12_HS6_BarPlot <- ggplot(WindSpeed_8_12_HS6_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_8_12_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "8 - 12 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_Wind_8_12$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -1.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_8_12_HS6_BarPlot)

# Wind speed  = 12 - 16 m/s
WindSpeed_12_16_HS6_BarPlot <- ggplot(WindSpeed_12_16_HS6_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_12_16_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "12 - 16 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_Wind_12_16$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -1.0, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  scale_fill_manual(values = c("North" = "blue", "East" = "green", "South" = "red", "West" = "skyblue"))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_12_16_HS6_BarPlot)

# Wind speed  = 16 - 20 m/s
WindSpeed_16_20_HS6_BarPlot <- ggplot(WindSpeed_16_20_HS6_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_16_20_HS6_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "16 - 20 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_HS6_HS8_Wind_16_20$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = - 2.2, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_16_20_HS6_BarPlot)

# Combine the plots for effect wind direction in the HVS river area. 
Wind_Direction_HS6_BarPlot_combined <- grid.arrange(WindSpeed_0_4_HS6_BarPlot, WindSpeed_4_8_HS6_BarPlot, WindSpeed_8_12_HS6_BarPlot, WindSpeed_12_16_HS6_BarPlot, WindSpeed_16_20_HS6_BarPlot, ncol = 2, nrow =3)

# Statistics of wind direction HVS sea
ggplot(Env_DE_HDC_HDE_SO_450_600, aes(sample = DE)) + # Normality violated
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Spui_Kier_Closed)
leveneTest(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600) # Homogeneity of variances violated

# Perform kruskal-wallis test instead
kruskal_SO_HVSr <- kruskal.test(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600)
print(kruskal_SO_HVSr)
pairwise_SO_HVSr <- pairwise.wilcox.test(Env_DE_HDC_HDE_SO_450_600$DE, Env_DE_HDC_HDE_SO_450_600$Spui_Kier_Closed, p.adjust.method = "holm")
print(pairwise_SO_HVSr)
p_SO_HVSr <- pairwise_SO_HVSr$p_SO_HVSr
Labels_SO_HVSr <- multcompLetters(p_values)
print(Labels_SO_HVSr$Letters)

# Summary statistical results
# Env_DE_HDC_HDE_SO_0_150. C =a, D = a, K= b.  X2 = 40.884, df = 2, p-value = 1.325e-09***
# Env_DE_HDC_HDE_SO_150_300. D = a, K= b.  X2 = 64.736, df = 2, p-value = 8.562e-16
# Env_DE_HDC_HDE_SO_300_450. D = a, K= b.  X2 = 10.449, df = 2, p-value = 0.001227
# Env_DE_HDC_HDE_SO_450_600. D = a, K= b.  X2 = 10.416, df = 2, p-value = 0.001227


# NWW
Env_DE_NW2_NW3_tot <- subset(Env_DE_HVS_NWW_total, ID %in% c("NW2_NW3"))

# Analyse DE ~ env. variables between HDC - HDB and HDD between 05-04-2023 - 31-12-2023 
Bar_DE_NW2_NW3 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = factor(DE))) +
  geom_bar(position = "dodge", color = "black", fill = "red") +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, by = 300)) +
  labs(x = "Detection efficiency (%)",  y = "Frequency", title = "DE NW2 - NW3 Jan - Dec 2023") +
  theme(axis.text.x = element_text(size = 8))
#print(Bar_DE_NW2_NW3)

# Discharge
Discharge_NW2 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = (factor(DE)), y = abs(Discharge)))+   
  geom_boxplot()+
  labs(y = "Discharge (m3/s)", x = "Detection efficiency(%)") +
  ggtitle("Avg discharge per DE NW2-NW3")+
  theme(axis.text.x = element_text(size = 8))
#print(Discharge_NW2)
anova_DI_NWW <- aov(abs(Discharge) ~ factor(DE), data = Env_DE_NW2_NW3_tot)
summary(anova_DI_NWW) #(F = 26.78, df= 6, p = <2e-16 *** (0 ‘***’))
emms <- emmeans(anova_DI_NWW, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_DI_NWW <- cld(emms, alpha = 0.05, Letters = letters) # (1= b , 2= a , 3=a , 4= a, 5=a , 6=a , 7=a )
print(cld_HSD_DI_NWW)

# Waterheight
Waterheight_NW2 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = (factor(DE)), y = Waterheight))+  
  geom_boxplot()+
  labs(y = "Waterheight (cm)", x = "Detection efficiency(%)") +
  ggtitle("Avg waterheight per DE NW2 - NW3")+
  theme(axis.text.x = element_text(size = 8))
print(Waterheight_NW2)
#anova_WH_NWW <- aov(Waterheight ~ factor(DE), data = Env_DE_NW2_NW3_tot)
summary(anova_WH_NWW) #(F = 185.9, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_WH_NWW, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_WH_NWW <- cld(emms, alpha = 0.05, Letters = letters) # (1=f, 2=e, 3=d, 4=c, 5=b , 6=a , 7= a)
print(cld_HSD_WH_NWW)

# Windspeed
WindSpeed_NW2 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = (factor(DE)), y = Windspeed))+  
  geom_boxplot()+
  labs(y = "Windspeed (m/s)", x = "Detection efficiency(%)") +
  ggtitle("Avg windspeed per DE NW2 - NW3")+
  theme(axis.text.x = element_text(size = 8))
#print(WindSpeed_NW2)
anova_WS_NWW <- aov(Windspeed ~ factor(DE), data = Env_DE_NW2_NW3_tot)
summary(anova_WS_NWW) #(F = 377.2, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_WS_NWW, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_WS_NWW <- cld(emms, alpha = 0.05, Letters = letters) # (1=e  , 2=d , 3=c , 4=b , 5=a , 6=a , 7=a)
print(cld_HSD_WS_NWW)

# Temperature
Temperature_NW2 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = (factor(DE)), y = Temp_rec))+  
  geom_boxplot()+
  labs(y = "Temperature (Celcius)" , x = "Detection efficiency(%)") +
  ggtitle("Avg temperature per DE NW2 - NW3")+
  theme(axis.text.x = element_text(size = 8))
#print(Temperature_NW2)
anova_Temp_NWW <- aov(Temp_rec ~ factor(DE), data = Env_DE_NW2_NW3_tot)
summary(anova_Temp_NWW) #(F =50.45, df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Temp_NWW, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Temp_NWW <- cld(emms, alpha = 0.05, Letters = letters) # (1=a  , 2=b , 3=c , 4=cd , 5=d , 6=de , 7=e )
print(cld_HSD_Temp_NWW)

# Noise
Noise_NW2 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = (factor(DE)), y = Noise))+  
  geom_boxplot()+
  labs(y = "Noise (mV)" , x = "Detection efficiency(%)") +
  ggtitle("Avg noise per DE NW2 - NW3")+
  theme(axis.text.x = element_text(size = 8))
#print(Noise_NW2)
anova_Noise_NWW <- aov(Noise ~ factor(DE), data = Env_DE_NW2_NW3_tot)
summary(anova_Noise_NWW) #(F = 5.686, df= 6, p = <6.62e-06 *** (0 ‘***’))
emms <- emmeans(anova_Noise_NWW, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Noise_NWW <- cld(emms, alpha = 0.05, Letters = letters) # (1=b  , 2=ab , 3=b , 4=b , 5=ab , 6=a , 7=ab)
print(cld_HSD_Noise_NWW)

# Tilt
Tilt_NW2 <- ggplot(Env_DE_NW2_NW3_tot, aes(x = (factor(DE)), y = Tilt))+  
  geom_boxplot()+
  labs(y = "Tilt (degrees)" , x = "Detection efficiency(%)") +
  ggtitle("Avg Tilt per DE NW2 - NW3")+
  theme(axis.text.x = element_text(size = 8))
#print(Tilt_NW2)
anova_Tilt_NWW <- aov(Tilt ~ factor(DE), data = Env_DE_NW2_NW3_tot)
summary(anova_Tilt_NWW) #(F= 15.1 , df= 6, p = <2e-16*** (0 ‘***’))
emms <- emmeans(anova_Tilt_NWW, ~ factor(DE))
pairs <- pairs(emms)
cld_HSD_Tilt_NWW <- cld(emms, alpha = 0.05, Letters = letters) # (1= ab, 2= a, 3= a, 4= a, 5=bc , 6=cd , 7=d)
print(cld_HSD_Tilt_NWW)

# Combine the plots into one figure
Env_DE_NW2_Complete <- grid.arrange(Bar_DE_NW2_NW3,  Discharge_NW2, Waterheight_NW2 , WindSpeed_NW2, Temperature_NW2 , Noise_NW2,  Tilt_NW2, ncol = 3, nrow=3)#+
#ggtitle("Average values for env. variabels per DE between HD-C - HD-D for Apr 15 - 20")
print(Env_DE_NW2_Complete)

# Wind direction 

# WIND DIRECTION NWW
Env_DE_NW2_NW3 <- subset(Env_DE_HVS_NWW_total, ID %in% c("NW2_NW3"))
summary(Env_DE_NW2_NW3$Windspeed) # min = 0. max = 20.0. Use bins of 4 m/s with 20 as max.

# Clarify that wind direction should be read as factors. Only then will they be plotted in the right order on the final x axis
Env_DE_NW2_NW3$Wind_Direction <- factor(Env_DE_NW2_NW3$Wind_Direction,
                                        levels = c("North", "East", "South", "West"))

# Create bins for windspeed to plot bars for wind direction against. Bin size = 4 m/s, max = 20 m/s.
Env_DE_NW2_NW3_Wind_0_4 <- subset(Env_DE_NW2_NW3, Windspeed <= 4)
Env_DE_NW2_NW3_Wind_4_8 <- subset(Env_DE_NW2_NW3, 
                                  Windspeed > 4 & Windspeed <= 8)
Env_DE_NW2_NW3_Wind_8_12 <- subset(Env_DE_NW2_NW3, 
                                   Windspeed > 8 & Windspeed <= 12)
Env_DE_NW2_NW3_Wind_12_16 <- subset(Env_DE_NW2_NW3, 
                                    Windspeed > 12 & Windspeed <= 16)
Env_DE_NW2_NW3_Wind_16_20 <- subset(Env_DE_NW2_NW3, 
                                    Windspeed > 16 & Windspeed <= 20)

# Create groups for compass wind directions for each of the distance bins and calculate standard error (SE)

# Windspeed = 0-4 m/s
WindSpeed_0_4_NW2_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_0_4, FUN = mean)
SE_WindSpeed_0_4_NW2 <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_0_4, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_0_4_NW2 <- rename(SE_WindSpeed_0_4_NW2, DE_se = DE) 
WindSpeed_0_4_NW2_groups <- merge(WindSpeed_0_4_NW2_groups, SE_WindSpeed_0_4_NW2, by = "Wind_Direction")

# Windspeed = 4-8 m/s
WindSpeed_4_8_NW2_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_4_8, FUN = mean)
SE_WindSpeed_4_8_NW2 <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_4_8, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_4_8_NW2 <- rename(SE_WindSpeed_4_8_NW2, DE_se = DE) 
WindSpeed_4_8_NW2_groups <- merge(WindSpeed_4_8_NW2_groups, SE_WindSpeed_4_8_NW2, by = "Wind_Direction")

# Windspeed = 8 -12 m/s
WindSpeed_8_12_NW2_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_8_12, FUN = mean)
SE_WindSpeed_8_12_NW2 <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_8_12, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_8_12_NW2 <- rename(SE_WindSpeed_8_12_NW2, DE_se = DE) 
WindSpeed_8_12_NW2_groups <- merge(WindSpeed_8_12_NW2_groups, SE_WindSpeed_8_12_NW2, by = "Wind_Direction")

# Windspeed = 12 -16 m/s
WindSpeed_12_16_NW2_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_12_16, FUN = mean)
SE_WindSpeed_12_16_NW2 <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_12_16, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_12_16_NW2 <- rename(SE_WindSpeed_12_16_NW2, DE_se = DE) 
WindSpeed_12_16_NW2_groups <- merge(WindSpeed_12_16_NW2_groups, SE_WindSpeed_12_16_NW2, by = "Wind_Direction")

# Windspeed = 16 -20 m/s
WindSpeed_16_20_NW2_groups <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_16_20, FUN = mean)
SE_WindSpeed_16_20_NW2 <- aggregate(DE ~ Wind_Direction, data = Env_DE_NW2_NW3_Wind_16_20, FUN = function(x) sd(x)/sqrt(length(x)))
SE_WindSpeed_16_20_NW2 <- rename(SE_WindSpeed_16_20_NW2, DE_se = DE) 
WindSpeed_16_20_NW2_groups <- merge(WindSpeed_16_20_NW2_groups, SE_WindSpeed_16_20_NW2, by = "Wind_Direction")

# Create bar graphs for each wind direction over different windspeed bins
WindSpeed_0_4_NW2_BarPlot <- ggplot(WindSpeed_0_4_NW2_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(stat = "identity", colour = "black", fill = "red") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_0_4_NW2_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "Detection efficiency in the NWW  area over different wind directions", x = "0 - 4 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_NW2_NW3_Wind_0_4$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = 2.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_0_4_NW2_BarPlot)

# Wind speed  = 4 - 8 m/s
WindSpeed_4_8_NW2_BarPlot <- ggplot(WindSpeed_4_8_NW2_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_4_8_NW2_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "4 - 8 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_NW2_NW3_Wind_4_8$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = 1.8, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"), 
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_4_8_NW2_BarPlot)

# Wind speed  = 8 - 12 m/s
WindSpeed_8_12_NW2_BarPlot <- ggplot(WindSpeed_8_12_NW2_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_8_12_NW2_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "8 - 12 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_NW2_NW3_Wind_8_12$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -1.5, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_8_12_HS6_BarPlot)

# Wind speed  = 12 - 16 m/s
WindSpeed_12_16_NW2_BarPlot <- ggplot(WindSpeed_12_16_NW2_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_12_16_NW2_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "12 - 16 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_NW2_NW3_Wind_12_16$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = -1.0, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  scale_fill_manual(values = c("North" = "blue", "East" = "green", "South" = "red", "West" = "skyblue"))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_12_16_NW2_BarPlot)

# Wind speed  = 16 - 20 m/s
WindSpeed_16_20_NW2_BarPlot <- ggplot(WindSpeed_16_20_NW2_groups, aes(x = Wind_Direction, y = DE)) +
  geom_bar(fill = "red", stat = "identity", colour = "black") +
  geom_errorbar(aes(ymin = DE - DE_se, ymax = DE + DE_se), data = WindSpeed_16_20_NW2_groups, width = 0.4, position = position_dodge(0.9)) +
  labs(title = "", x = "16 - 20 m/s", y = "Average DE (%/ hour)")+
  geom_text(aes(label = paste("n =", table(Env_DE_NW2_NW3_Wind_16_20$Wind_Direction)[as.character(Wind_Direction)])), 
            vjust = - 2.2, color = "black", size = 5)+
  scale_y_continuous(limits = c(0, 100))+
  theme(plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))

print(WindSpeed_16_20_NW2_BarPlot)

# Combine the plots for effect wind direction in the NWW area. 
Wind_Direction_NW2_BarPlot_combined <- grid.arrange(WindSpeed_0_4_NW2_BarPlot, WindSpeed_4_8_NW2_BarPlot, WindSpeed_8_12_NW2_BarPlot, WindSpeed_12_16_NW2_BarPlot, WindSpeed_16_20_NW2_BarPlot, ncol = 2, nrow =3)

# Statistics of wind direction in NWW
ggplot(Env_DE_HDC_HDE_SO_450_600, aes(sample = DE)) + # Normality violated
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Spui_Kier_Closed)
leveneTest(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600) # Homogeneity of variances violated

# Perform kruskal-wallis test instead
kruskal_SO_HVSr <- kruskal.test(DE ~ Spui_Kier_Closed, data = Env_DE_HDC_HDE_SO_450_600)
print(kruskal_SO_HVSr)
pairwise_SO_HVSr <- pairwise.wilcox.test(Env_DE_HDC_HDE_SO_450_600$DE, Env_DE_HDC_HDE_SO_450_600$Spui_Kier_Closed, p.adjust.method = "holm")
print(pairwise_SO_HVSr)
p_SO_HVSr <- pairwise_SO_HVSr$p_SO_HVSr
Labels_SO_HVSr <- multcompLetters(p_values)
print(Labels_SO_HVSr$Letters)

# Summary statistical results
# Env_DE_HDC_HDE_SO_0_150. C =a, D = a, K= b.  X2 = 40.884, df = 2, p-value = 1.325e-09***
# Env_DE_HDC_HDE_SO_150_300. D = a, K= b.  X2 = 64.736, df = 2, p-value = 8.562e-16
# Env_DE_HDC_HDE_SO_300_450. D = a, K= b.  X2 = 10.449, df = 2, p-value = 0.001227
# Env_DE_HDC_HDE_SO_450_600. D = a, K= b.  X2 = 10.416, df = 2, p-value = 0.001227






















