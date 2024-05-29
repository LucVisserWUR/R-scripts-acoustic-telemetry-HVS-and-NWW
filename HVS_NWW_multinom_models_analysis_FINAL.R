# MULTINOMIAL LOGISTIC REGRESSION, STATISTICAL ANALYSIS
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
library(ALEPlot)
library(ale) # visualise relationshop between predictor - and response variable. Did not work well
library(pdp)
library(margins)
library(mgcViz)
library(mgcv)

# NWW

# Try the model that Benoit sent you by email. CREATING GAM MODELS HAS NOT BEEN SUCCESSFUL SO FAR

# Benoit
GAM_NWW <- gamV(list(DE~s(ID, bs = 're')+s(WaterTemp0001,k=9)+s(AqSpd,k=9)+s(yday,k=20)+s(hour,k=9)+s(timeTag,k=9)+s(timeTagExtend,k=9)+s(center.Reef.dist,k=9),
                     ~s(ID, bs = 're')+s(WaterTemp0001,k=9)+s(AqSpd,k=9)+s(yday,k=20)+s(hour,k=9)+s(timeTag,k=9)+s(timeTagExtend,k=9)+s(center.Reef.dist,k=9)),
                family=multinom(K=2),data= subset(Env_DE_HVS_NWW_total, ID == "NW2_NW3"))

# My own code
GamV_NWW <- subset(Env_DE_HVS_NWW_total, ID == "NW2_NW3")
GamV_NWW$DE <- factor(GamV_NWW$DE)
GamV_NWW$Windspeed <- as.numeric(GamV_NWW$Windspeed)
GamV_NWW$Wind_Direction <- as.factor(GamV_NWW$Wind_Direction)
GamV_NWW$Temp_rec <- as.numeric(GamV_NWW$Temp_rec)
GamV_NWW$Tilt <- as.numeric(GamV_NWW$Tilt)
GamV_NWW$Discharge <- as.numeric(GamV_NWW$Discharge)
GamV_NWW$Noise <- as.numeric(GamV_NWW$Noise)
GamV_NWW$Waterheight <- as.numeric(GamV_NWW$Waterheight)

# 1st try
GAM_NWW <- mgcViz::gamV(list(DE ~ s(Windspeed, k = 6) + Wind_Direction + s(Temp_rec, k = 6) + s(Tilt, k = 6) + s(Discharge, k = 6) + s(Noise, k = 6) + s(Waterheight, k = 6)),
                        ~s(Windspeed, k = 6) + Wind_Direction + s(Temp_rec, k = 6) + s(Tilt, k = 6) +  s(Discharge, k = 6) + s(Noise, k = 6) + s(Waterheight, k = 6),
                        family = multinom(K = 7),  data =  subset(Env_DE_HVS_NWW_total, ID == "NW2_NW3"))

# 2d try
GAM_NWW <- mgcv::gam(list(DE ~ s(Windspeed, k=6) + Wind_Direction + s(Temp_rec, k=6) + s(Tilt, k=6) + s(Discharge, k=6) + s(Noise, k=6) + s(Waterheight, k=6)),
                     ~s(ID, bs = 're')+s(Windspeed, k=6) + Wind_Direction + s(Temp_rec, k=6) + s(Tilt, k=6) +  s(Discharge, k=6) + s(Noise, k=6) + s(Waterheight, k=6),
                     family = mgcv::multinom(K=7),  data =  GamV_NWW)


# 3d try. Handle NAs 
Gam_NWW_DE_NA <- which(is.na(GamV_NWW$DE)) # Detection efficiency
print(Gam_NWW_DE_NA)
Gam_NWW_Windspeed_NA <- which(is.na(GamV_NWW$Windspeed)) # Windspeed
print(Gam_NWW_Windspeed_NA)
Gam_NWW_Wind_Direction_NA <- which(is.na(GamV_NWW$Wind_Direction)) # Wind Direction
print(Gam_NWW_Wind_Direction_NA)
Gam_NWW_Temp_rec_NA <- which(is.na(GamV_NWW$Temp_rec)) # Temperature
print(Gam_NWW_Temp_rec_NA)
Gam_NWW_Tilt_NA <- which(is.na(GamV_NWW$Tilt)) # Tilt
print(Gam_NWW_Tilt_NA)
Gam_NWW_Discharge_NA <- which(is.na(GamV_NWW$Discharge)) # Discharge
Gam_NWW_Discharge_NA_count <- sum(is.na(GamV_NWW$Discharge)) 
print(Gam_NWW_Discharge_NA) #  Discharge. Seems to be the only one with NAs. 
print(Gam_NWW_Discharge_NA_count) # 14 NAs for discharge NWW total
Gam_NWW_Noise_NA <- which(is.na(GamV_NWW$Noise)) # Noise
print(Gam_NWW_Noise_NA)  
Gam_NWW_Waterheight_NA <- which(is.na(GamV_NWW$Waterheight)) # Waterheight
print(Gam_NWW_Waterheight_NA)

# Remove rows with discharge is NA and insert row IDs as a column 
GamV_NWW <- GamV_NWW[complete.cases(GamV_NWW$Discharge), ]
GamV_NWW$ID_row <- seq_len(nrow(GamV_NWW))
GamV_NWW <- GamV_NWW %>%
  dplyr::select("ID_row", everything())

formula1 <- DE ~ s(Windspeed, k = 6) + Wind_Direction + s(Temp_rec, k = 6) + s(Tilt, k = 6) + s(Discharge, k = 6) + s(Noise, k = 6) + s(Waterheight, k = 6)
formula_simple <- DE ~ s(GamV_NWW$Windspeed, k = 9)
GAM_NWW <- mgcv::gam((list(GamV_NWW$DE ~ GamV_NWW$Windspeed)), family = mgcv::multinom(K = 6), data = GamV_NWW) # Try gam

GLM_NWW <- glm(list(GamV_NWW$DE~ GamV_NWW$Windspeed), family = mgcv::multinom(K = 6), data = GamV_NWW) # Try glm 
GLM_NWW <- glm(DE ~ Windspeed + Temp_rec, factorial, data=GamV_NWW) 



formula1 <- DE ~  s(Windspeed, k = 6) + Wind_Direction + s(Temp_rec, k = 6) + s(Tilt, k = 6) + s(Discharge, k = 6) + s(Noise, k = 6) + s(Waterheight, k = 6)
multinom_model <- multinom(DE ~ Windspeed + Temp_rec, data = GamV_NWW) # First try with just nnet, not smoothing functions yet
gam_windspeed <- mgcv::gam(DE ~ s(ID_row, bs = 're')+ s(Temp_rec, k = 6), data = GamV_NWW)

formula1_1 <- (DE ~  s(Windspeed, k = 6) + s(Temp_rec, k = 6))
GAM_NWW <- mgcv::gam(formula = formula1_1, family = mgcv::multinom(K = 7), data = GamV_NWW)


count_NA_Windspeed_NWW <- sum(is.na(ML_NWW_DE$Windspeed))
count_NA_Wind_Direction_NWW <- sum(is.na(ML_NWW_DE$Wind_Direction))
count_NA_Temp_NWW <- sum(is.na(ML_NWW_DE$Temp_rec))
count_NA_Tilt_NWW <- sum(is.na(ML_NWW_DE$Tilt))
count_NA_Discharge_NWW <- sum(is.na(ML_NWW_DE$Discharge))
count_NA_Noise_NWW <- sum(is.na(ML_NWW_DE$Noise))
count_NA_Waterheight_NWW <- sum(is.na(ML_NWW_DE$Waterheight)) #  NAs for discharge after 2023-12-31 08:00:00

Discharge_limit_ML_NWW_DE <- as.POSIXct("2023-12-31 08:00:00", tz = "UTC" )
ML_NWW_DE <- subset(ML_NWW_DE, DateTime_UTC < Discharge_limit_ML_NWW_DE)

summary(GAM_NWW)

# Multinom model (form nnet package)

# NWW multinomial
ML_NWW_DE <- subset(Env_DE_HVS_NWW_total, ID == "NW2_NW3")
ML_NWW_DE$DE <- as.factor(ML_NWW_DE$DE)
ML_NWW_DE$Windspeed <- as.numeric(ML_NWW_DE$Windspeed)
ML_NWW_DE$Wind_Direction <- as.factor(ML_NWW_DE$Wind_Direction)
ML_NWW_DE$Temp_rec <- as.numeric(ML_NWW_DE$Temp_rec)

# Renumber the DE levels: use levels 0 - 6 for PDP plots later on 
ML_NWW_DE <- ML_NWW_DE %>%
  mutate(DE = recode(DE, `0` = 1, `16.7` = 2, `33.3` = 3, `50` = 4, `66.7` = 5, `83.3` = 6, `100` = 7))
ML_NWW_DE$DE <- factor(ML_NWW_DE$DE, levels = c("1", "2", "3", "4", "5", "6", "7")) # Make sure that order of DE goes down from 100 to 0 for later plots

# Check the correlation between windspeed and temp. The plot does not indicate clear seasonal relation with sluice opening
ggplot(ML_NWW_DE, aes(x = Windspeed, y = Temp_rec)) +
  geom_point() 
ggplot(ML_NWW_DE, aes(x = abs(Discharge), y = Tilt)) +
  geom_point() 
ggplot(ML_NWW_DE, aes(x = Waterheight, y = Discharge)) +
  geom_point() 

# Make summary table of the predictor variables
ML_NWW_DE$DE <- as.factor(ML_NWW_DE$DE)
with(ML_NWW_DE, table(Wind_Direction, DE))
with(ML_NWW_DE, do.call(rbind, tapply(Windspeed, DE, function(x) c(M = mean(x), SD = sd(x)))))
with(ML_NWW_DE, do.call(rbind, tapply(Temp_rec, DE, function(x) c(M = mean(x), SD = sd(x)))))
ML_NWW_DE$DE_2 <- relevel(ML_NWW_DE$DE, ref = "7") # Set reference DE category

# Put variables in multinomial logistical regression model to see which model is the best fit
ML_NWW_DE_TEST_windS_temp <- nnet::multinom(DE_2 ~ Windspeed * Temp_rec, data = ML_NWW_DE) # AIC 22993.15 
ML_NWW_DE_TEST_windSD_temp <- nnet::multinom(DE_2 ~ Windspeed * Wind_Direction + Windspeed * Temp_rec, data = ML_NWW_DE) # AIC 22842.75
ML_NWW_DE_TEST_windSD_temp_tilt <- nnet::multinom(DE_2 ~ Windspeed * Wind_Direction + Windspeed * Temp_rec + Tilt, data = ML_NWW_DE) # AIC 22738.67
ML_NWW_DE_TEST_windSD_temp_tilt_discharge <- nnet::multinom(DE_2 ~ Windspeed + Windspeed * Wind_Direction + Temp_rec + Windspeed * Temp_rec + Tilt + Tilt*abs(Discharge), data = ML_NWW_DE) # AIC 22528.38.67
ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise <- nnet::multinom(DE_2 ~ Windspeed + Windspeed * Wind_Direction + Temp_rec + Windspeed * Temp_rec + Tilt + Tilt*abs(Discharge)+Noise, data = ML_NWW_DE) # AIC 22330.
ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH <- nnet::multinom(DE_2 ~  Windspeed * Wind_Direction + Temp_rec + Tilt*abs(Discharge) + Noise*abs(Discharge)  + Waterheight * Discharge, data = ML_NWW_DE) # AIC 21760.

# Summarize the model and perform significance test obtaining the P value
summary(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH)
z <- summary(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH)$coefficients/summary(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(test))

# Try to visualize the relationship between predictor variable and response variabale by visualisation 

# Method 1: PDP simple line plot 

# Partial dependence plot (PDP).  
partial_NWW_Windspeed <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Windspeed")
#plot(partial_NWW_Windspeed)
partial_NWW_Windspeed_df <- as.data.frame(partial_NWW_Windspeed)
partial_NWW_Windspeed_plot <- ggplot(partial_NWW_Windspeed_df, aes(x = Windspeed, y = yhat)) +
  geom_line() +
  xlab("Windspeed (m/s)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE NW2-NW3 category over windspeed")
plot(partial_NWW_Windspeed_plot) 
partial_NWW_Discharge <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Discharge")
#plot(partial_NWW_Discharge)
partial_NWW_Discharge_df <- as.data.frame(partial_NWW_Discharge)
partial_NWW_Discharge_plot <- ggplot(partial_NWW_Discharge_df, aes(x = Discharge, y = yhat)) +
  geom_line() +
  xlab("Discharge (m3/s)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE NW2-NW3 category over discharge")
#plot(partial_NWW_Discharge_plot)
partial_NWW_Waterheight <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Waterheight")
#plot(partial_NWW_Waterheight)
partial_NWW_Waterheight_df <- as.data.frame(partial_NWW_Waterheight)
partial_NWW_Waterheight_plot <- ggplot(partial_NWW_Waterheight_df, aes(x = Waterheight, y = yhat)) +
  geom_line() +
  xlab("Waterheight (cm)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE NW2-NW3 category over waterheight")
#plot(partial_NWW_Waterheight_plot)
partial_NWW_Temperature <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Temp_rec")
#plot(partial_NWW_Temperature)
partial_NWW_Temperature_df <- as.data.frame(partial_NWW_Temperature)
partial_NWW_Temperature_plot <- ggplot(partial_NWW_Temperature_df, aes(x = Temp_rec, y = yhat)) +
  geom_line() +
  xlab("Temperature (Degrees Celcius)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE NW2-NW3 category over temperature")
#plot(partial_NWW_Temperature_plot)
partial_NWW_Noise <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Noise")
#plot(partial_NWW_Noise)
partial_NWW_Noise_df <- as.data.frame(partial_NWW_Noise)
partial_NWW_Noise_plot <- ggplot(partial_NWW_Noise_df, aes(x = Noise, y = yhat)) +
  geom_line() +
  xlab("Noise (mV)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE NW2-NW3 category over  noise")
#plot(partial_NWW_Noise_plot)
partial_NWW_Tilt <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Tilt")
#plot(partial_NWW_Tilt)
partial_NWW_Tilt_df <- as.data.frame(partial_NWW_Tilt)
partial_NWW_Tilt_plot <- ggplot(partial_NWW_Tilt_df, aes(x = Tilt, y = yhat)) +
  geom_line() +
  xlab("Tilt (Degrees)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE NW2-NW3 category over  tilt")
plot(partial_NWW_Tilt_plot)

# Create complete PDP for NWW
PDP_DE_NW2_NW3_Complete <- grid.arrange(partial_NWW_Windspeed_plot,  partial_NWW_Discharge_plot, partial_NWW_Waterheight_plot, partial_NWW_Temperature_plot, partial_NWW_Noise_plot, partial_NWW_Tilt_plot, ncol = 3, nrow=2)
plot(PDP_DE_NW2_NW3_Complete)
# Method 2: PDP line plot for DE category 

# Create dataframe with environmental variables for each DE category based on the multinom model
partial_combined_NWW <- do.call(rbind, lapply(1:7, function(cat) {
  cat_name <- paste0("class", cat)
  plot <- partial(ML_NWW_DE_TEST_windSD_temp_tilt_discharge_noise_WH, pred.var = "Temp_rec", which.class = cat, prob = TRUE)
  df <- as.data.frame(plot)
  if (nrow(df) > 0) {df$category <- cat 
  return(df)}
  return(NULL)}))

# Plot the partial dependence lineplot. Change the env. variable every time to create a total figure with all the graphs
colors <- c("blue", "orange", "green", "red", "purple", "brown", "black")
partial_NWW_Temp_rec_plot <- ggplot(partial_combined_NWW, aes(x = Temp_rec, y = yhat, color = factor(category))) +
  geom_line() +
  xlab("Temperature (Degrees Celcius)") +  
  ylab("Predicted Probability") +   
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over temperature in NWW") +
  scale_color_manual(name = "DE Category", values = colors) +
  theme(plot.title = element_text(size = 13, hjust = 0.3, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))+
  ylim(0, 1)
plot(partial_NWW_Temp_rec_plot)

# List all the plots you just made
partial_NWW_Windspeed_plot
partial_NWW_Discharge_plot
partial_NWW_Waterheight_plot
partial_NWW_Temp_rec_plot
partial_NWW_Noise_plot
partial_NWW_Tilt_plot

# Create complete PDP for NWW
PDP_DE_NW2_NW3_Complete_LinePlot <- grid.arrange(partial_NWW_Windspeed_plot,  partial_NWW_Discharge_plot, partial_NWW_Waterheight_plot, partial_NWW_Temp_rec_plot, partial_NWW_Noise_plot, partial_NWW_Tilt_plot, ncol = 3, nrow=2)
plot(PDP_DE_NW2_NW3_Complete_LinePlot)



#!!!!!!! MAYBE USE LATER SECTION !!!!!!!#

# ANALYSE THE PROBABILITIES FOR EACH DE CATEGORY

# Create models containing only the predictor variable(s) of choice. Otherwise it will not work 
ML_NWW_DE$Tilt <- as.numeric(ML_NWW_DE$Tilt )
ML_NWW_DE_WindSD <- nnet::multinom(DE_2 ~ Windspeed * Wind_Direction, data = ML_NWW_DE) # AIC 22993.15 
ML_NWW_DE_Discharge <- nnet::multinom(DE_2 ~ Discharge, data = ML_NWW_DE) # AIC 22993.15 
ML_NWW_DE_Waterheight <- nnet::multinom(DE_2 ~ Waterheight, data = ML_NWW_DE) # AIC 22993.15 
ML_NWW_DE_Temperature <- nnet::multinom(DE_2 ~ Temp_rec, data = ML_NWW_DE) # AIC 22993.15 
ML_NWW_DE_Noise <- nnet::multinom(DE_2 ~ Noise, data = ML_NWW_DE) # AIC 22993.15 
ML_NWW_DE_Tilt <- nnet::multinom(DE_2 ~ Tilt, data = ML_NWW_DE) # AIC 22993.15 

# Calculate and plot the average predicted probabilities over the continuous variable of interest

# Wind
head(pp <- fitted(ML_NWW_DE_WindSD))
Wind_D <- data.frame(Wind_Direction = c("North", "East", "South", "West"), Windspeed = mean(ML_NWW_DE$Windspeed))
predict(ML_NWW_DE_WindSD, newdata = Wind_D, "probs")

# Calculate average predicted probabilities over the different values of continuous variable 
Wind_SD <- data.frame(Wind_Direction = rep(c("North", "East", "South", "West"), each = 21), Windspeed = rep(c(0:20),
                                                                                                            4))
Average_probs_WD <- cbind(Wind_SD, predict(ML_NWW_DE_TEST_Wind, newdata = Wind_SD, type = "probs", se = TRUE))
by(Average_probs_WD[, 3:9], Average_probs_WD$Wind_Direction, colMeans)

# Plot the average probabilities
WD_props_plot <- melt(Average_probs_WD, id.vars = c("Wind_Direction", "Windspeed"), value.name = "probability")
head(WD_props_plot) 

WD_props_plot$Wind_Direction <- factor(WD_props_plot$Wind_Direction, 
                                       levels = c("North", "East", "South", "West"),
                                       ordered = TRUE)

WindSD_props_PLOT <- ggplot(WD_props_plot, aes(x = Windspeed, y = probability, colour = Wind_Direction)) + geom_line() + facet_grid(variable ~
                                                                                                                                      ., scales = "free")

# Discharge
Discharge_bins <- data.frame(Discharge = rep(seq(0, 6000, by = 500), each = 12))
Average_probs_Discharge <- cbind(Discharge_bins, predict(ML_NWW_DE_Discharge, newdata = Discharge_bins, type = "probs", se = TRUE))
by(Average_probs_Discharge[, 2:8], Average_probs_Discharge$Discharge, colMeans)

Discharge_props_plot <- melt(Average_probs_Discharge, id.vars = c("Discharge"), value.name = "probability")
head(Discharge_props_plot) 

Discharge_props_PLOT <- ggplot(Discharge_props_plot, aes(x = Discharge, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                                       ., scales = "free")

# Waterheight
Waterheight_bins <- data.frame(Waterheight = rep(seq(-500, 500, by = 50), each = 20))
Average_probs_Waterheight <- cbind(Waterheight_bins, predict(ML_NWW_DE_Waterheight, newdata = Waterheight_bins, type = "probs", se = TRUE))
by(Average_probs_Waterheight[, 2:8], Average_probs_Waterheight$Waterheight, colMeans)

Waterheight_props_plot <- melt(Average_probs_Waterheight, id.vars = c("Waterheight"), value.name = "probability")
head(Waterheight_props_plot) 

Waterheight_props_PLOT <- ggplot(Waterheight_props_plot, aes(x = Waterheight, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                                             ., scales = "free")

# Temperature
Temp_bins <- data.frame(Temp_rec = rep(seq(0, 20, by = 2), each = 10))
Average_probs_Temp <- cbind(Temp_bins, predict(ML_NWW_DE_Temperature, newdata = Temp_bins, type = "probs", se = TRUE))
by(Average_probs_Temp[, 2:8], Average_probs_Temp$Temp_rec, colMeans)

Temp_props_plot <- melt(Average_probs_Temp, id.vars = c("Temp_rec"), value.name = "probability")
head(Average_probs_Temp) 

Temp_props_PLOT <- ggplot(Temp_props_plot, aes(x = Temp_rec, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                            ., scales = "free")

# Noise
Noise_bins <- data.frame(Noise = rep(seq(0, 700, by = 100), each = 7))
Average_probs_Noise <- cbind(Noise_bins, predict(ML_NWW_DE_Noise, newdata = Noise_bins, type = "probs", se = TRUE))
by(Average_probs_Noise[, 2:8], Average_probs_Noise$Noise, colMeans)

Noise_props_plot <- melt(Average_probs_Noise, id.vars = c("Noise"), value.name = "probability")
head(Average_probs_Noise) 

Noise_props_PLOT <- ggplot(Noise_props_plot, aes(x = Noise, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                           ., scales = "free")

# Tilt
Tilt_bins <- data.frame(Tilt = rep(seq(0, 70, by = 10), each = 7))
Average_probs_Tilt <- cbind(Tilt_bins, predict(ML_NWW_DE_Tilt, newdata = Tilt_bins, type = "probs", se = TRUE))
by(Average_probs_Tilt[, 2:8], Average_probs_Tilt$Tilt, colMeans)

Tilt_props_plot <- melt(Average_probs_Tilt, id.vars = c("Tilt"), value.name = "probability")
head(Average_probs_Tilt) 

Tilt_props_PLOT <- ggplot(Tilt_props_plot, aes(x = Tilt, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                        ., scales = "free")

# Create complete Probability plot figure for NWW
Probs_DE_NW2_NW3_Complete <- grid.arrange(WindSD_props_PLOT,  Discharge_props_PLOT, Waterheight_props_PLOT, Temp_props_PLOT, Noise_props_PLOT, Tilt_props_PLOT, ncol = 3, nrow=2)


# HVS sea

# HVS sea multinomial logistic regression
ML_HVSs_DE <- subset(Env_DE_HVS_NWW_total, ID == "HS6_HS8")
ML_HVSs_DE$DE <- as.factor(ML_HVSs_DE$DE)
ML_HVSs_DE$Windspeed <- as.numeric(ML_HVSs_DE$Windspeed)
ML_HVSs_DE$Wind_Direction <- as.factor(ML_HVSs_DE$Wind_Direction)
ML_HVSs_DE$Discharge <- as.numeric(ML_HVSs_DE$Discharge)
ML_HVSs_DE$Waterheight <- as.numeric(ML_HVSs_DE$Waterheight)
ML_HVSs_DE$Sluice_Opening <- as.numeric(ML_HVSs_DE$Sluice_Opening)
ML_HVSs_DE$Spui_Kier_Closed <- as.factor(ML_HVSs_DE$Spui_Kier_Closed)
ML_HVSs_DE$Temp_rec <- as.numeric(ML_HVSs_DE$Temp_rec)
ML_HVSs_DE$Chloride <- as.numeric(ML_HVSs_DE$Chloride)
ML_HVSs_DE$Tilt <- as.numeric(ML_HVSs_DE$Tilt)
ML_HVSs_DE$Noise <- as.numeric(ML_HVSs_DE$Noise)
ML_HVSs_DE$DE <- factor(ML_HVSs_DE$DE, levels = c("100", "83.3", "66.7", "50", "33.3", "16.7", "0"))

# Renumber the DE levels: use levels 1 - 7 for PDP plots later on 
ML_HVSs_DE <- ML_HVSs_DE %>%
  mutate(DE = recode(DE, `0` = 1, `16.7` = 2, `33.3` = 3, `50` = 4, `66.7` = 5, `83.3` = 6, `100` = 7))
ML_HVSs_DE$DE <- factor(ML_HVSs_DE$DE, levels = c("1", "2", "3", "4", "5", "6", "7")) # Make sure that order of DE goes down from 100 to 0 for later plots

# Check the correlation between windspeed and temp. Unclear if there is a seasonal relation from the plot
ggplot(ML_HVSs_DE, aes(x = Windspeed, y = Discharge)) +
  geom_point() 
ggplot(ML_HVSs_DE, aes(x = Waterheight, y = Discharge)) +
  geom_point() 
ggplot(ML_HVSs_DE, aes(x = Sluice_Opening, y = Discharge)) +
  geom_point() 
ggplot(ML_HVSs_DE, aes(x = Sluice_Opening, y = Waterheight)) +
  geom_point() 
ggplot(ML_HVSs_DE, aes(x = Sluice_Opening, y = Temperature)) +
  geom_point
ggplot(ML_HVSs_DE, aes(x = Sluice_Opening, y = Chloride)) +
  geom_point() 
ggplot(ML_HVSs_DE, aes(x = Sluice_Opening, y = Tilt)) +
  geom_point()
ggplot(ML_HVSs_DE, aes(x = Sluice_Opening, y = Noise)) +
  geom_point()
ggplot(ML_HVSs_DE, aes(x = Windspeed, y = Temp_rec)) +
  geom_point() 
ggplot(ML_HVSs_DE, aes(x = Windspeed , y = Noise)) +
  geom_point()
ggplot(ML_HVSs_DE, aes(x = Windspeed , y = Tilt)) +
  geom_point()

# Make summary table of the predictor variables
with(ML_HVSs_DE, table(Wind_Direction, DE))
with(ML_HVSs_DE, do.call(rbind, tapply(Windspeed, DE, function(x) c(M = mean(x), SD = sd(x)))))
with(ML_HVSs_DE, do.call(rbind, tapply(Temp_rec, DE, function(x) c(M = mean(x), SD = sd(x)))))
ML_HVSs_DE$DE_2 <- relevel(ML_HVSs_DE$DE, ref = "7") # Set reference DE category

# Put variables in multinomial logistical regression model to see which model is the best fit
ML_HVSs_DE_windS<- nnet::multinom(DE_2 ~ Windspeed, data = ML_HVSs_DE) # AIC 24287.27
ML_HVSs_DE_windSD<- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction, data = ML_HVSs_DE) # AIC 23918.7
ML_HVSs_DE_windSD_WH <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Waterheight, data = ML_HVSs_DE) # AIC 23841.59 
ML_HVSs_DE_windSD_WH_SO <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Sluice_Opening*Waterheight , data = ML_HVSs_DE) # AIC 23773.89
ML_HVSs_DE_windSD_WH_SO_SKC <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed, data = ML_HVSs_DE) # AIC 23536.04
ML_HVSs_DE_windSD_WH_SO_SKC_Temp <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec, data = ML_HVSs_DE) # AIC 23159.94 
ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec + Sluice_Opening*Chloride, data = ML_HVSs_DE) # AIC 21751.03  
ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec + Sluice_Opening*Chloride + Sluice_Opening*Noise , data = ML_HVSs_DE) # AIC 21651.76  
#ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise_Tilt <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec + Sluice_Opening*Chloride + Sluice_Opening*Noise + Sluice_Opening*Tilt, data = ML_HVSs_DE) # AIC 22225.66 

# Don't include tilt as a factor. Did not add significantly to the model. Correlated with sluice opening and wind speed though. 
# Did look at noise and seemed to have quite some effect still. Therefore I think we should include it. 

# Summarize the model and perform significance test obtaining the P value
summary(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise)
z <- summary(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise)$coefficients/summary(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(test))

# Try to visualize the relationship between predictor variable and response variabale by visualisation 

# Partial dependence plot (PDP).  

# Method 1: PDP simple line plot 
partial_HVSs_Windspeed <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Windspeed")
plot(partial_HVSs_Windspeed)
partial_HVSs_Windspeed_df <- as.data.frame(partial_HVSs_Windspeed)
partial_HVSs_Windspeed_plot <- ggplot(partial_HVSs_Windspeed_df, aes(x = Windspeed, y = yhat)) +
  geom_line() +
  xlab("Windspeed (m/s)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over windspeed")
#plot(partial_HVSs_Windspeed_plot) 
partial_HVSs_SO <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Sluice_Opening") # I don't get this one.
#plot(partial_HVSs_SO)
partial_HVSs_SO_df <- as.data.frame(partial_HVSs_SO)
partial_HVSs_SO_plot <- ggplot(partial_HVSs_SO_df, aes(x = Sluice_Opening, y = yhat)) +
  geom_line() +
  xlab("Sluice opening (m2)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over Sluice opening")
#plot(partial_HVSs_SO_plot)
partial_HVSs_Waterheight <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Waterheight")
#plot(partial_HVSs_Waterheight)
partial_HVSs_Waterheight_df <- as.data.frame(partial_HVSs_Waterheight)
partial_HVSs_Waterheight_plot <- ggplot(partial_HVSs_Waterheight_df, aes(x = Waterheight, y = yhat)) +
  geom_line() +
  xlab("Waterheight (cm)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over waterheight")
#plot(partial_HVSs_Waterheight_plot)
partial_HVSs_Temperature <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Temp_rec")
#plot(partial_HVSs_Temperature)
partial_HVSs_Temperature_df <- as.data.frame(partial_HVSs_Temperature)
partial_HVSs_Temperature_plot <- ggplot(partial_HVSs_Temperature_df, aes(x = Temp_rec, y = yhat)) +
  geom_line() +
  xlab("Temperature (Degrees Celcius)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over temperature")
#plot(partial_HVSs_Temperature_plot)
partial_HVSs_Chloride <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Chloride")
#plot(partial_HVSs_Chloride)
partial_HVSs_Chloride_df <- as.data.frame(partial_HVSs_Chloride)
partial_HVSs_Chloride_plot <- ggplot(partial_HVSs_Chloride_df, aes(x = Chloride, y = yhat)) +
  geom_line() +
  xlab("Chloride (mg / L)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over Chloride")
plot(partial_HVSs_Chloride_plot)
partial_HVSs_Noise <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Noise")
#plot(partial_HVSs_Noise)
partial_HVSs_Noise_df <- as.data.frame(partial_HVSs_Noise)
partial_HVSs_Noise_plot <- ggplot(partial_HVSs_Noise_df, aes(x = Noise, y = yhat)) +
  geom_line() +
  xlab("Noise (mV)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over noise")
plot(partial_HVSs_Noise_plot)
partial_HVSs_Tilt <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Tilt")
#plot(partial_HVSs_Tilt)
partial_HVSs_Tilt_df <- as.data.frame(partial_HVSs_Tilt)
partial_HVSs_Tilt_plot <- ggplot(partial_HVSs_Tilt_df, aes(x = Tilt, y = yhat)) +
  geom_line() +
  xlab("Tilt (Degrees)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE HS6-HS8 category over tilt")
plot(partial_HVSs_Tilt_plot)

# Create complete Partial dependence plot figure for NWW
PDP_DE_HS6_HS8_Complete <- grid.arrange(partial_HVSs_Windspeed_plot, partial_HVSs_SO_plot ,partial_HVSs_Waterheight_plot, partial_HVSs_Temperature_plot, partial_HVSs_Chloride_plot, partial_HVSs_Noise_plot ,ncol = 3, nrow=2)

# Method 2: PDP line plot for DE category 

# Create dataframe with environmental variables for each DE category based on the multinom model
partial_combined_HVSs <- do.call(rbind, lapply(1:7, function(cat) {
  cat_name <- paste0("class", cat)
  plot <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Noise", which.class = cat, prob = TRUE)
  df <- as.data.frame(plot)
  if (nrow(df) > 0) {df$category <- cat 
  return(df)}
  return(NULL)}))

# Plot the partial dependence lineplot. Change the env. variable every time to create a total figure with all the graphs
colors <- c("blue", "orange", "green", "red", "purple", "brown", "black")
partial_HVSs_Noise_plot <- ggplot(partial_combined_HVSs, aes(x = Noise, y = yhat, color = factor(category))) +
  geom_line() +
  xlab("Noise (mV)") +  
  ylab("Predicted Probability") +   
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over noise in HVS sea") +
  scale_color_manual(name = "DE Category", values = colors) +
  theme(plot.title = element_text(size = 13, hjust = 0.3, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))+
  ylim(0, 1)
plot(partial_HVSs_Noise_plot)

# List all the plots you just made
partial_HVSs_Windspeed_plot
partial_HVSs_Sluice_Opening_plot
partial_HVSs_Waterheight_plot
partial_HVSs_Temp_rec_plot
partial_HVSs_Chloride_plot
partial_HVSs_Noise_plot

# Create complete PDP for HVSsea
PDP_DE_HS6_HS8_Complete_LinePlot <- grid.arrange(partial_HVSs_Windspeed_plot,  partial_HVSs_Sluice_Opening_plot, partial_HVSs_Waterheight_plot, partial_HVSs_Temp_rec_plot, partial_HVSs_Chloride_plot, partial_HVSs_Noise_plot, ncol = 3, nrow=2)
plot(PDP_DE_HS6_HS8_Complete_LinePlot)

# !!!!!!!!! MAYBE USE LATER SECTION !!!!!!# 

# ANALYSE THE PROBABILITIES FOR EACH DE CATEGORY

# Create models containing only the predictor variable(s) of choice. Otherwise it will not work 
ML_HVSs_DE$DE_2 <- relevel(ML_HVSs_DE$DE, ref = "100") # Set reference DE category
ML_HVSs_DE_WindSD <- nnet::multinom(DE_2 ~ Windspeed * Wind_Direction, data = ML_HVSs_DE) # AIC 22993.15 
ML_HVSs_DE_SO <- nnet::multinom(DE_2 ~ Sluice_Opening*Spui_Kier_Closed, data = ML_HVSs_DE) # AIC 22993.15 
ML_HVSs_DE_Waterheight <- nnet::multinom(DE_2 ~ Waterheight, data = ML_HVSs_DE) # AIC 22993.15 
ML_HVSs_DE_Temperature <- nnet::multinom(DE_2 ~ Temp_rec, data = ML_HVSs_DE) # AIC 22993.15 
ML_HVSs_DE_Chloride <- nnet::multinom(DE_2 ~ Chloride, data = ML_HVSs_DE) # AIC 22993.15 
ML_HVSs_DE_Noise <- nnet::multinom(DE_2 ~ Noise, data = ML_HVSs_DE) # AIC 22993.15 
ML_HVSs_DE_Tilt <- nnet::multinom(DE_2 ~ Tilt, data = ML_HVSs_DE) # AIC 22993.15 

# Calculate and plot the average predicted probabilities over the continuous variable of interest

# Wind. THIS ONE IS NOT WORKING WELL YET. WHY?
head(pp <- fitted(ML_HVSs_DE_WindSD))
Wind_S <- data.frame(Wind_Direction = c("North", "East", "South", "West"), Windspeed = mean(ML_HVSs_DE$Windspeed))
predict(ML_HVSs_DE_WindSD, newdata = Wind_S, "probs")

# Calculate average predicted probabilities over the different values of continuous variable 
Wind_SD <- data.frame(Wind_Direction = rep(c("North", "East", "South", "West"), each = 21), Windspeed = rep(c(0:20),
                                                                                                            4))
Average_probs_WD <- cbind(Wind_SD, predict(ML_HVSs_DE_WindSD, newdata = Wind_SD, type = "probs", se = TRUE))
by(Average_probs_WD[, 3:9], Average_probs_WD$Wind_Direction, colMeans)

# Plot the average probabilities
WD_props_plot <- melt(Average_probs_WD, id.vars = c("Wind_Direction", "Windspeed"), value.name = "probability")
head(WD_props_plot) 

WD_props_plot$Wind_Direction <- factor(WD_props_plot$Wind_Direction, 
                                       levels = c("North", "East", "South", "West"),
                                       ordered = TRUE)

WindSD_props_PLOT <- ggplot(WD_props_plot, aes(x = Windspeed, y = probability, colour = Wind_Direction)) + geom_line() + facet_grid(variable ~
                                                                                                                                      ., scales = "free")
# Sluice Opening
head(pp <- fitted(ML_HVSs_DE_SO))
SO_HVSs <- data.frame(Spui_Kier_Closed = c("Spui", "Kier"), Sluice_Opening = mean(ML_HVSs_DE$Sluice_Opening))
predict(ML_HVSs_DE_SO, newdata = SO_HVSs, "probs")

# Determine max sluice opening during kieren
max_sluice_opening_kier <- max(ML_HVSs_DE$Sluice_Opening[ML_HVSs_DE$Spui_Kier_Closed == "Kier"])
print(max_sluice_opening_kier)

# Create bins for sluice_opening and link to 
sluice_opening_values <- seq(0, 800, by = 50)

# Create all combinations of Spui_Kier_Closed and Sluice_Opening
combinations <- expand.grid(Spui_Kier_Closed = c("Spui", "Kier"),
                            Sluice_Opening = sluice_opening_values)

# Create the data frame
SO_HVSs_SKC <- combinations[rep(seq_len(nrow(combinations)), each = 20), ]

# Calculate predictions and probabilities
Average_probs_SKC <- cbind(SO_HVSs_SKC, predict(ML_HVSs_DE_SO, newdata = SO_HVSs_SKC, type = "probs", se = TRUE))

# Calculate column means by Spui_Kier_Closed
by(Average_probs_SKC[, 3:9], Average_probs_SKC$Spui_Kier_Closed, colMeans)

# Plot the average probabilities
SKC_props_plot <- melt(Average_probs_SKC, id.vars = c("Spui_Kier_Closed", "Sluice_Opening"), value.name = "probability")
head(SKC_props_plot) 

SKC_props_plot$Spui_Kier_Closed <- factor(SKC_props_plot$Spui_Kier_Closed, 
                                          levels = c("Spui", "Kier"),
                                          ordered = TRUE)
SKC_props_PLOT <- ggplot(SKC_props_plot, aes(x = Sluice_Opening, y = probability, colour = Spui_Kier_Closed)) + geom_line() + facet_grid(variable ~                                                                                                                                 ., scales = "free")

# Waterheight
Waterheight_bins <- data.frame(Waterheight = rep(seq(-200, 200, by = 20), each = 20))
Average_probs_Waterheight <- cbind(Waterheight_bins, predict(ML_HVSs_DE_Waterheight, newdata = Waterheight_bins, type = "probs", se = TRUE))
by(Average_probs_Waterheight[, 2:8], Average_probs_Waterheight$Waterheight, colMeans)

Waterheight_props_plot <- melt(Average_probs_Waterheight, id.vars = c("Waterheight"), value.name = "probability")
head(Waterheight_props_plot) 

Waterheight_props_PLOT <- ggplot(Waterheight_props_plot, aes(x = Waterheight, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                                             ., scales = "free")

# Temperature
Temp_bins <- data.frame(Temp_rec = rep(seq(0, 24, by = 2), each = 10))
Average_probs_Temp <- cbind(Temp_bins, predict(ML_HVSs_DE_Temperature, newdata = Temp_bins, type = "probs", se = TRUE))
by(Average_probs_Temp[, 2:8], Average_probs_Temp$Temp_rec, colMeans)

Temp_props_plot <- melt(Average_probs_Temp, id.vars = c("Temp_rec"), value.name = "probability")
head(Average_probs_Temp) 

Temp_props_PLOT <- ggplot(Temp_props_plot, aes(x = Temp_rec, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                            ., scales = "free")
# Noise
Noise_bins <- data.frame(Noise = rep(seq(0, 700, by = 100), each = 7))
Average_probs_Noise <- cbind(Noise_bins, predict(ML_HVSs_DE_Noise, newdata = Noise_bins, type = "probs", se = TRUE))
by(Average_probs_Noise[, 2:8], Average_probs_Noise$Noise, colMeans)

Noise_props_plot <- melt(Average_probs_Noise, id.vars = c("Noise"), value.name = "probability")
head(Average_probs_Noise) 

Noise_props_PLOT <- ggplot(Noise_props_plot, aes(x = Noise, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                           ., scales = "free")

# Chloride
Chloride_bins <- data.frame(Chloride = rep(seq(0, 15000, by = 1000), each = 7))
Average_probs_Chloride <- cbind(Chloride_bins, predict(ML_HVSs_DE_Chloride, newdata = Chloride_bins, type = "probs", se = TRUE))
by(Average_probs_Chloride[, 2:8], Average_probs_Chloride$Chloride, colMeans)

Chloride_props_plot <- melt(Average_probs_Chloride, id.vars = c("Chloride"), value.name = "probability")
head(Average_probs_Chloride) 

Chloride_props_PLOT <- ggplot(Chloride_props_plot, aes(x = Chloride, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                                    ., scales = "free")

# Tilt
Tilt_bins <- data.frame(Tilt = rep(seq(0, 70, by = 10), each = 7))
Average_probs_Tilt <- cbind(Tilt_bins, predict(ML_HVSs_DE_Tilt, newdata = Tilt_bins, type = "probs", se = TRUE))
by(Average_probs_Tilt[, 2:8], Average_probs_Tilt$Tilt, colMeans)

Tilt_props_plot <- melt(Average_probs_Tilt, id.vars = c("Tilt"), value.name = "probability")
head(Average_probs_Tilt) 

Tilt_props_PLOT <- ggplot(Tilt_props_plot, aes(x = Tilt, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                        ., scales = "free")

plot(Tilt_props_PLOT)

# Create complete Probability plot figure for NWW
Probs_DE_HS6_HS8_Complete <- grid.arrange(SKC_props_PLOT, Waterheight_props_PLOT, Temp_props_PLOT, Chloride_props_PLOT ,Noise_props_PLOT, Tilt_props_PLOT, ncol = 3, nrow=2)

# HVS river

# HVS river multinomial logistic regression
ML_HVSr_DE <- subset(Env_DE_HVS_NWW_total, ID == "HDC_HDE")
ML_HVSr_DE$DE <- as.factor(ML_HVSr_DE$DE)
ML_HVSr_DE$Windspeed <- as.numeric(ML_HVSr_DE$Windspeed)
ML_HVSr_DE$Wind_Direction <- as.factor(ML_HVSr_DE$Wind_Direction)
ML_HVSr_DE$Discharge <- as.numeric(ML_HVSr_DE$Discharge)
ML_HVSr_DE$Waterheight <- as.numeric(ML_HVSr_DE$Waterheight)
ML_HVSr_DE$Sluice_Opening <- as.numeric(ML_HVSr_DE$Sluice_Opening)
ML_HVSr_DE$Spui_Kier_Closed <- as.factor(ML_HVSr_DE$Spui_Kier_Closed)
ML_HVSr_DE$Temp_rec <- as.numeric(ML_HVSr_DE$Temp_rec)
ML_HVSr_DE$Chloride <- as.numeric(ML_HVSr_DE$Chloride)
ML_HVSr_DE$Tilt <- as.numeric(ML_HVSr_DE$Tilt)
ML_HVSr_DE$Noise <- as.numeric(ML_HVSr_DE$Noise)
ML_HVSr_DE$DE <- factor(ML_HVSr_DE$DE, levels = c("100", "83.3", "66.7", "50", "33.3", "16.7", "0"))

# Renumber the DE levels: use levels 0 - 6 for PDP plots later on 
ML_HVSr_DE <- ML_HVSr_DE %>%
  mutate(DE = recode(DE, `0` = 1, `16.7` = 2, `33.3` = 3, `50` = 4, `66.7` = 5, `83.3` = 6, `100` = 7))
ML_HVSr_DE$DE <- factor(ML_HVSr_DE$DE, levels = c("1", "2", "3", "4", "5", "6", "7")) # Make sure that order of DE goes down from 100 to 0 for later plots

# Check the correlation between windspeed and temp. Unclear if there is a seasonal relation from the plot
ggplot(ML_HVSr_DE, aes(x = Windspeed, y = Discharge)) +
  geom_point() 
ggplot(ML_HVSr_DE, aes(x = Waterheight, y = Discharge)) +
  geom_point() 
ggplot(ML_HVSr_DE, aes(x = Sluice_Opening, y = Discharge)) +
  geom_point() 
ggplot(ML_HVSr_DE, aes(x = Sluice_Opening, y = Waterheight)) +
  geom_point() 
ggplot(ML_HVSr_DE, aes(x = Sluice_Opening, y = Temperature)) +
  geom_point
ggplot(ML_HVSr_DE, aes(x = Sluice_Opening, y = Chloride)) +
  geom_point() 
ggplot(ML_HVSr_DE, aes(x = Sluice_Opening, y = Tilt)) +
  geom_point()
ggplot(ML_HVSr_DE, aes(x = Sluice_Opening, y = Noise)) +
  geom_point()
ggplot(ML_HVSr_DE, aes(x = Windspeed, y = Temp_rec)) +
  geom_point() 
ggplot(ML_HVSr_DE, aes(x = Windspeed , y = Noise)) +
  geom_point()
ggplot(ML_HVSr_DE, aes(x = Windspeed , y = Tilt)) +
  geom_point()

# Make summary table of the predictor variables
with(ML_HVSr_DE, table(Wind_Direction, DE))
with(ML_HVSr_DE, do.call(rbind, tapply(Windspeed, DE, function(x) c(M = mean(x), SD = sd(x)))))
with(ML_HVSr_DE, do.call(rbind, tapply(Temp_rec, DE, function(x) c(M = mean(x), SD = sd(x)))))
ML_HVSr_DE$DE_2 <- relevel(ML_HVSr_DE$DE, ref = "7") # Set reference DE category

# Put variables in multinomial logistical regression model to see which model is the best fit
ML_HVSr_DE_windS<- nnet::multinom(DE_2 ~ Windspeed, data = ML_HVSr_DE) # AIC 14928.09
ML_HVSr_DE_windSD<- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction, data = ML_HVSr_DE) # AIC 14748.34 
ML_HVSr_DE_windSD_WH <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Waterheight, data = ML_HVSr_DE) # AIC 14433.54  
ML_HVSr_DE_windSD_WH_SO <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Sluice_Opening*Waterheight , data = ML_HVSr_DE) # AIC 13930 
ML_HVSr_DE_windSD_WH_SO_SKC <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed, data = ML_HVSr_DE) # AIC 13606.96 
ML_HVSr_DE_windSD_WH_SO_SKC_Chl <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Chloride, data = ML_HVSr_DE) # AIC 13539.47 
ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction +  Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Chloride + Sluice_Opening*Temp_rec, data = ML_HVSr_DE) # AIC 13369.23  
#ML_HVSr_DE_windSD_WH_SO_SKC_Temp_Chl_Tilt <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction  + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec + Sluice_Opening*Chloride + Sluice_Opening*Tilt , data = ML_HVSr_DE) # AIC 13570.63  
#ML_HVSr_DE_windSD_WH_SO_SKC_Temp_Chl_Noise <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction + Windspeed*Noise + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec + Sluice_Opening*Chloride + Sluice_Opening*Noise , data = ML_HVSr_DE) # AIC 13603.61  
#ML_HVSr_DE_windSD_WH_SO_SKC_Temp_Chl_Noise_Tilt <- nnet::multinom(DE_2 ~ Windspeed*Wind_Direction + Windspeed * Noise + Sluice_Opening*Waterheight + Sluice_Opening*Spui_Kier_Closed + Sluice_Opening*Temp_rec + Sluice_Opening*Chloride + Sluice_Opening*Noise + Sluice_Opening*Tilt, data = ML_HVSr_DE) # AIC 22114.77    

# Don't include noise or tilt as a factor. Did not add significantly to the model. Correlated with sluice opening and wind speed though. 
# Did look at noise and seemed to have quite some effect still. Therefore I think we should include it. 

# Summarize the model and perform significance test obtaining the P value
summary(ML_HVSr_DE_windSD_WH_SO_SKC_Temp_Chl_Noise)

z <- summary(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp)$coefficients/summary(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(test))


# Try to visualize the relationship between predictor variable and response variabale by visualisation 

# Partial dependence plot (PDP).  

# Method 1: PDP simple line plot 
partial_HVSr_Windspeed <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Windspeed")
#plot(partial_HVSr_Windspeed)
partial_HVSr_Windspeed_df <- as.data.frame(partial_HVSr_Windspeed)
partial_HVSr_Windspeed_plot <- ggplot(partial_HVSr_Windspeed_df, aes(x = Windspeed, y = yhat)) +
  geom_line() +
  xlab("Windspeed (m/s)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over windspeed for HD-E")
#plot(partial_HVSr_Windspeed_plot) 
partial_HVSr_SO <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Sluice_Opening") # Incorrect. Must be something with SKC but can't figure out what
plot(partial_HVSr_SO)
partial_HVSr_SO_df <- as.data.frame(partial_HVSr_SO)
partial_HVSr_SO_plot <- ggplot(partial_HVSr_SO_df, aes(x = Sluice_Opening, y = yhat)) +
  geom_line() +
  xlab("Sluice opening (m2)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over Sluice opening for HD-E")
plot(partial_HVSr_SO_plot)
partial_HVSr_Waterheight <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Waterheight")
#plot(partial_HVSr_Waterheight)
partial_HVSr_Waterheight_df <- as.data.frame(partial_HVSr_Waterheight)
partial_HVSr_Waterheight_plot <- ggplot(partial_HVSr_Waterheight_df, aes(x = Waterheight, y = yhat)) +
  geom_line() +
  xlab("Waterheight (cm)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over waterheight for HD-E")
plot(partial_HVSr_Waterheight_plot)
partial_HVSr_Temperature <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Temp_rec")
#plot(partial_HVSr_Temperature)
partial_HVSr_Temperature_df <- as.data.frame(partial_HVSr_Temperature)
partial_HVSr_Temperature_plot <- ggplot(partial_HVSr_Temperature_df, aes(x = Temp_rec, y = yhat)) +
  geom_line() +
  xlab("Temperature (Degrees Celcius)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over temperature for HD-E")
plot(partial_HVSr_Temperature_plot)
partial_HVSr_Chloride <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Chloride")
#plot(partial_HVSr_Chloride)
partial_HVSr_Chloride_df <- as.data.frame(partial_HVSr_Chloride)
partial_HVSr_Chloride_plot <- ggplot(partial_HVSr_Chloride_df, aes(x = Chloride, y = yhat)) +
  geom_line() +
  xlab("Chloride (mg / L)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over Chloride for HD-E")
#plot(partial_HVSr_Chloride_plot)
partial_HVSs_Noise <- partial(ML_HVSs_DE_windSD_WH_SO_SKC_Temp_Chl_Noise, pred.var = "Noise")
#plot(partial_HVSs_Noise)
partial_HVSs_Noise_df <- as.data.frame(partial_HVSs_Noise)
partial_HVSs_Noise_plot <- ggplot(partial_HVSs_Noise_df, aes(x = Noise, y = yhat)) +
  geom_line() +
  xlab("Noise (mV)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over the surrounding noise for HD-E")
plot(partial_HVSs_Noise_plot)
partial_HVSs_Tilt <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Tilt")
#plot(partial_HVSs_Tilt)
partial_HVSs_Tilt_df <- as.data.frame(partial_HVSs_Tilt)
partial_HVSs_Tilt_plot <- ggplot(partial_HVSs_Tilt_df, aes(x = Tilt, y = yhat)) +
  geom_line() +
  xlab("Tilt (Degrees)") +  
  ylab("Predicted DE category") +   
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over receiver tilt")
plot(partial_HVSs_Tilt_plot)

# Create complete Partial dependence plot figure for NWW
PDP_DE_HDC_HDE_Complete <- grid.arrange(partial_HVSr_Windspeed_plot, partial_HVSr_SO_plot, partial_HVSr_Waterheight_plot, partial_HVSr_Temperature_plot, partial_HVSr_Chloride_plot, ncol = 3, nrow=2)

# Method 2: PDP line plot for DE category 

# Create dataframe with environmental variables for each DE category based on the multinom model
partial_combined_HVSr <- do.call(rbind, lapply(1:7, function(cat) {
  cat_name <- paste0("class", cat)
  plot <- partial(ML_HVSr_DE_windSD_WH_SO_SKC_Chl_Temp, pred.var = "Chloride", which.class = cat, prob = TRUE)
  df <- as.data.frame(plot)
  if (nrow(df) > 0) {df$category <- cat 
  return(df)}
  return(NULL)}))

# Plot the partial dependence lineplot. Change the env. variable every time to create a total figure with all the graphs
colors <- c("blue", "orange", "green", "red", "purple", "brown", "black")
partial_HVSr_Chloride_plot <- ggplot(partial_combined_HVSr, aes(x = Chloride, y = yhat, color = factor(category))) +
  geom_line() +
  xlab("Chloride (mg/L)") +  
  ylab("Predicted Probability") +   
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Predicted DE category over chloride in HVS river") +
  scale_color_manual(name = "DE Category", values = colors) +
  theme(plot.title = element_text(size = 13, hjust = 0.3, face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"))+
  ylim(0, 1)
plot(partial_HVSr_Chloride_plot)

# List all the plots you just made
partial_HVSr_Windspeed_plot
partial_HVSr_Sluice_Opening_plot
partial_HVSr_Waterheight_plot
partial_HVSr_Temp_rec_plot
partial_HVSr_Chloride_plot

# Create complete PDP for HVSsea
PDP_DE_HDC_HDE_Complete_LinePlot <- grid.arrange(partial_HVSr_Windspeed_plot, partial_HVSr_Sluice_Opening_plot, partial_HVSr_Waterheight_plot, partial_HVSr_Temp_rec_plot, partial_HVSr_Chloride_plot, ncol = 3, nrow=2)
plot(PDP_DE_HS6_HS8_Complete_LinePlot)



# ANALYSE THE PROBABILITIES FOR EACH DE CATEGORY

# Create models containing only the predictor variable(s) of choice. Otherwise it will not work 
ML_HVSr_DE$DE_2 <- relevel(ML_HVSr_DE$DE, ref = "100") # Set reference DE category
ML_HVSr_DE_WindSD <- nnet::multinom(DE_2 ~ Windspeed * Wind_Direction, data = ML_HVSr_DE) # AIC 22993.15 
ML_HVSr_DE_SO <- nnet::multinom(DE_2 ~ Sluice_Opening*Spui_Kier_Closed, data = ML_HVSr_DE) # AIC 22993.15 
ML_HVSr_DE_Waterheight <- nnet::multinom(DE_2 ~ Waterheight, data = ML_HVSr_DE) # AIC 22993.15 
ML_HVSr_DE_Temperature <- nnet::multinom(DE_2 ~ Temp_rec, data = ML_HVSr_DE) # AIC 22993.15 
ML_HVSr_DE_Chloride <- nnet::multinom(DE_2 ~ Chloride, data = ML_HVSr_DE) # AIC 22993.15 
ML_HVSr_DE_Noise <- nnet::multinom(DE_2 ~ Noise, data = ML_HVSr_DE) # AIC 22993.15 
ML_HVSr_DE_Tilt <- nnet::multinom(DE_2 ~ Tilt, data = ML_HVSr_DE) # AIC 22993.15 

# Calculate and plot the average predicted probabilities over the continuous variable of interest

# Wind. THIS ONE IS NOT WORKING WELL YET. WHY?
head(pp <- fitted(ML_HVSr_DE_WindSD))
Wind_S <- data.frame(Wind_Direction = c("North", "East", "South", "West"), Windspeed = mean(ML_HVSs_DE$Windspeed))
predict(ML_HVSs_DE_WindSD, newdata = Wind_S, "probs")

# Calculate average predicted probabilities over the different values of continuous variable 
Wind_SD <- data.frame(Wind_Direction = rep(c("North", "East", "South", "West"), each = 21), Windspeed = rep(c(0:20),
                                                                                                            4))
Average_probs_WD <- cbind(Wind_SD, predict(ML_HVSs_DE_WindSD, newdata = Wind_SD, type = "probs", se = TRUE))
by(Average_probs_WD[, 3:9], Average_probs_WD$Wind_Direction, colMeans)

# Plot the average probabilities
WD_props_plot <- melt(Average_probs_WD, id.vars = c("Wind_Direction", "Windspeed"), value.name = "probability")
head(WD_props_plot) 

WD_props_plot$Wind_Direction <- factor(WD_props_plot$Wind_Direction, 
                                       levels = c("North", "East", "South", "West"),
                                       ordered = TRUE)

WindSD_props_PLOT <- ggplot(WD_props_plot, aes(x = Windspeed, y = probability, colour = Wind_Direction)) + geom_line() + facet_grid(variable ~
                                                                                                                                      ., scales = "free")
# Sluice Opening
head(pp <- fitted(ML_HVSr_DE_SO))
SO_HVSr <- data.frame(Spui_Kier_Closed = c("Spui", "Kier"), Sluice_Opening = mean(ML_HVSr_DE$Sluice_Opening))
predict(ML_HVSr_DE_SO, newdata = SO_HVSr, "probs")

# Determine max sluice opening during kieren
max_sluice_opening_kier <- max(ML_HVSr_DE$Sluice_Opening[ML_HVSr_DE$Spui_Kier_Closed == "Kier"])
print(max_sluice_opening_kier)

# Create bins for sluice_opening and link to 
sluice_opening_values <- seq(0, 800, by = 50)

# Create all combinations of Spui_Kier_Closed and Sluice_Opening
combinations <- expand.grid(Spui_Kier_Closed = c("Spui", "Kier"),
                            Sluice_Opening = sluice_opening_values)

# Create the data frame
SO_HVSr_SKC <- combinations[rep(seq_len(nrow(combinations)), each = 20), ]

# Calculate predictions and probabilities
Average_probs_SKC <- cbind(SO_HVSr_SKC, predict(ML_HVSr_DE_SO, newdata = SO_HVSr_SKC, type = "probs", se = TRUE))

# Calculate column means by Spui_Kier_Closed
by(Average_probs_SKC[, 3:9], Average_probs_SKC$Spui_Kier_Closed, colMeans)

# Plot the average probabilities
SKC_props_plot <- melt(Average_probs_SKC, id.vars = c("Spui_Kier_Closed", "Sluice_Opening"), value.name = "probability")
head(SKC_props_plot) 

SKC_props_plot$Spui_Kier_Closed <- factor(SKC_props_plot$Spui_Kier_Closed, 
                                          levels = c("Spui", "Kier"),
                                          ordered = TRUE)
SKC_props_PLOT <- ggplot(SKC_props_plot, aes(x = Sluice_Opening, y = probability, colour = Spui_Kier_Closed)) + geom_line() + facet_grid(variable ~     ., scales = "free")+
  ggtitle("Sluice management types as factors of the sluice opening, the continuous predictor variable for DE between HD-C and HD-E")


plot(SKC_props_PLOT)

# Waterheight
Waterheight_bins <- data.frame(Waterheight = rep(seq(-200, 200, by = 20), each = 20))
Average_probs_Waterheight <- cbind(Waterheight_bins, predict(ML_HVSr_DE_Waterheight, newdata = Waterheight_bins, type = "probs", se = TRUE))
by(Average_probs_Waterheight[, 2:8], Average_probs_Waterheight$Waterheight, colMeans)

Waterheight_props_plot <- melt(Average_probs_Waterheight, id.vars = c("Waterheight(cm)"), value.name = "probability")
head(Waterheight_props_plot) 

Waterheight_props_PLOT <- ggplot(Waterheight_props_plot, aes(x = Waterheight, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                                             ., scales = "free")+
  xlab("Waterheight (cm)")+
  ggtitle("Waterheight as a continuous predictor value for the categories of DE between HD-C and HD-E")
print(Waterheight_props_PLOT)

# Temperature
Temp_bins <- data.frame(Temp_rec = rep(seq(0, 24, by = 2), each = 10))
Average_probs_Temp <- cbind(Temp_bins, predict(ML_HVSr_DE_Temperature, newdata = Temp_bins, type = "probs", se = TRUE))
by(Average_probs_Temp[, 2:8], Average_probs_Temp$Temp_rec, colMeans)

Temp_props_plot <- melt(Average_probs_Temp, id.vars = c("Temp_rec"), value.name = "probability")
head(Average_probs_Temp) 

Temp_props_PLOT <- ggplot(Temp_props_plot, aes(x = Temp_rec, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                            ., scales = "free")+
  xlab("Temperature (degrees Celcius)")+
  ggtitle("Temperature as a continuous predictor value for the categories of DE between HD-C and HD-E")
print(Temp_props_PLOT)

# Noise
Noise_bins <- data.frame(Noise = rep(seq(0, 700, by = 100), each = 7))
Average_probs_Noise <- cbind(Noise_bins, predict(ML_HVSs_DE_Noise, newdata = Noise_bins, type = "probs", se = TRUE))
by(Average_probs_Noise[, 2:8], Average_probs_Noise$Noise, colMeans)

Noise_props_plot <- melt(Average_probs_Noise, id.vars = c("Noise"), value.name = "probability")
head(Average_probs_Noise) 

Noise_props_PLOT <- ggplot(Noise_props_plot, aes(x = Noise, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                           ., scales = "free")

# Chloride
Chloride_bins <- data.frame(Chloride = rep(seq(0, 150, by = 100), each = 7))
Average_probs_Chloride <- cbind(Chloride_bins, predict(ML_HVSr_DE_Chloride, newdata = Chloride_bins, type = "probs", se = TRUE))
by(Average_probs_Chloride[, 2:8], Average_probs_Chloride$Chloride, colMeans)

Chloride_props_plot <- melt(Average_probs_Chloride, id.vars = c("Chloride"), value.name = "probability")
head(Average_probs_Chloride) 

Chloride_props_PLOT <- ggplot(Chloride_props_plot, aes(x = Chloride, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                                    ., scales = "free")+
  xlab("Chloride (mg / L)")+
  ggtitle("Chloride as a continuous predictor value for the categories of DE between HD-C and HD-E")

print(Chloride_props_PLOT)

# Tilt
Tilt_bins <- data.frame(Tilt = rep(seq(0, 70, by = 10), each = 7))
Average_probs_Tilt <- cbind(Tilt_bins, predict(ML_HVSs_DE_Tilt, newdata = Tilt_bins, type = "probs", se = TRUE))
by(Average_probs_Tilt[, 2:8], Average_probs_Tilt$Tilt, colMeans)

Tilt_props_plot <- melt(Average_probs_Tilt, id.vars = c("Tilt"), value.name = "probability")
head(Average_probs_Tilt) 

Tilt_props_PLOT <- ggplot(Tilt_props_plot, aes(x = Tilt, y = probability)) + geom_line() + facet_grid(variable ~
                                                                                                        ., scales = "free")

plot(Tilt_props_PLOT)

# Create complete Probability plot figure for NWW
Probs_DE_HDC_HDE_Complete <- grid.arrange(SKC_props_PLOT, Waterheight_props_PLOT, Temp_props_PLOT, Chloride_props_PLOT , ncol = 3, nrow=2)








