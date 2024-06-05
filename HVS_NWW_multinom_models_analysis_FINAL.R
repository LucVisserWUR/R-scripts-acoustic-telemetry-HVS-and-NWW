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


# Use multinomial model (multinom function from nnet package)

# NWW 
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

# Visualize the relationship between predictor variable and response variables by visualization 

# Partial Dependence Plot (PDP) line plot for DE categories, NWW

# Create data frame with environmental variables for each DE category based on the multinom model
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

# HVS SEA

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

# Try to visualize the relationship between predictor variable and response variables by visualization 

# Partial Dependence Plot (PDP) line plot for DE categories, HVS SEA 

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

# HVS RIVER

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

# Partial Dependence Plot (PDP) line plot for DE category, HVS RIVER

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

# Create complete PDP for HVS RIVER
PDP_DE_HDC_HDE_Complete_LinePlot <- grid.arrange(partial_HVSr_Windspeed_plot, partial_HVSr_Sluice_Opening_plot, partial_HVSr_Waterheight_plot, partial_HVSr_Temp_rec_plot, partial_HVSr_Chloride_plot, ncol = 3, nrow=2)
plot(PDP_DE_HDC_HDE_Complete_LinePlot)










