Steps to analyse the performance of the acoustic telemetry network around the Haringvlietsluices (HVS) and Nieuwe Waterweg (NWW):

Comment: For step 1 - 4, you'll only need the branch named 'main'. After that, you'll also need files from the 'Environmental-variable-csv-files' branch

 1. First load the data set that contains all the detections. Do so in the 'Abacus_DE_time_distance_HVS_NWW.R' R script.
    For this, you need the csv file 'Detections2023_2024.csv'. This file is too big too upload in GitHub. Contact me personally to receive this csv file.

2. Once you have the 'Detections2023_2024.csv' file, continue in the 'Abacus_DE_time_distance_HVS_NWW.R' R file. Perform the first 3 chapters. 
   The csv files that are required ('Receiverinformation2023_2024.csv', 'Transmitterinformation2023_2024.csv' and 'Receiver_Sync_Transmitter_Combo.csv') are all provided in this same repository.
   The file 'Receiver_distances_matrix_correct.csv' is also provided in the repository, this is a product of the calculated distances between receivers
   You can also calculate distances between your own receivers. To do so, use the 'Inter_receiver_distances_correct.R' R script. 

3. To continue with chapter 4 and 5 of 'Abacus_DE_time_distance_HVS_NWW.R' you'll need the calculated hourly detection efficiencies of the HVS and NWW areas. 
   Obtain these calculated detection efficiencies for both areas by running the 'DE_calculation_HVS_1_hour.R' and 'DE_calculation_NWW_1_hour.R' scripts. 
   This will result in two data frames: 'one_hourly_observed_expected_detections_HVS' and 'one_hourly_observed_expected_detections_NWW'
   Use these two dataframes for all the remaining script that require the calculated detection efficiencies.

4. To dive deeper into the effects of different types of sluice management, continue with the 'Sluice_management_analysis_FINAL.R' script. To work with this script you'll need to load the 'Env_Getij_Spui_Kier_HVS.csv' csv file, which is also provided in the repository. 

5. Now you can import the environmental variable information, to later link it to the detection efficiencies that were calculated in DE_calculation_HVS_1_hour.R' and 'DE_calculation_NWW_1_hour'.
To correlate detection efficiency with the environmental variables, you should start with the 'Env_Variables_HVS_NWW_MainScript.R' script. This script will form the mainscript for all the plots and statistical analyses you will later do. In the 'Env_Variables_HVS_NWW_MainScript.R' script you will import environmental data by importing a variety of environmental variable csv files. Most of these csv files can be found in the 'Environmental-variable-csv-files' branch. Some files were too big to attach in this branch. For the files 'Env_hvs-debiet_0308.csv', 




Environmental-variable-csv-files
   

DE_Env_Var_HVS_NWW_Plots_FINAL.R

Env_Variables_HVS_NWW_Plots_FINAL.R

Env_Variables_HVS_NWW_MainScript.R

HVS_NWW_multinom_models_analysis_FINAL.R


7. In the 4th chapter of the 'Mainscript' you'll be required to import receiver information from VUE first. Do so for the receivers (called sync transmitters in the R script) of your interest. 
