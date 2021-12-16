
# This is the modeling wrapper 

# Steps in the process


  #1) simulate the fishery for the basleline year (2019) under actual regulations/fishing conditions
      # Requires:
        # a) catch-per-trip distribution. These are based on trips that caught or targeted summer flounder. 
        #    I use 2019 data to create fitted dist'ns for scup, WF, RD. Catch-per-trip distributions for 
        #     SF and BSB are based on copula modeling.
        # b) Sets of utility parameters from each of the four surveys
        # c) Distributions of trip costs derived from the 2017 expenditure survey
        # d) a file containing regulations for each state and species. There are 24 bi-monthly periods for 
        #    for each state. Some periods differ in  regualtions across species. 
      
        # After running the calibration, retain keep- and release-at-length for summer flounder (#'s), total keep and release 
        # for other species, and number of choice occasions for each state/period. Note that for these outputs, 
        # there will be X estimates based on X draws of utility parameters. 

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch



# Modeling wrapper test

#load needed packages and install if not currently installed.
pkgs_to_use <- c("readxl",
                 "tidyr",
                 "reshape2",
                 "splitstackshape",
                 "doBy",
                 "WriteXLS",
                 'Rcpp',
                 "ggplot2",
                 "dplyr",
                 "rlist",
                 "fitdistrplus",
                 "MASS",
                 "psych",
                 "rgl",
                 "copula",
                 "VineCopula",
                 "readxl",
                 "scales",
                 "univariateML",
                 "xlsx",
                 "writexl",
                 "logspline")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE)

# Set the wd to wherever all the code/data is 

#setwd("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/")
setwd("C:/Users/Lou/Dropbox/NMFS/fluke_mse/simulation_R_code/")


# Start the clock!
ptm <- proc.time()


########## 
# 1) Run the calibration files

source("calibration4 MA.R")
source("calibration4 RI.R")
source("calibration4 CT.R")
source("calibration4 NY.R")
source("calibration4 NJ.R")
source("calibration4 DE.R")
source("calibration4 MD.R")
source("calibration4 VA.R")
source("calibration4 NC.R")
  

# Combine the results  
calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                       pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                       pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

#calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
calibration_output_by_period[is.na(calibration_output_by_period)] = 0
write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")



aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs, period))
aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")


#Apply the calibration estimates of total catch to the catch-at-length distribution used in the assessment 
tot_sf_keep = aggregate_calibration_output$tot_keep
tot_sf_rel = aggregate_calibration_output$tot_rel
tot_sf_catch = tot_sf_keep+tot_sf_rel

assment_CAL = data.frame(read_excel("assessment_catch_at_length.xlsx"))                                                                            
assment_CAL$calibration_keep_at_length=assment_CAL$ab1_prop*tot_sf_keep
assment_CAL$calibration_release_at_length=assment_CAL$b2_prop*tot_sf_rel

calibration_catch_at_length= subset(assment_CAL, select=c(l_in_bin, calibration_keep_at_length, calibration_release_at_length))

##########  

#save calibration output objects
saveRDS(calibration_output_by_period,file = "calibration_output_by_period.rds")
saveRDS(aggregate_calibration_output,file = "aggregate_calibration_output.rds")
saveRDS(calibration_catch_at_length, file = "calibration_catch_at_length.rds")

costs_all <- NULL
costs_all[[1]] <- costs_new_all_MA
costs_all[[2]] <- costs_new_all_RI
costs_all[[3]] <- costs_new_all_CT
costs_all[[4]] <- costs_new_all_NY
costs_all[[5]] <- costs_new_all_NJ
costs_all[[6]] <- costs_new_all_DE
costs_all[[7]] <- costs_new_all_MD
costs_all[[8]] <- costs_new_all_VA
costs_all[[9]] <- costs_new_all_NC
saveRDS(costs_all, file = "costs_all.rds")

param_draws_all <- NULL
param_draws_all[[1]] <- param_draws_MA
param_draws_all[[2]] <- param_draws_RI
param_draws_all[[3]] <- param_draws_CT
param_draws_all[[4]] <- param_draws_NY
param_draws_all[[5]] <- param_draws_NJ
param_draws_all[[6]] <- param_draws_DE
param_draws_all[[7]] <- param_draws_MD
param_draws_all[[8]] <- param_draws_VA
param_draws_all[[9]] <- param_draws_NC
saveRDS(param_draws_all, file = "param_draws_all.rds")



##########  
# Input new population numbers-at-age distribution (numbers_at_age_YYYY) in the following script to create population adjusted 
# catch-at-length and catch-per-trip for summer flounder
source("catch at length given stock structure - prediction.R")
##########  







##########  
# run the simulation code under the new set of regulations (regulatiopn file is directed_trips_region - alternative regs test.xlsx)

source("prediction3 MA.R")
source("prediction3 RI.R")
source("prediction3 CT.R")
source("prediction3 NY.R")
source("prediction3 NJ.R")
source("prediction3 DE.R")
source("prediction3 MD.R")
source("prediction3 VA.R")
source("prediction3 NC.R")


prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                      pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                      pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

prediction_output_by_period[is.na(prediction_output_by_period)] = 0
write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")

aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

##########  


# Stop the clock
proc.time() - ptm



###
# Calculate ouput statisitics for calibration and prediction year
source("simulation output stats.R")


