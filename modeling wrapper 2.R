
# This is the modeling wrapper 

# Steps in the process


  #1) simulate the fishery for the basleline year (2019) under actual regulations/fishing conditions
      # Requires:
        # a) catch-at-length dist'n. I use MRIP data from 2018 and 2019 to create this dist'n (gamma) for each 
        #    species included in the simualtion (SF, BSB, scup, WF, RD) in the basleline year.
        # b) catch-per-trip distribution. These are based on trips that caught or targeted summer flounder. 
        #    I use 2019 data to create fitted dist'ns for scup, WF, RD. Catch-per-trip distributions for 
        #     SF and BSB are based on copula modeling.
        # c) Sets of utility parameters from each of the four surveys
        # d) Distributions of trip costs derived from the 2017 expenditure survey
        # e) a file containing regulations for each state and species. There are several intra-seasonal periods for 
        #    for each state that differ in  regualtions across species. 
      
        # After running the calibration, retain keep- and release-at-length for summer flounder (#'s), total keep and release 
        # for other species, and number of choice occasions for each state/period. Note that for these outputs, 
        # there will be X estimates based on X draws of utility parameters. 

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch



# Modeling wrapper test
install.packages("readxl")
install.packages("tidyr")
install.packages("reshape2")
install.packages("splitstackshape")
install.packages("doBy")
install.packages("WriteXLS")
install.packages("Writexl")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages("ggplot")
install.packages("dplyr")

library(Rcpp)
library(writexl)
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(splitstackshape)
library(doBy)
library(WriteXLS)
library(fitdistrplus)
library(ggplot2)


# Set the wd to wherever all the code/data is 

setwd("C:/Users/andrew.carr-harris/Dropbox/NMFS/fluke_mse/simulation_R_code/")


# Start the clock!
  ptm <- proc.time()


########## 
# 1) Run the calibration files
# I've included only the Massachusetts file for now
 
source("calibration2 MA.R")
source("calibration2 RI.R")
source("calibration2 CT.R")
source("calibration2 NY.R")
source("calibration2 NJ.R")
source("calibration2 DE.R")
source("calibration2 MD.R")
source("calibration2 VA.R")
source("calibration2 NC.R")
  

# Combine the results  
calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                       pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                       pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))

#calibration_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
calibration_output_by_period[is.na(calibration_output_by_period)] = 0
write_xlsx(calibration_output_by_period,"calibration_output_by_period.xlsx")

aggregate_calibration_output= subset(calibration_output_by_period, select=-c(state, alt_regs))
aggregate_calibration_output = aggregate(aggregate_calibration_output, by=list(calibration_output_by_period$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_calibration_output,"aggregate_calibration_output.xlsx")

##########  






##########  
# Input new population numbers-at-length distribution, run the following script to create catch-at-length for summer flounder
# For now, this I am using the same catch-at-length distirbutions for the baseline and prediction years 

# source("catch at length given stock structure - prediction.R")
##########  







##########  
# run the simulation code under the new set of regulations (directed_trips_region - alternative regs test.xlsx)
# I've included only the Massachusetts file for now

source("prediction2 MA.R")
source("prediction2 RI.R")
source("prediction2 CT.R")
source("prediction2 NY.R")
source("prediction2 NJ.R")
source("prediction2 DE.R")
source("prediction2 MD.R")
source("prediction2 VA.R")
source("prediction2 NC.R")


prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA, pds_new_all_RI, pds_new_all_CT,
                                                      pds_new_all_NY, pds_new_all_NJ, pds_new_all_DE,
                                                      pds_new_all_MD, pds_new_all_VA, pds_new_all_NC))
# 
#prediction_output_by_period = as.data.frame(bind_rows(pds_new_all_MA))
prediction_output_by_period[is.na(prediction_output_by_period)] = 0
write_xlsx(prediction_output_by_period,"prediction_output_by_period.xlsx")

aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs))
aggregate_prediction_output = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")

##########  


# Stop the clock
proc.time() - ptm







