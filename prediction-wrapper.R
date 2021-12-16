
# This is the modeling wrapper 

# Steps in the process

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
                 "logspline",
                 "readr")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE)

### 

# Input the data set containing alternative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
#directed_trips_table <- data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))

# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
calibration_data_table <- readRDS("calibration_output_by_period.rds")
#utility parameter draws
param_draws_all <- readRDS("param_draws_all.rds")
#costs
costs_new <- readRDS( "costs_all.rds")


# Read-in current population length composition (from sinatra output)
Nlen <- 42
om_length_cm <- scan("om-length.dat",n=Nlen+1)
cm2in <- read_csv("cm2in.csv", col_names = FALSE)
lenbinuse <- as.integer(unlist(cm2in[,1]))
Nlen_in <- length(lenbinuse)
cm2in <- cm2in %>% 
  dplyr::select(-1) %>% 
  as.matrix() %>% 
  I()
om_length_in <- om_length_cm[-1] %*% t(cm2in)
# size_data <- data.frame(fitted_prob = rep(om_length_in,3),
#                         fitted_length = rep(lenbinuse,3),
#                         region = rep(c("SO","NJ","NO"), each = Nlen_in),
#                         year = rep("y2",3*Nlen_in))


# catch-at-length and catch-per-trip for summer flounder
profvis::profvis(source("catch at length given stock structure - prediction.R"))



# Read-in the current population length composition
size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))


params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA","NC"),
               region1 = c(rep("NO",4),"NJ",rep("SO",4)),
               calibration_data_table = rep(list(calibration_data_table),9),
               directed_trips_table = rep(list(directed_trips_table),9),
               #size_data_read = rep(list(size_data_read),9),
               size_data_read = rep(list(size_data),9),
               param_draws_MANY = c(rep(list(ud1),4),list(ud2),rep(list(ud3),2),rep(list(ud4),2)),
               costs_new_all = costs_new)

params <- list(state1 = "MA",
               region1 = "NO",
               calibration_data_table = list(calibration_data_table),
               directed_trips_table = list(directed_trips_table),
               size_data_read = list(size_data_read),
               param_draws_MANY = list(ud1),
               costs_new_all = list(costs_new[[1]]))











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


