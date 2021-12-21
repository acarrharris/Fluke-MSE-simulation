
# This is the modeling wrapper 

# Steps in the process

  #2) Simulate the fishery under alternative regulations and a new catch-at-length distribution for summer flounder
        # a) Create new catch-at-length/catch-per-trip distributions for summer flounder based on population numbers at length. 
        # a) Calcualte angler welfare/fishing effort changes and changes in catch


# Modeling wrapper test

#load needed packages and install if not currently installed.
pkgs_to_use <- c("tidyr",
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
                 "scales",
                 "univariateML",
                 "logspline",
                 "readr")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE)

### 

# Input the data set containing alternative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
#directed_trips_table <- data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))
directed_trips_table <- readRDS("directed_trips_regions_bimonthly.rds")
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
source("catch at length given stock structure - prediction.R")

#catch data
sf_catch_data_no <- readRDS("predicted_catch_NO.xlsx") %>% 
  tibble() %>% 
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_nj <- readRDS("predicted_catch_NJ.rds") %>% 
  tibble() %>% 
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()
sf_catch_data_so <- readRDS("predicted_catch_SO.rds") %>% 
  tibble() %>% 
  rename(tot_sf_catch = sf_t_nb,
         tot_bsb_catch = bsb_t_nb) %>%
  I()


# Read-in the current population length composition  #don't need this in final as it's already an object.
#size_data_read <- data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))
size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()

# loop over states
params <- list(state1 = c("MA","RI","CT","NY","NJ","DE","MD","VA","NC"),
               region1 = c(rep("NO",4),"NJ",rep("SO",4)),
               calibration_data_table = rep(list(calibration_data_table),9),
               directed_trips_table = rep(list(directed_trips_table),9),
               size_data_read = rep(list(size_data_read),9),
               param_draws_MA = param_draws_all,
               costs_new_all_MA = costs_new,
               sf_catch_data_all = c(rep(list(sf_catch_data_no),4),list(sf_catch_data_nj),rep(list(sf_catch_data_so),4)),
               prop_bsb_keep = rep(0.33,9))  # add Lou's p* values here!

# params <- list(state1 = "MA",
#                region1 = "NO",
#                calibration_data_table = list(calibration_data_table),
#                directed_trips_table = list(directed_trips_table),
#                size_data_read = list(size_data_read),
#                param_draws_MA = list(param_draws_all[[1]]),
#                costs_new_all_MA = list(costs_new[[1]]),
#                sf_catch_data_all = list(sf_catch_data_no))
# 
# params <- list(state1 = "NJ",
#                region1 = "NJ",
#                calibration_data_table = list(calibration_data_table),
#                directed_trips_table = list(directed_trips_table),
#                size_data_read = list(size_data_read),
#                param_draws_MA = list(param_draws_all[[5]]),
#                costs_new_all_MA = list(costs_new[[5]]),
#                sf_catch_data_all = list(sf_catch_data_nj))

source("prediction-all.R")


##########  need to add link to OM scenario regulations

safe_predict_rec_catch <- purrr::safely(predict_rec_catch, otherwise = NA_real_)
profvis::profvis(xx <- purrr::pmap(params, safe_predict_rec_catch))

# xx <- predict_rec_catch(state1 = "MA",
#                         region1 = "NO",
#                         calibration_data_table = calibration_data_table,
#                         directed_trips_table = directed_trips_table,
#                         size_data_read = size_data_read,
#                         param_draws_MA = param_draws_all[[1]],
#                         costs_new_all_MA = costs_new[[1]],
#                         sf_catch_data_all = sf_catch_data_no)
# 
# xx <- predict_rec_catch(state1 = "NJ",
#                         region1 = "NJ",
#                         calibration_data_table = calibration_data_table,
#                         directed_trips_table = directed_trips_table,
#                         size_data_read = size_data_read,
#                         param_draws_MA = param_draws_all[[5]],
#                         costs_new_all_MA = costs_new[[5]],
#                         sf_catch_data_all = sf_catch_data_nj)


prediction_output_by_period <- purrr::map(xx, 1)

saveRDS(prediction_output_by_period, file = "prediction_output_by_period.rds")

#aggregate_prediction_output= subset(prediction_output_by_period, select=-c(state, alt_regs, period))
aggregate_prediction_output <- prediction_output_by_period %>% 
  list.stack(fill = TRUE) %>% 
  mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
  dplyr::select(-state, -alt_regs, -period) %>% 
  group_by(sim) %>% 
  summarize_if(is.numeric, .funs = sum,na.rm=TRUE) %>% 
  #dplyr::select(order(colnames(.))) %>% 
  I()
#  = aggregate(aggregate_prediction_output, by=list(aggregate_prediction_output$sim),FUN=sum, na.rm=TRUE)
#write_xlsx(aggregate_prediction_output,"aggregate_prediction_output.xlsx")
saveRDS(aggregate_prediction_output, file = "aggregate_prediction_output.rds")

##########  

pred_len <- tibble(aggregate_prediction_output) %>% 
  dplyr::select(contains("length")) %>% 
  pivot_longer(cols = 1:ncol(.), names_to = "bin",values_to = "num") %>% 
  separate(bin, into =c("type","len"),sep = "_length_") %>% 
  mutate(len = as.numeric(len)) %>% 
  I()
pred_len
out_lens <- tibble(type = rep(c("release","keep"),each=Nlen_in),
                   len = rep(lenbinuse,2)) %>% 
  left_join(pred_len) %>% 
  replace_na(list(num=0)) %>% 
  I()
out_lens
in2cm <- readr::read_csv("in2cm.csv", col_names = FALSE)[,-1]
keep <- out_lens %>% 
  filter(type == "keep") %>% 
  dplyr::select(num) %>% 
  unlist() %>%
  I()
keep <- keep %*% t(in2cm)
release <- out_lens %>% 
  filter(type == "release") %>% 
  dplyr::select(num) %>% 
  unlist() %>%
  I()
release <- release %*% t(in2cm)
write.table(round(rbind(keep,release),3),file = "rec-catch.out", row.names = FALSE, col.names = FALSE)


#####
# Stop the clock
proc.time() - ptm



# ###
# # Calculate ouput statisitics for calibration and prediction year
# source("simulation output stats.R")
# 

