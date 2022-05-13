

###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By region (MA-NY, NJ, DE-NC): catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 


# This code requires the following data:
# 1) The output from the calibration: calibration_output_by_period.xlsx
# 2) Dataset containing the alternative regulations to be imposed: directed_trips_region - alternative regs test.xlsx
# 3) Abundance- and size-adjusted catch-at-length for summer flounder. This is an output file from the script catch at length given stock structure - prediction.R
 
predict_rec_catch <- function(state1 = "MA", 
                               region1 = "NO", 
                               calibration_data_table = NULL,
                               directed_trips_table = NULL,
                               size_data_read = NULL,
                               param_draws_MA = NULL,
                               costs_new_all_MA = NULL,
                               sf_catch_data_all = NULL,
                               prop_bsb_keep = 0.33,
                               dchoose = 1) {
   

# #   #MA test vals for running through function directly
# state1 = "MA"
# region1 = "NO"
# calibration_data_table = calibration_data_table
# directed_trips_table = directed_trips_table
# size_data_read = size_data_read
# param_draws_MA = param_draws_all[[1]]
# costs_new_all_MA = costs_new[[1]]
# sf_catch_data_all = sf_catch_data_ma
# prop_bsb_keep = 1-0.53
# dchoose = 1

# 
   # state1 = "NC"
   # region1 = "SO"
   # calibration_data_table = calibration_data_table
   # directed_trips_table = directed_trips_table
   # size_data_read = size_data_read
   # param_draws_MA = param_draws_all[[9]]
   # costs_new_all_MA = costs_new[[9]]
   # sf_catch_data_all = sf_catch_data_so
   
   
  #profvis::profvis({
  #lp <- lineprof({
# state1="MA"
# region1="NO"
calibration_data <- calibration_data_table %>% tibble() %>% dplyr::filter(state == state1) %>% 
  dplyr::select(period, sim, state, n_choice_occasions)
directed_trips <- directed_trips_table %>% tibble() %>% dplyr::filter(state == state1)
#size_data <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble() %>% filter(region == region1)
size_data <- size_data_read %>% filter(region == state1)
#param_draws_MA <- param_draws_all[[1]]
#costs_new_all_MA <- costs_new[[1]]
# sf_catch_data_all <- readRDS("predicted_catch_NO.xlsx") %>% 
#   tibble() %>% 
#   rename(tot_sf_catch = sf_t_nb,
#          tot_bsb_catch = bsb_t_nb) %>%
#   I()

# Input the calibration output which contains the number of choice occasions needed to simulate
#calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
#calibration_data <- subset(calibration_data, state == state1, select=c(period, sim, state, n_choice_occasions))


# Input the data set containing alterntative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
#directed_trips = data.frame(read_excel("directed_trips_regions_bimonthly.xlsx"))
directed_trips$dtrip <- round(directed_trips$dtrip_2019)
#directed_trips <- subset(directed_trips, state == state1)

min_period <- min(directed_trips$period)
max_period <- max(directed_trips$period)


######################################
##   Begin simulating trip outcomes ##
######################################

periodz <- as.factor(directed_trips$period)
levels(periodz)
nperiod <- length(periodz)

#for(p in levels(periodz)){
directed_trips_p = directed_trips %>% #subset(directed_trips, period == p)
  mutate(period = as.character(period)) %>% 
  group_by(period) %>% 
  mutate(n_trips = floor(mean(dtrip_2019)),
         n_draws = floor(min(1000,n_trips*2.5)))
         #n_draws = floor(min(30000,n_trips*2.5)))
nsamp = 10
niter <- nsamp*sum(directed_trips_p$n_draws)

period_vec <- directed_trips_p %>% 
  dplyr::select(period, n_draws) %>% 
  uncount(n_draws)

regs <- directed_trips_p %>% 
  dplyr::select(period, fluke_bag, fluke_min, fluke_max,
                bsb_bag,
                bsb_min,
                scup_bag,
                scup_min,
                wf_bag,
                wf_min,
                rd_bag,
                rd_min,
                rd_max)
# fluke_bag = mean(directed_trips_p$fluke_bag)
# fluke_min = mean(directed_trips_p$fluke_min)
# fluke_max = mean(directed_trips_p$fluke_max)
# bsb_bag = mean(directed_trips_p$bsb_bag)
# bsb_min = mean(directed_trips_p$bsb_min)
# if (region1 %in% c("NO","NJ")) {
#   scup_bag = mean(directed_trips_p$scup_bag)
#   scup_min = mean(directed_trips_p$scup_min)
# }
# if(state1 %in% c("NJ","DE","MD","VA")) {
#   wf_bag = mean(directed_trips_p$wf_bag)
#   wf_min = mean(directed_trips_p$wf_min)    
# }
# if(state1 %in% c("VA","NC")) {
#   rd_bag = mean(directed_trips_p$rd_bag)
#   rd_min = mean(directed_trips_p$rd_min)  
#   rd_max = mean(directed_trips_p$rd_max)  
# }

#dfs = list()

# Input catch-per-trip numbers 
#sf_catch_data = data.frame(read_excel("predicted_catch_NO.xlsx"))                                                                            
#tot_sf_catch = sf_catch_data$sf_t_nb
#tot_bsb_catch = sf_catch_data$bsb_t_nb


#set.seed(10)
#get_catch_draws <- function(i, sf_catch_data_all) {    
#  print(i)
# random draw of fluke and bsb catch
#sf_catch_data = as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_draws), ])
#sf_catch_data$tripid = 1:nrow(sf_catch_data)
#set.seed(42)
sf_catch_data <- sf_catch_data_all %>% 
  slice_sample(n = niter, replace = TRUE) %>%
  mutate(period = rep(period_vec$period, each = nsamp),
         catch_draw = rep(1:nsamp, length.out = niter),
         tripid = rep(unlist(purrr::map(directed_trips_p$n_draws, seq_len)), each = nsamp)) #%>% 
  #tibble::rowid_to_column("tripid") %>%
  #I()

# subset trips with zero catch, as no size draws are required
sf_zero_catch <- filter(sf_catch_data, tot_sf_catch == 0)

#remove trips with zero summer flounder catch
#sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
sf_catch_data <- filter(sf_catch_data, tot_sf_catch > 0) 

#expand the sf_catch_data so that each row represents a fish
row_inds <- seq_len(nrow(sf_catch_data))
sf_catch_data <- sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
rownames(sf_catch_data) <- NULL
sf_catch_data$fishid <- 1:nrow(sf_catch_data)


# generate lengths for each fish
catch_size_data <- sf_catch_data %>% 
  mutate(fitted_length = sample(size_data$fitted_length,
                                nrow(.),
                                prob = size_data$fitted_prob,
                                replace = TRUE)) #%>%
##I()

# Impose regulations, calculate keep and release per trip
# For summer flounder, retain keep- and release-at-length


catch_size_data <- catch_size_data %>% 
  left_join(regs, by = "period") %>% 
  mutate(posskeep = ifelse(fitted_length>=fluke_min & fitted_length<=fluke_max,1,0)) %>% 
  #  data.table()
  #  pop <- pop[,lapply(.SD,cusum),by=list(period, catch_draw, tripid),.SDcols=c("posskeep2")] %>%
  # tibble() %>% rename(csum_keep = posskeep2)})
  group_by(period, catch_draw, tripid) %>%
  # keep = case_when(
  # fitted_length>=minsize & fitted_length<=maxsize ~ 1,
  # TRUE ~ 0),
  mutate(csum_keep = cumsum(posskeep)) %>%
  ungroup() %>%
  mutate(
    keep_adj = case_when(
      fluke_bag > 0 ~ ifelse(csum_keep<=fluke_bag & posskeep==1,1,0),
      TRUE ~ 0),
    # keep_adj = case_when(
    #   csum_keep<=bag & keep==1 ~ 1,
    #   TRUE ~ 0),
    release = case_when(
      fluke_bag > 0 ~ ifelse(posskeep==0 & csum_keep < fluke_bag, 1,0),
      TRUE ~ 1)
  )

# catch_size_data$keep = ifelse(catch_size_data$fitted_length>=minsize & catch_size_data$fitted_length<=maxsize, 1,0) 
# catch_size_data$csum_keep <- ave(catch_size_data$keep, catch_size_data$tripid, FUN=cumsum)
# catch_size_data$keep_adj = ifelse(catch_size_data$csum_keep<=bag & catch_size_data$keep==1, 1,0) 
# catch_size_data$release = ifelse(catch_size_data$keep_adj==1, 0,1) 

catch_size_data= subset(catch_size_data, select=c(period, catch_draw, fishid, fitted_length, tot_sf_catch, tot_bsb_catch, tripid, keep_adj, release)) %>% 
  rename(keep = keep_adj)
#names(catch_size_data)[names(catch_size_data) == "keep_adj"] = "keep"

# system.time({
new_size_data <- catch_size_data %>%
  group_by(period, catch_draw, tripid, fitted_length) %>%
  summarize(keep = sum(keep),
            release = sum(release), .groups = "drop") #%>%
# })
  #I()
# new_size_data <- catch_size_data %>% 
#   data.table() 
# new_size_data <- new_size_data[,lapply(.SD, sum), by=list(period, catch_draw, tripid, fitted_length),.SDcols=c("keep","release")] %>% 
#   tibble() #%>% 

# generate sum of number of kept and released fish by tripid
# system.time({
summed_catch_data <- catch_size_data %>%
  group_by(period, catch_draw, tripid) %>%
  summarize(tot_keep = sum(keep),
            tot_rel = sum(release),
            tot_bsb_catch = mean(tot_bsb_catch),
            .groups = "drop") #%>%
# })
# system.time({
# summed_catch_data <- catch_size_data %>% 
#   data.table()
# a1 <- summed_catch_data[,lapply(.SD, sum), by=list(period, catch_draw, tripid),.SDcols=c("keep","release")]
# a2 <- summed_catch_data[,lapply(.SD, mean), by=list(period, catch_draw, tripid),.SDcols=c("tot_bsb_catch")]
#   summed_catch_data <- a1 %>% 
#     tibble() %>% 
#     mutate(tot_bsb_catch = a2$tot_bsb_catch) %>% 
#   rename(tot_keep = keep,
#          tot_rel = release)
# })
  #I()
#catch_size_data$tot_keep=with(catch_size_data, ave(keep, tripid, FUN = sum))
#catch_size_data$tot_rel=with(catch_size_data, ave(release, tripid, FUN = sum))


# generate data set with total keep and release only
#catch_size_data1 = cbind(catch_size_data$tripid, catch_size_data$tot_keep, catch_size_data$tot_rel)
#colnames(catch_size_data1) = cbind("tripid", "tot_keep", "tot_rel")
#catch_size_data2 = catch_size_data1[!duplicated(catch_size_data1), ]


# generate data set with size of each fish kept and released
#keep_size_data= subset(catch_size_data, keep==1, select=c(fitted_length, tripid, keep, release))
#release_size_data= subset(catch_size_data, keep==0, select=c(fitted_length, tripid, keep, release))

#keep_size_data = subset(keep_size_data, select=c(fitted_length, tripid, keep))
#keep_size_data <- keep_size_data %>%
#  group_by(tripid, fitted_length) %>%
#  summarize(keep = sum(keep))
keep_size_data <- new_size_data %>%
  ungroup() %>%
  dplyr::select(-release) %>% 
  pivot_wider(names_from = fitted_length, #_length,
              names_glue = "keep_length_{fitted_length}",
              names_sort = TRUE,
              values_from = keep, 
              values_fill = 0) # %>% 
  #I()
#keep_size_data

release_size_data <- new_size_data %>%
  ungroup() %>% 
  dplyr::select(-keep) %>% 
  pivot_wider(names_from = fitted_length, #_length,
              names_glue = "release_length_{fitted_length}",
              names_sort = TRUE,
              values_from = release, 
              values_fill = 0) #%>% 
  #I()
#release_size_data

# names(keep_size_data)[names(keep_size_data) == "fitted_length"] = "keep_length"
# keep_size_data_wide <- spread(keep_size_data, keep_length, keep)
# colnames(keep_size_data_wide) = paste("keep_length",  colnames(keep_size_data_wide), sep="_")
# names(keep_size_data_wide)[names(keep_size_data_wide) == "keep_length_tripid"] = "tripid"
# keep_size_data_wide[is.na(keep_size_data_wide)] = 0
# 
# 
# release_size_data = subset(release_size_data, select=c(fitted_length, tripid, release))
# release_size_data <- release_size_data %>%
#   group_by(tripid, fitted_length) %>%
#   summarize(release = sum(release))
# 
# 
# names(release_size_data)[names(release_size_data) == "fitted_length"] = "release_length"
# release_size_data_wide <- spread(release_size_data, release_length, release)
# colnames(release_size_data_wide) = paste("release_length",  colnames(release_size_data_wide), sep="_")
# names(release_size_data_wide)[names(release_size_data_wide) == "release_length_tripid"] = "tripid"
# release_size_data_wide[is.na(release_size_data_wide)] = 0
# 
# 
# # merge the keep-/release-at-length files with the tot_keep/release file 
# trip_data =  merge(release_size_data_wide,keep_size_data_wide,by="tripid", all.x=TRUE, all.y=TRUE)
# trip_data =  merge(trip_data,catch_size_data2,by="tripid", all.x=TRUE, all.y=TRUE)

trip_data <- summed_catch_data %>% 
  left_join(keep_size_data, by = c("period", "catch_draw","tripid")) %>% 
  left_join(release_size_data, by = c("period", "catch_draw","tripid")) #%>% 
  #I()
#trip_data

#add the zero catch trips 
trip_data <- bind_rows(trip_data, sf_zero_catch) %>% 
  #arrange(period, catch_draw, tripid) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate_if(is.character, replace_na, replace = state1) %>% #region1) #%>% 
  #I()

#quick sort and cleanup 
# trip_data = trip_data[order(trip_data$tripid),] %>% 
#   replace_na(0) %>% 
#   I()
#rownames(trip_data) <- NULL

#trip_data[is.na(trip_data)] = 0
  mutate(tot_sf_catch = tot_keep + tot_rel)


# merge catch information for other species. Assume per-trip catch outcomes for these species are the same as the calibration. 
# This info is contained in the costs_new_all_state datasets
if (region1 == "NO") {
  bsb_sc_data=subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw, tripid,tot_keep_scup_base, tot_rel_scup_base)) %>%
    tibble()

  names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_scup_base"] = "tot_keep_scup"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_scup_base"] = "tot_rel_scup"
}

if (region1 == "NJ") {
  bsb_sc_data=subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw,tripid,tot_keep_scup_base, tot_rel_scup_base,
                                                                   tot_keep_wf_base, tot_rel_wf_base)) %>%
    tibble()

  names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_wf_base"] = "tot_keep_wf"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_wf_base"] = "tot_rel_wf"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_scup_base"] = "tot_keep_scup"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_scup_base"] = "tot_rel_scup"
}

if (state1 %in% c("DE","MD")) {
  bsb_sc_data=subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw,tripid,tot_keep_wf_base, tot_rel_wf_base)) %>%

     tibble()

  names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_wf_base"] = "tot_keep_wf"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_wf_base"] = "tot_rel_wf"
}

if (state1 %in% c("VA","NC")) {
  bsb_sc_data=subset(costs_new_all_MA, catch_draw<=nsamp, select=c(period, catch_draw, tripid,
                                                                   tot_keep_wf_base, tot_rel_wf_base,
                                                                   tot_keep_rd_base, tot_rel_rd_base)) %>%
    tibble()
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_wf_base"] = "tot_keep_wf"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_wf_base"] = "tot_rel_wf"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_rd_base"] = "tot_keep_rd"
  names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_rd_base"] = "tot_rel_rd"
}

#names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_bsb_base"] = "tot_keep_bsb"
#names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_bsb_base"] = "tot_rel_bsb"

# merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
#trip_data <-  merge(trip_data,bsb_sc_data,by="tripid") %>% 
dfs <- trip_data %>% 
  ungroup() %>% 
  left_join(bsb_sc_data, by = c("period","catch_draw","tripid")) %>% 
  # zap <- bsb_sc_data %>% data.table()
  # zip <- trip_data %>% ungroup() %>% data.table()
  # zop <- zap[zip,on=c("period","catch_draw","tripid")]
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  left_join(regs %>% dplyr::select(period, bsb_bag), by = c("period")) %>% 
  mutate(region = region1,
         keep_bsb = rbinom(nrow(.), tot_bsb_catch, prop_bsb_keep),   # GF adding BSB catch from draws to retain correlation
         tot_keep_bsb = ifelse(keep_bsb <= bsb_bag, keep_bsb, bsb_bag),
         tot_rel_bsb = tot_bsb_catch - tot_keep_bsb) %>%
  dplyr::select(-keep_bsb,-bsb_bag) %>% 
  dplyr::select(period, catch_draw, everything()) #%>% 
 # I()
#trip_data[is.na(trip_data)] = 0        


#    trip_data$catch_draw=i
#dfs <- trip_data
#return(dfs)

#dfs_all = as.data.frame(bind_rows(dfs[[1]], dfs[[2]],dfs[[3]],dfs[[4]],dfs[[5]],
#                                  dfs[[6]], dfs[[7]],dfs[[8]],dfs[[9]],dfs[[10]]))
# dfs_all <- dfs %>% #bind_rows(dfs) %>% 
#   #dfs_all <- purrr::map_dfr(1:10,get_catch_draws, sf_catch_data_all=sf_catch_data_all) %>% 
#   #arrange(period, tripid, catch_draw) %>% 
#   #mutate(period = rep(p,nrow(.))) %>% 
#   I()

# dfs_all[is.na(dfs_all)] = 0
# dfs_all <- dfs_all[order(dfs_all$tripid),]
# rownames(dfs_all) = NULL

#  dfs_all$period=p
#  return(dfs_all)
#pds[[p]] = dfs_all

#}

#pds_all <- dfs #_all

######################################
##   End simulating trip outcomes   ##
######################################

# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion
  
#for(p in levels(periodz)){

# Merge the prediction year data to the calibration data
#pds=subset(pds_all, period==p)
#pds <- pds_all

#  cost_data = subset(costs_new_all_MA, period == p, select=-c(period, tot_sf_catch))
#GF check here-- cost data is state-specific, need to locate correct costs in the next line
cost_data <- tibble(costs_new_all_MA) %>% dplyr::select(-tot_sf_catch)
#trip_data =  merge(pds,cost_data,by=c("tripid", "catch_draw"))
#trip_data <- left_join(pds,cost_data,by=c("period", "tripid", "catch_draw")) %>% 
trip_data <- left_join(dfs,cost_data,by=c("period", "tripid", "catch_draw")) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) #%>% 
#I()
#  trip_data[is.na(trip_data)] = 0


#set up an output file for each draw of utility parameters. For now, only taking one draw. 
parameter_draws = list()

#for(d in 1:1) {
d <- as.integer(1)
#dchoose = 1  
# Use the previously drawn set of utility parameters to calculate expected utility, welfare, and effort in the prediction year
#param_draws_MA_prediction = subset(param_draws_MA, parameter_draw=i)
param_draws_MA_prediction = param_draws_MA %>% tibble() %>% filter(parameter_draw==dchoose)
#trip_data =  merge(param_draws_MA_prediction,trip_data,by="tripid")
trip_data <- right_join(param_draws_MA_prediction,trip_data,by="tripid")

if (region1 %in% c("NO","NJ")) {
  trip_data <- trip_data %>% 
    # Expected utility (prediction year)
    mutate(vA = beta_sqrt_sf_keep*sqrt(tot_keep) +
             beta_sqrt_sf_release*sqrt(tot_rel) +  
             beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
             beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
             beta_sqrt_scup_keep*sqrt(tot_keep_scup) +
             beta_sqrt_scup_release*sqrt(tot_rel_scup) +    
             beta_cost*cost,
           # Expected utility (base year)
           v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
             beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
             beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
             beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
             beta_sqrt_scup_keep*sqrt(tot_keep_scup_base) +
             beta_sqrt_scup_release*sqrt(tot_rel_scup_base) +    
             beta_cost*cost)
  # # Expected utility (prediction year)
  # trip_data$vA = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep) +
  #   trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel) +  
  #   trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
  #   trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
  #   trip_data$beta_sqrt_scup_keep*sqrt(trip_data$tot_keep_scup) +
  #   trip_data$beta_sqrt_scup_release*sqrt(trip_data$tot_rel_scup) +    
  #   trip_data$beta_cost*trip_data$cost 
  # 
  # # Expected utility (base year)
  # trip_data$v0 = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf_base) +
  #   trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf_base) +  
  #   trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb_base) +
  #   trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb_base) +  
  #   trip_data$beta_sqrt_scup_keep*sqrt(trip_data$tot_keep_scup_base) +
  #   trip_data$beta_sqrt_scup_release*sqrt(trip_data$tot_rel_scup_base) +    
  #   trip_data$beta_cost*trip_data$cost 
}

if (region1 == "NJ") {
  trip_data <- trip_data %>% 
    mutate(vA = vA + beta_sqrt_wf_keep*sqrt(tot_keep_wf) +
             beta_sqrt_wf_release*sqrt(tot_rel_wf),
    v0 = v0 + beta_sqrt_wf_keep*sqrt(tot_keep_wf_base) +
    beta_sqrt_wf_release*sqrt(tot_rel_wf_base))       
  # trip_data$vA = trip_data$vA + 
  #   trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf) +
  #   trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf)
  # trip_data$v0 = trip_data$v0 + 
  #   trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf_base) +
  #   trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf_base)       
}


if (state1 %in% c("DE","MD")) {
  # Expected utility (prediction year)
  trip_data <- trip_data %>% 
    mutate(
      vA = beta_sqrt_sf_keep*sqrt(tot_keep) +
        beta_sqrt_sf_release*sqrt(tot_rel) +  
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
        beta_sqrt_wf_keep*sqrt(tot_keep_wf) +
        beta_sqrt_wf_release*sqrt(tot_rel_wf) +
        beta_cost*cost, 
    # trip_data$vA = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep) +
    # trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel) +  
    # trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
    # trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
    # trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf) +
    # trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf) +
    # trip_data$beta_cost*trip_data$cost 
  
  # Expected utility (base year)
  v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
    beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
    beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
    beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
    beta_sqrt_wf_keep*sqrt(tot_keep_wf_base) +
    beta_sqrt_wf_release*sqrt(tot_rel_wf_base) +       
    beta_cost*cost)
    # trip_data$v0 = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf_base) +
    # trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf_base) +  
    # trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb_base) +
    # trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb_base) +  
    # trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf_base) +
    # trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf_base) +       
    # trip_data$beta_cost*trip_data$cost 
}

if (state1 %in% c("VA","NC")) {
  # Expected utility (prediction year)
  trip_data <- trip_data %>% 
    mutate(
    vA = beta_sqrt_sf_keep*sqrt(tot_keep) +
    beta_sqrt_sf_release*sqrt(tot_rel) +  
    beta_sqrt_bsb_keep*sqrt(tot_keep_bsb) +
    beta_sqrt_bsb_release*sqrt(tot_rel_bsb) +  
    beta_sqrt_wf_keep*sqrt(tot_keep_wf) +
    beta_sqrt_wf_release*sqrt(tot_rel_wf) +   
    beta_sqrt_rd_keep*sqrt(tot_keep_rd) +
    beta_sqrt_rd_release*sqrt(tot_rel_rd) +   
    beta_cost*cost,
    # trip_data$vA = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep) +
    #   trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel) +  
    #   trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
    #   trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
    #   trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf) +
    #   trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf) +   
    #   trip_data$beta_sqrt_rd_keep*sqrt(trip_data$tot_keep_rd) +
    #   trip_data$beta_sqrt_rd_release*sqrt(trip_data$tot_rel_rd) +   
    #   trip_data$beta_cost*trip_data$cost 
    # 
  # Expected utility (base year)
  v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
    beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +  
    beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
    beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +  
    beta_sqrt_wf_keep*sqrt(tot_keep_wf_base) +
    beta_sqrt_wf_release*sqrt(tot_rel_wf_base) + 
    beta_sqrt_rd_keep*sqrt(tot_keep_rd_base) +
    beta_sqrt_rd_release*sqrt(tot_rel_rd_base) + 
    beta_cost*cost)
  # trip_data$v0 = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf_base) +
  #   trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf_base) +  
  #   trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb_base) +
  #   trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb_base) +  
  #   trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf_base) +
  #   trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf_base) + 
  #   trip_data$beta_sqrt_rd_keep*sqrt(trip_data$tot_keep_rd_base) +
  #   trip_data$beta_sqrt_rd_release*sqrt(trip_data$tot_rel_rd_base) + 
  #   trip_data$beta_cost*trip_data$cost
}

#trip_data$period <- as.numeric(trip_data$period)
trip_data <- trip_data %>% 
  mutate(period = as.numeric(period))

# Collapse data from the X catch draws so that each row contains mean values
#mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
mean_trip_data <- trip_data %>% 
  dplyr::select(-region) %>% 
  data.table()
mean_trip_data <- mean_trip_data[, lapply(.SD, mean), by=list(period,tripid)] %>% 
  tibble() #%>% 
#group_by(period, tripid) %>% 
#summarise_if(is.numeric,.funs = c("mean"), na.rm = TRUE, .groups = "drop") %>% 
#ungroup()
# summarise_all(.funs = c("mean"), na.rm = TRUE, .groups = "drop") %>% 
# ungroup() %>% 
#  mutate(region = rep(region1,nrow(.)))

#summarise_all(.funs = c("mean"), na.rm = TRUE, .groups = "drop")

# Now expand the data to create three alternatives, representing the alternatives available in choice survey
#mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
mean_trip_data <- mean_trip_data %>% 
  mutate(n_alt = rep(3,nrow(.))) %>% 
  uncount(n_alt) %>% 
  mutate(alt = rep(1:3,nrow(.)/3),
         opt_out = ifelse(alt == 3, 1, 0),
         striper_blue = ifelse(alt == 2, 1, 0))

#Alt 1, 2, 3
#mean_trip_data$alt <- sequence(tabulate(mean_trip_data$tripid))

# #Alt 2 and 3 are the opt_out and other_fishing alternatives
# #mean_trip_data$opt_out = ifelse(mean_trip_data$alt==3, 1,0)
# mean_trip_data$opt_out = 0
# mean_trip_data$opt_out[mean_trip_data$alt==3] = 1
# #mean_trip_data$striper_blue = ifelse(mean_trip_data$alt==2, 1,0) 
# mean_trip_data$striper_blue = 0
# mean_trip_data$striper_blue[mean_trip_data$alt==2] = 1

#Caluculate the expected utility of alts 2 and 3 based on the parameters of the utility function
#These will be the same for both v0 and v1
# mean_trip_data$vA_optout <- mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
# mean_trip_data$vA_striper_blue <- mean_trip_data$beta_striper_blue*mean_trip_data$striper_blue 
mean_trip_data <- mean_trip_data %>% 
  mutate(vA_optout = beta_opt_out*opt_out,
         vA_striper_blue = beta_striper_blue*striper_blue + beta_cost*cost,
         vA = ifelse(alt==1,vA,0),
         v0 = ifelse(alt==1,v0,0))
#Now put these three values in the same column, exponentiate, and calculate their sum (vA_col_sum)
# mean_trip_data$vA[mean_trip_data$alt!=1] <- 0
# mean_trip_data$v0[mean_trip_data$alt!=1] <- 0

mean_trip_data <- mean_trip_data %>% 
  group_by(period, tripid) %>% 
  mutate(vA_row_sum = exp(vA + vA_striper_blue + vA_optout),
         vA_col_sum = sum(vA_row_sum),
         v0_row_sum = exp(v0 + vA_striper_blue + vA_optout),
         v0_col_sum = sum(v0_row_sum)) %>% 
  ungroup()
# mean_trip_data$vA_row_sum = rowSums(mean_trip_data[,c("vA", "vA_striper_blue","vA_optout")])
# mean_trip_data$vA_row_sum = exp(mean_trip_data$vA_row_sum)
# mean_trip_data$vA_col_sum = ave(mean_trip_data$vA_row_sum, mean_trip_data$tripid, FUN = sum)

# mean_trip_data$v0_row_sum = rowSums(mean_trip_data[,c("v0", "vA_striper_blue","vA_optout")])
# mean_trip_data$v0_row_sum = exp(mean_trip_data$v0_row_sum)
# mean_trip_data$v0_col_sum = ave(mean_trip_data$v0_row_sum, mean_trip_data$tripid, FUN = sum)


#change in Consmer surplus between prediction year and baseline year 
#mean_trip_data$change_CS = (1/mean_trip_data$beta_cost)*(log(mean_trip_data$vA_col_sum) - log(mean_trip_data$v0_col_sum))
mean_trip_data <- mean_trip_data %>% 
  mutate(change_CS = (1/beta_cost)*(log(vA_col_sum) - log(v0_col_sum))) %>% 

# Calculate the probability of a respondent selected each alternative based on 
# exponentiated expected utility of the alternative [exp(expected utility, alt=i] 
# and the sum of exponentiated expected utility across the three altenratives.
# You will notice the striper_blue alternative has a large probability based on the utility parameters
#mean_trip_data$probA = mean_trip_data$vA_row_sum/mean_trip_data$vA_col_sum
#mean_trip_data$prob0 = mean_trip_data$v0_row_sum/mean_trip_data$v0_col_sum
  mutate(probA = vA_row_sum/vA_col_sum,
         prob0 = v0_row_sum/v0_col_sum)

# Get rid of things we don't need. 
if (region1 == "NO") {
  mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                            beta_cost, beta_striper_blue, beta_opt_out, beta_sqrt_scup_release, beta_sqrt_scup_keep,
                                                            beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep))
}
if (region1 == "NJ") {
  mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                            beta_cost, beta_striper_blue, beta_opt_out, beta_sqrt_scup_release, beta_sqrt_scup_keep,
                                                            beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep,
                                                            beta_sqrt_wf_release,beta_sqrt_wf_keep))
}
if (state1 %in% c("DE","MD")) {
  mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                            beta_cost, beta_striper_blue, beta_opt_out,
                                                            beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep,
                                                            beta_sqrt_wf_release,beta_sqrt_wf_keep))
}
if (state1 %in% c("VA","NC")) {
  mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                            beta_cost, beta_striper_blue, beta_opt_out,
                                                            beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep, 
                                                            beta_sqrt_wf_release, beta_sqrt_wf_keep, beta_sqrt_rd_release, beta_sqrt_rd_keep))
}

# Multiply the average trip probability by each of the catch variables (not the variable below) to get probability-weighted catch
list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                      & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period" #& colnames(mean_trip_data) !="cost" 
                                      & colnames(mean_trip_data) !="tot_keep_sf_base" & colnames(mean_trip_data) !="tot_rel_sf_base" 
                                      & colnames(mean_trip_data) !="tot_keep_bsb_base" & colnames(mean_trip_data) !="tot_rel_bsb_base" 
                                      & colnames(mean_trip_data) !="tot_keep_scup_base" & colnames(mean_trip_data) !="tot_rel_scup_base"
                                      & colnames(mean_trip_data) !="tot_keep_wf_base" & colnames(mean_trip_data) !="tot_rel_wf_base" 
                                      & colnames(mean_trip_data) !="tot_keep_rd_base" & colnames(mean_trip_data) !="tot_rel_rd_base"
                                      & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"  & colnames(mean_trip_data) !="probA"
                                      & colnames(mean_trip_data) !="prob0" & colnames(mean_trip_data) !="change_CS"  ]    

#mean_trip_data <- 
mean_trip_data <- mean_trip_data %>% 
  filter(!is.na(probA)) %>% 
  mutate(chosen = rbinom(nrow(.), 1, prob = probA)) %>% 
  mutate(across(.cols = all_of(list_names),.fns=function(x) chosen*x),
         n_choice_occasions = rep(1,nrow(.)))
  #filter(chosen == 1) 

# for (l in list_names){
#   mean_trip_data[,l] = mean_trip_data[,l]*mean_trip_data$probA
# }
#mean_trip_data[,list_names] <- mean_trip_data$probA*mean_trip_data[,list_names]
#mean_trip_data[,list_names] <- system.time({mean_trip_data$chosen*mean_trip_data[,list_names]})

# ntrips = 10000
# xx <- tibble(trip = rep(1:ntrips,each=3),
#              choice=rep(1:3,ntrips), 
#              p1=rep(c(0.1,0.3,0.6),ntrips)) %>%
#   mutate(u=rnorm(nrow(.),0,0.5),
#          p2 = p1*exp(u)) %>%
#   group_by(trip) %>%
#   mutate(x = rmultinom(1,1,p2)) %>%
#   ungroup()
# yy <- xx %>%
#   filter(x == 1) %>%
#   count(choice)
# yy


# Make a variable "n_choice_occasions" for number of choice occasion, which, when expanded by the expansion factor, should equal the 
# number of choice occasion used in the calibration year
#mean_trip_data$n_choice_occasions <- 1


#Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
#mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
sims=subset(calibration_data, sim==d) %>% 
  left_join(mean_trip_data %>% count(period, name = "ndraws") %>% mutate(period = as.character(period)), by = "period") %>% 
  mutate(expand = n_choice_occasions/ndraws)
#n_choice_occasions = mean(sims$n_choice_occasions)
#ndraws = nrow(mean_trip_data)
#expand=n_choice_occasions/ndraws


mean_trip_data$sim=1
mean_trip_data <- mean_trip_data %>% mutate(keep_one = ifelse(tot_keep>0,1,0))

#sum probability weighted catch over all choice occasions
#aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
aggregate_trip_data <- mean_trip_data %>% 
  group_by(period, sim) %>% 
  summarize_all(sum, na.rm = TRUE) %>% 
  #left_join(sims %>% mutate(period = as.numeric(period)), by = c("period","sim"))
  right_join(sims %>% mutate(period = as.numeric(period)) %>% dplyr::select(-n_choice_occasions),
             by = c("period","sim"))


list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                      & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period"
                                      & colnames(mean_trip_data) !="tot_keep_sf_base" & colnames(mean_trip_data) !="tot_rel_sf_base" 
                                      & colnames(mean_trip_data) !="tot_keep_bsb_base" & colnames(mean_trip_data) !="tot_rel_bsb_base" 
                                      & colnames(mean_trip_data) !="tot_keep_scup_base" & colnames(mean_trip_data) !="tot_rel_scup_base" 
                                      & colnames(mean_trip_data) !="tot_keep_wf_base" & colnames(mean_trip_data) !="tot_rel_wf_base" 
                                      & colnames(mean_trip_data) !="tot_keep_rd_base" & colnames(mean_trip_data) !="tot_rel_rd_base"
                                      & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"
                                      & colnames(mean_trip_data) != "state" 
                                      & colnames(mean_trip_data) != "ndraws" 
                                      & colnames(mean_trip_data) != "expand" ]
#& colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"


# for (l in list_names){
#   mean_trip_data[,l] = mean_trip_data[,l]*expand
# }
#aggregate_trip_data[,list_names] <- aggregate_trip_data$expand*aggregate_trip_data[,list_names]

aggregate_trip_data <- aggregate_trip_data %>% 
  mutate(across(.cols = all_of(list_names),.fns=function(x) expand*x))

# #This equals the observed number of trips under the new conditions
# sum(mean_trip_data$probA)
# 
# #This should equal the observed # of trips in that period of the baseline year
# sum(mean_trip_data$prob0)

#    aggregate_trip_data$n_choice_occasions = n_choice_occasions


#aggregate_trip_data = subset(aggregate_trip_data, select=-c(tripid, catch_draw, period, cost, vA , v0, sim))
aggregate_trip_data = subset(aggregate_trip_data, select=-c(tripid, catch_draw, period, vA , v0, sim))
#names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "observed_trips"
aggregate_trip_data <- rename(aggregate_trip_data, observed_trips = chosen) %>% 
  mutate(sim = rep(d,nrow(.)))

#aggregate_trip_data$sim = d

#    parameter_draws[[d]]=aggregate_trip_data

# }
#   
#   parameter_draws_all = as.data.frame(bind_rows(parameter_draws[[1]]))
#   
#   
#   #parameter_draws_all[is.na(parameter_draws_all)] = 0
#   rownames(parameter_draws_all) = NULL
#   
#   
#   parameter_draws_all$period=p
#   pds_new[[p]]=parameter_draws_all
#   
# }
#pds_new <- aggregate_trip_data

pds_new_all_MA <- aggregate_trip_data %>%  #list.stack(pds_new, fill=TRUE) %>% 
  mutate_at(vars(contains("length")), replace_na, replace = 0) %>% 
#pds_new_all_MA[is.na(pds_new_all_MA)] = 0
  mutate(state = state1,
         region = region1,
         alt_regs = 1)
#  pds_new_all_MA$state = state1
#pds_new_all_MA$region = region1
#pds_new_all_MA$alt_regs = 1
# pds_new_all_MA=subset(pds_new_all_MA, select=-c(Group.1, tot_keep_sf_base, tot_rel_sf_base, 
#                                                 tot_keep_scup_base, tot_rel_scup_base, 
#                                                 tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
if (region1 == "NO") {
pds_new_all_MA=subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                tot_keep_scup_base, tot_rel_scup_base, 
                                                tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}
if (region1 == "NJ") {
  pds_new_all_MA=subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                  tot_keep_scup_base, tot_rel_scup_base, 
                                                  tot_keep_wf_base, tot_rel_wf_base, 
                                                  tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}
if (state1 %in% c("DE","MD")) {
  pds_new_all_MA=subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                  tot_keep_wf_base, tot_rel_wf_base, 
                                                  tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}
if (state1 %in% c("VA","NC")) {
pds_new_all_MA=subset(pds_new_all_MA, select=-c(tot_keep_sf_base, tot_rel_sf_base, 
                                                tot_keep_wf_base, tot_rel_wf_base, tot_keep_rd_base, tot_rel_rd_base,
                                                tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))
}

#})
# write_xlsx(pds_new_all_MA,"MA_prediction_output_check.xlsx")
return(pds_new_all_MA)

#end function
}