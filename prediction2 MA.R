

###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By region (MA-NY, NJ, DE-NC): catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 


# This code requires the following data:
# 1) The output from the calibration: calibration_output_by_period.xlsx
# 2) Dataset containing the alternative regulations to be imposed: directed_trips_region - alternative regs test.xlsx
# 3) Abundance adjusted catch-at-length for summer flounder. This is an output file from the script catch at length given stock structure - prediction.R
#    For now, the catch-at-length disitributions in this file are identical to the calibration catch-at-lengths: sf_fitted_sizes_y2plus.xlsx
# 4) Set of utility parameters draws from one of the four surveys, for MA-NY states: utility_param_draws_MA_NY.xlsx


state1="MA"
region1="NO"


# Input the calibration output which contains the number of choice occasions needed to simulate
calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
calibration_data = subset(calibration_data, state == state1, select=c(period, sim, state, n_choice_occasions))


# Input the data set containing alterntative regulations and directed trips (directed_trips_region - alternative regs test.xlsx)
directed_trips = data.frame(read_excel("directed_trips_region - alternative regs test.xlsx"))                                                                            
directed_trips$dtrip=round(directed_trips$dtrip)
directed_trips= subset(directed_trips, state == state1)

min_period=min(directed_trips$period)
max_period=max(directed_trips$period)


######################################
##   Begin simulating trip outcomes ##
######################################

# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()

for (p in min_period:max_period) {
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip)
  n_draws = min(10000,n_trips*2.5 )
  fluke_bag = mean(directed_trips_p$fluke_bag)
  fluke_min = mean(directed_trips_p$fluke_min)
  fluke_max = mean(directed_trips_p$fluke_max)
  bsb_bag = mean(directed_trips_p$bsb_bag)
  bsb_min = mean(directed_trips_p$bsb_min)
  scup_bag = mean(directed_trips_p$scup_bag)
  scup_min = mean(directed_trips_p$scup_min)
  
  
  # Set up an output file for the separate catch draw files 
  dfs = list()
  
  for(i in 1:10) {
    
    # Input catch-per-trip numbers 
    # Catch-per-trip for all species remains the same. We extract that info from costs_new_all_state.
    # The size of summer flounder caught, however, changes accordaning to the 
    # population-based catch-at-length distribution, which is contained in sf_fitted_sizes_y2plus.xlsx.
    # Keep and release of summer changes according to the newly drawn sizes and regulations.
    
    sf_catch_data = subset(costs_new_all_MA, period ==p & catch_draw==i, select=c(tripid, tot_sf_catch))
 
    
    # subset trips with zero sf catch
    sf_zero_catch = subset(sf_catch_data, tot_sf_catch == 0)
    
    
    #remove trips with zero summer flounder catch, will add them in later
    sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(sf_catch_data))
    sf_catch_data = sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
    rownames(sf_catch_data) = NULL
    sf_catch_data$fishid = 1:nrow(sf_catch_data)
    

    #import and expand the population numbers-adjusted sf_size_data so that each row represents a fish
    size_data_sf = data.frame(read_excel("sf_fitted_sizes_y2plus.xlsx"))
    size_data_sf = subset(size_data_sf, region==region1, select= c(fitted_length, fitted_prob))
    size_data_sf$nfish = round(100000 * size_data_sf$fitted_prob, digits=0)
    sum(size_data_sf$nfish)
    
    row_inds <- seq_len(nrow(size_data_sf))
    size_data_sf <- size_data_sf[c(rep(row_inds, size_data_sf$nfish)), ]
    rownames(size_data_sf) = NULL
    size_data_sf = subset(size_data_sf, select= fitted_length)
    
    
    #draw random sample of sizes matching the number of fish caught
    random_sizes  = data.frame(size_data_sf[sample(nrow(size_data_sf), nrow(sf_catch_data)), ])
    colnames(random_sizes) = "fitted_length"
    random_sizes$fishid = 1:nrow(random_sizes)
    catch_size_data =  merge(random_sizes,sf_catch_data,by="fishid")
    
    
    # Impose regulations, calculate keep and release per trip
    # For summer flounder, retain keep- and release-at-length
    
    bag = fluke_bag
    minsize = fluke_min
    maxsize = fluke_max
    catch_size_data$keep = ifelse(catch_size_data$fitted_length>=minsize & catch_size_data$fitted_length<=maxsize, 1,0) 
    catch_size_data$csum_keep <- ave(catch_size_data$keep, catch_size_data$tripid, FUN=cumsum)
    catch_size_data$keep_adj = ifelse(catch_size_data$csum_keep<=bag & catch_size_data$keep==1, 1,0) 
    catch_size_data$release = ifelse(catch_size_data$keep_adj==1, 0,1) 
    
    catch_size_data= subset(catch_size_data, select=c(fishid, fitted_length, tot_sf_catch, tripid, keep_adj, release))
    names(catch_size_data)[names(catch_size_data) == "keep_adj"] = "keep"
    
    
    # generate sum of number of kept and released fish by tripid
    catch_size_data$tot_keep=with(catch_size_data, ave(keep, tripid, FUN = sum))
    catch_size_data$tot_rel=with(catch_size_data, ave(release, tripid, FUN = sum))
    
    
    # generate data set with total keep and release only
    catch_size_data1 = cbind(catch_size_data$tripid, catch_size_data$tot_keep, catch_size_data$tot_rel)
    colnames(catch_size_data1) = cbind("tripid", "tot_keep", "tot_rel")
    catch_size_data2 = catch_size_data1[!duplicated(catch_size_data1), ]
    
    
    # generate data set with size of each fish kept and released
    keep_size_data= subset(catch_size_data, keep==1, select=c(fitted_length, tripid, keep, release))
    release_size_data= subset(catch_size_data, keep==0, select=c(fitted_length, tripid, keep, release))
    
    keep_size_data = subset(keep_size_data, select=c(fitted_length, tripid, keep))
    keep_size_data <- keep_size_data %>%
      group_by(tripid, fitted_length) %>%
      summarize(keep = sum(keep))
    
    
    names(keep_size_data)[names(keep_size_data) == "fitted_length"] = "keep_length"
    keep_size_data_wide <- spread(keep_size_data, keep_length, keep)
    colnames(keep_size_data_wide) = paste("keep_length",  colnames(keep_size_data_wide), sep="_")
    names(keep_size_data_wide)[names(keep_size_data_wide) == "keep_length_tripid"] = "tripid"
    keep_size_data_wide[is.na(keep_size_data_wide)] = 0
    
    
    release_size_data = subset(release_size_data, select=c(fitted_length, tripid, release))
    release_size_data <- release_size_data %>%
      group_by(tripid, fitted_length) %>%
      summarize(release = sum(release))
    
    
    names(release_size_data)[names(release_size_data) == "fitted_length"] = "release_length"
    release_size_data_wide <- spread(release_size_data, release_length, release)
    colnames(release_size_data_wide) = paste("release_length",  colnames(release_size_data_wide), sep="_")
    names(release_size_data_wide)[names(release_size_data_wide) == "release_length_tripid"] = "tripid"
    release_size_data_wide[is.na(release_size_data_wide)] = 0
    
    
    # merge the keep-/release-at-length files with the tot_keep/release file 
    trip_data =  merge(release_size_data_wide,keep_size_data_wide,by="tripid", all.x=TRUE, all.y=TRUE)
    trip_data =  merge(trip_data,catch_size_data2,by="tripid", all.x=TRUE, all.y=TRUE)
    
    
    #add the zero catch trips 
    trip_data = bind_rows(trip_data, sf_zero_catch)
    
    #quick sort and cleanup 
    trip_data = trip_data[order(trip_data$tripid),]
    rownames(trip_data) <- NULL
    
    trip_data[is.na(trip_data)] = 0
    trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
    
    # merge catch information for other species. Assume per-trip catch outcomes for these species are the same as the calibration. 
    # This info is contained in the costs_new_all_state datasets
    bsb_sc_data=subset(costs_new_all_MA, period ==p & catch_draw==i, select=c(tripid,tot_keep_scup_base, tot_rel_scup_base, 
                                                                              tot_keep_bsb_base, tot_rel_bsb_base)) 
    
    names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_bsb_base"] = "tot_keep_bsb"
    names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_bsb_base"] = "tot_rel_bsb"
    names(bsb_sc_data)[names(bsb_sc_data) == "tot_keep_scup_base"] = "tot_keep_scup"
    names(bsb_sc_data)[names(bsb_sc_data) == "tot_rel_scup_base"] = "tot_rel_scup"
    
    # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
    trip_data =  merge(trip_data,bsb_sc_data,by="tripid")
    trip_data[is.na(trip_data)] = 0        
    
    
    trip_data$catch_draw=i
    dfs[[i]]=trip_data
   
    
  }
  
  dfs_all = as.data.frame(bind_rows(dfs[[1]], dfs[[2]],dfs[[3]],dfs[[4]],dfs[[5]],
                                    dfs[[6]], dfs[[7]],dfs[[8]],dfs[[9]],dfs[[10]]))

  
  dfs_all[is.na(dfs_all)] = 0
  dfs_all <- dfs_all[order(dfs_all$tripid),]
  rownames(dfs_all) = NULL
  
  dfs_all$period=p
  pds[[p]] = dfs_all
}


######################################
##   End simulating trip outcomes   ##
######################################

# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion

pds_new = list()
for (p in min_period:max_period) {
  
  # Merge the prediction year data to the calibration data
  cost_data = subset(costs_new_all_MA, period == p, select=-c(period, tot_sf_catch))
  trip_data =  merge(pds[[p]],cost_data,by=c("tripid", "catch_draw"))
  trip_data[is.na(trip_data)] = 0

  
  #set up an output file for each draw of utility parameters. For now, only taking one draw. 
  parameter_draws = list()
  
  for(d in 1:1) {
    
    #Import utility parameter draws
    param_draws_MANY = data.frame(read_excel("utility_param_draws_MA_NY.xlsx"))                                                                            
    param_draws_MANY1 = subset(param_draws_MANY, n==d)
    
    # Expected utility (prediction year)
    trip_data$vA = param_draws_MANY1$sqrt_sf_keep*sqrt(trip_data$tot_keep) +
      param_draws_MANY1$sqrt_sf_release*sqrt(trip_data$tot_rel) +
      param_draws_MANY1$sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
      param_draws_MANY1$sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +
      param_draws_MANY1$sqrt_scup_keep*sqrt(trip_data$tot_keep_scup) +
      param_draws_MANY1$sqrt_scup_release*sqrt(trip_data$tot_rel_scup) +
      param_draws_MANY1$cost*trip_data$cost
    
    # Expected utility (baseline year)
    trip_data$v0 = param_draws_MANY1$sqrt_sf_keep*sqrt(trip_data$tot_keep_sf_base) +
      param_draws_MANY1$sqrt_sf_release*sqrt(trip_data$tot_rel_sf_base) +
      param_draws_MANY1$sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb_base) +
      param_draws_MANY1$sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb_base) +
      param_draws_MANY1$sqrt_scup_keep*sqrt(trip_data$tot_keep_scup_base) +
      param_draws_MANY1$sqrt_scup_release*sqrt(trip_data$tot_rel_scup_base) +
      param_draws_MANY1$cost*trip_data$cost
    
    
    # Collapse data from the X catch draws so that each row contains mean values
    mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
    
    #Trip probabiltiy (prediction year), based on expected utility
    mean_trip_data$probA = exp(mean_trip_data$vA)/(1+exp(mean_trip_data$vA))
    
    #Trip probabiltiy (baseline year), based on expected utility
    mean_trip_data$prob0 = exp(mean_trip_data$v0)/(1+exp(mean_trip_data$v0))
    
    #change in Consmer surplus between prediction year and baseline year 
    mean_trip_data$change_CS = (1/param_draws_MANY1$cost)*(log(exp(mean_trip_data$vA)) - log(exp(mean_trip_data$v0)))
    

    # Multiply the average trip probability by each of the catch variables (not the variable below) to get probability-weighted catch
    list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                          & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period" & colnames(mean_trip_data) !="cost" 
                                          & colnames(mean_trip_data) !="tot_keep_sf_base" & colnames(mean_trip_data) !="tot_rel_sf_base" 
                                          & colnames(mean_trip_data) !="tot_keep_bsb_base" & colnames(mean_trip_data) !="tot_rel_bsb_base" 
                                          & colnames(mean_trip_data) !="tot_keep_scup_base" & colnames(mean_trip_data) !="tot_rel_scup_base" 
                                          & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"  & colnames(mean_trip_data) !="probA"
                                          & colnames(mean_trip_data) !="prob0" & colnames(mean_trip_data) !="change_CS"  ]
    
    for (l in list_names){
      mean_trip_data[,l] = mean_trip_data[,l]*mean_trip_data$probA
    }
    
    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    sims=subset(calibration_data, period==p & sim==d)
    n_choice_occasions = mean(sims$n_choice_occasions)
    ndraws = nrow(mean_trip_data)
    expand=n_choice_occasions/ndraws
    
    list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                          & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period"
                                          & colnames(mean_trip_data) !="tot_keep_sf_base" & colnames(mean_trip_data) !="tot_rel_sf_base" 
                                          & colnames(mean_trip_data) !="tot_keep_bsb_base" & colnames(mean_trip_data) !="tot_rel_bsb_base" 
                                          & colnames(mean_trip_data) !="tot_keep_scup_base" & colnames(mean_trip_data) !="tot_rel_scup_base" 
                                          & colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"]
    
    for (l in list_names){
      mean_trip_data[,l] = mean_trip_data[,l]*expand
  }
    
    #This equals the observed number of trips under the new conditions
    sum(mean_trip_data$probA)
    
    #This should equal the observed # of trips in that period of the baseline year
    sum(mean_trip_data$prob0)
    
    mean_trip_data$sim=1
    
    #sum probability weighted catch over all choice occasions
    aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
    aggregate_trip_data$n_choice_occasions = n_choice_occasions
    
    
    aggregate_trip_data = subset(aggregate_trip_data, select=-c(Group.1, Group.1, tripid, catch_draw, period, cost, vA , v0, sim))
    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "observed_trips"
    names(aggregate_trip_data)[names(aggregate_trip_data) == "prob0"] = "observed_trips_base"
    
    
    aggregate_trip_data$sim = d
    
    parameter_draws[[d]]=aggregate_trip_data
    
   }

    parameter_draws_all = as.data.frame(bind_rows(parameter_draws[[1]]))


    parameter_draws_all[is.na(parameter_draws_all)] = 0
    rownames(parameter_draws_all) = NULL


    parameter_draws_all$period=p
    pds_new[[p]]=parameter_draws_all

}


pds_new_all_MA = as.data.frame(bind_rows(pds_new[[2]],pds_new[[3]],pds_new[[4]]))
pds_new_all_MA[is.na(pds_new_all_MA)] = 0
pds_new_all_MA$state = state1
pds_new_all_MA$alt_regs = 1
pds_new_all_MA=subset(pds_new_all_MA, select=-c(Group.1, tot_keep_sf_base, tot_rel_sf_base, 
                                                tot_keep_scup_base, tot_rel_scup_base, 
                                                tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))


# write_xlsx(pds_new_all_MA,"MA_prediction_output_check.xlsx")


