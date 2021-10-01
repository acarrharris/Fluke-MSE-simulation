
###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By region (MA-NY, NJ, DE-NC): catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 

# This code requires the following data:
# 1) Directed summer flounder by regualtory period in the baseline year: directed_trips_region.xlsx
# 2) Distribution of trip costs by mode in each state: trip_costs_NE.xlsx
# 3) Catch-per-trip of SF and BSB from the copula model: MANY_catch_data_sim1.xlsx
# 4) Catch-at-length for each of the species included in the models: fitted_sizes_region_raw.xlsx
# 5) Catch-per-trip of species other than SF and BSB: other_species_fitted_catch.xlsx
# 6) Set of utility parameters draws from one of the four surveys, for MA-NY states: utility_param_draws_MA_NY.xlsx

state1="NJ"
region1="NJ"


######################################
##   Begin simulating trip outcomes ##
######################################


#Import directed trips file - gives directed trips by regulatory period in 2019
directed_trips = data.frame(read_excel("directed_trips_region.xlsx"))                                                                            
directed_trips$dtrip=round(directed_trips$dtrip)
directed_trips= subset(directed_trips, state == state1)

min_period=min(directed_trips$period)
max_period=max(directed_trips$period)


# Import trip cost data
trip_cost_data = data.frame(read_excel("trip_costs_NE.xlsx"))                                                                            
trip_cost_data = subset(trip_cost_data, state == state1)
rownames(trip_cost_data) = NULL

trip_cost_data_shore = subset(trip_cost_data, mode == "shore")
trip_cost_data_privt = subset(trip_cost_data, mode == "privt")
trip_cost_data_headb = subset(trip_cost_data, mode == "headb")
trip_cost_data_chart = subset(trip_cost_data, mode == "chart")


# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()

for (p in min_period:max_period) {
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip)
  n_draws = min(10000,n_trips*2.5 )
  fluke_bag = mean(directed_trips_p$fluke_bag)
  fluke_min = mean(directed_trips_p$fluke_min)
  bsb_bag = mean(directed_trips_p$bsb_bag)
  bsb_min = mean(directed_trips_p$bsb_min)
  scup_bag = mean(directed_trips_p$scup_bag)
  scup_min = mean(directed_trips_p$scup_min)
  wf_bag = mean(directed_trips_p$wf_bag)
  wf_min = mean(directed_trips_p$wf_min) 
  
  # Set up an output file for catch draw files 
  dfs = list()
  
  #Run the catch loop X times to give X draws of catch for each species
  for(i in 1:10) {
    # Input catch-per-trip numbers 
    sf_catch_data = data.frame(read_excel("NJ_catch_data_sim1.xlsx"))                                                                            
    tot_sf_catch = sf_catch_data$sf_t_nb
    tot_bsb_catch = sf_catch_data$bsb_t_nb
    sf_catch_data = data.frame(tot_sf_catch,tot_bsb_catch)
    
    # random draw of fluke and bsb catch
    sf_catch_data = as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_draws), ])
    sf_catch_data$tripid = 1:nrow(sf_catch_data)
    
    
    # subset trips with zero catch, as no size draws are required
    sf_zero_catch = subset(sf_catch_data, tot_sf_catch == 0)
    
    
    #remove trips with zero summer flounder catch, will add them on later
    sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(sf_catch_data))
    sf_catch_data = sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
    rownames(sf_catch_data) = NULL
    sf_catch_data$fishid = 1:nrow(sf_catch_data)
    
    
    #import and expand the sf_size_data so that each row represents a fish
    size_data = data.frame(read_excel("fitted_sizes_region_raw.xlsx"))
    size_data_sf = subset(size_data, region==region1 & species=="summerflounder", select= c(fitted_length, fitted_prob))
    size_data_sf$nfish = round(100000 * size_data_sf$fitted_prob, digits=0)
    sum(size_data_sf$nfish)
    
    row_inds <- seq_len(nrow(size_data_sf))
    size_data_sf <- size_data_sf[c(rep(row_inds, size_data_sf$nfish)), ]
    rownames(size_data_sf) = NULL
    size_data_sf = subset(size_data_sf, select= fitted_length)
    
    
    #draw random sample of sf sizes matching the number of fish caught
    random_sizes  = data.frame(size_data_sf[sample(nrow(size_data_sf), nrow(sf_catch_data)), ])
    colnames(random_sizes) = "fitted_length"
    random_sizes$fishid = 1:nrow(random_sizes)
    catch_size_data =  merge(random_sizes,sf_catch_data,by="fishid")
    
    
    # Impose regulations, calculate keep and release per trip
    # For summer flounder, retain keep- and release-at-length
    bag = fluke_bag
    minsize = fluke_min
    catch_size_data$keep = ifelse(catch_size_data$fitted_length>=minsize, 1,0) 
    catch_size_data$csum_keep <- ave(catch_size_data$keep, catch_size_data$tripid, FUN=cumsum)
    catch_size_data$keep_adj = ifelse(catch_size_data$csum_keep<=bag & catch_size_data$keep==1, 1,0) 
    catch_size_data$release = ifelse(catch_size_data$keep_adj==1, 0,1) 
    
    catch_size_data= subset(catch_size_data, select=c(fishid, fitted_length, tot_sf_catch, tripid, keep_adj, release, tot_bsb_catch))
    names(catch_size_data)[names(catch_size_data) == "keep_adj"] = "keep"
    
    
    # generate the sum of number of kept and released fish by tripid
    catch_size_data$tot_keep=with(catch_size_data, ave(keep, tripid, FUN = sum))
    catch_size_data$tot_rel=with(catch_size_data, ave(release, tripid, FUN = sum))
    
    
    # generate data set with total keep and release only
    catch_size_data1 = cbind(catch_size_data$tripid, catch_size_data$tot_keep, catch_size_data$tot_rel, catch_size_data$tot_bsb_catch)
    colnames(catch_size_data1) = cbind("tripid", "tot_keep", "tot_rel", "tot_bsb_catch")
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
    
    
    
    # merge the the keep-/releasse-atlength files with the tot_keep/release file 
    trip_data =  merge(release_size_data_wide,keep_size_data_wide,by="tripid", all.x=TRUE, all.y=TRUE)
    trip_data =  merge(trip_data,catch_size_data2,by="tripid", all.x=TRUE, all.y=TRUE)
    
    #add the zero catch trips 
    trip_data = bind_rows(trip_data, sf_zero_catch)
    
    #quick sort and cleanup 
    trip_data = trip_data[order(trip_data$tripid),]
    rownames(trip_data) <- NULL
    
    trip_data[is.na(trip_data)] = 0
    trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
    
    
    
    #########################
    ###  Black sea bass  ####
    #########################
    
    
    #draw sizes for black sea bass catch
    bsb_catch_data =subset(trip_data, select=c(tripid, tot_bsb_catch))
    
    #subset trips with zero bsb catch 
    bsb_zero_catch = subset(trip_data, tot_bsb_catch == 0, select=c(tripid, tot_bsb_catch))
    
    
    #remove trips with zero bsb catch, will add them on later
    bsb_catch_data=bsb_catch_data[bsb_catch_data$tot_bsb_catch!=0, ]
    rownames(bsb_catch_data) = NULL
    
    
    #expand the bsb_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(bsb_catch_data))
    bsb_catch_data[is.na(bsb_catch_data)] = 0
    bsb_catch_data = bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
    rownames(bsb_catch_data) = NULL
    bsb_catch_data$fishid = 1:nrow(bsb_catch_data)
    
    
    #import and expand the bsb size data so that each row represents a fish
    size_data_bsb = subset(size_data, region==region1 & species=="blackseabass", select= c(fitted_length, fitted_prob))
    size_data_bsb$nfish = round(100000 * size_data_bsb$fitted_prob, digits=0)
    sum(size_data_bsb$nfish)
    
    row_inds <- seq_len(nrow(size_data_bsb))
    size_data_bsb <- size_data_bsb[c(rep(row_inds, size_data_bsb$nfish)), ]
    rownames(size_data_sf) = NULL
    size_data_bsb = subset(size_data_bsb, select= fitted_length)
    
    
    #draw random sample of sizes matching the number of fish caught
    random_sizes  = data.frame(size_data_bsb[sample(nrow(size_data_bsb), nrow(bsb_catch_data)), ])
    colnames(random_sizes) = "fitted_length"
    random_sizes$fishid = 1:nrow(random_sizes)
    catch_size_data =  merge(random_sizes,bsb_catch_data,by="fishid")
    
    
    # Impose regulations, calculate keep and release per trip
    bag = bsb_bag
    minsize = bsb_min
    catch_size_data$keep_bsb = ifelse(catch_size_data$fitted_length>=minsize, 1,0) 
    catch_size_data$csum_keep_bsb <- ave(catch_size_data$keep_bsb, catch_size_data$tripid, FUN=cumsum)
    catch_size_data$keep_bsb_adj = ifelse(catch_size_data$csum_keep_bsb<=bag & catch_size_data$keep_bsb==1, 1,0) 
    catch_size_data$release_bsb = ifelse(catch_size_data$keep_bsb_adj==1, 0,1) 
    
    
    catch_size_data= subset(catch_size_data, select=c(fishid, fitted_length, tot_bsb_catch, tripid, keep_bsb_adj, release_bsb))
    names(catch_size_data)[names(catch_size_data) == "keep_bsb_adj"] = "keep_bsb"
    
    
    
    # generate sum of number of kept and released fish by tripid
    catch_size_data$tot_keep_bsb=with(catch_size_data, ave(keep_bsb, tripid, FUN = sum))
    catch_size_data$tot_rel_bsb=with(catch_size_data, ave(release_bsb, tripid, FUN = sum))
    
    
    # generate data set with total keep and release only by tripid
    catch_size_data1 = cbind(catch_size_data$tripid, catch_size_data$tot_keep_bsb, catch_size_data$tot_rel_bsb, catch_size_data$tot_bsb_cat)
    colnames(catch_size_data1) = cbind("tripid", "tot_keep_bsb", "tot_rel_bsb", "tot_bsb_cat")
    catch_size_data2 = as.data.frame(catch_size_data1[!duplicated(catch_size_data1), ])
    names(catch_size_data2)[names(catch_size_data2) == "tot_bsb_cat"] = "tot_bsb_catch"
    
    
    # append the zero bsb catch trip_cost_data
    catch_size_data2 = bind_rows(catch_size_data2, bsb_zero_catch)
    catch_size_data2 = subset(catch_size_data2, select=c(tripid, tot_keep_bsb, tot_rel_bsb))
    
    
    # merge the trip data (summer flounder catch, lengths, and cost) with the bsb data (numbers kept and released))
    trip_data =  merge(trip_data,catch_size_data2,by="tripid")
    trip_data[is.na(trip_data)] = 0
    
    
    
    
    #####################
    #####   Scup   ######
    #####################
    
    #import scup data
    scup_catch_data = data.frame(read_excel("other_species_fitted_catch.xlsx"))                                                                            
    scup_catch_data = as.data.frame(subset(scup_catch_data, region == region1 & species=="scup", select=c(tot_cat, fitted_prob)))
    scup_catch_data$nfish = round(100000 * scup_catch_data$fitted_prob, digits=0)
    
    #expand the scup_data so that each row represents a trip
    row_inds = seq_len(nrow(scup_catch_data))
    scup_catch_data = scup_catch_data[c(rep(row_inds, scup_catch_data$nfish)), ]
    scup_catch_data = subset(scup_catch_data,select= tot_cat)
    rownames(scup_catch_data) = NULL
    
    
    # Randomly select X number of these trips that match the number of trips drawn for fluke and bsb
    scup_catch_data <- as.data.frame(scup_catch_data[sample(1:nrow(scup_catch_data), n_draws), ])
    colnames(scup_catch_data) = "tot_catch_scup"
    scup_catch_data$tripid = 1:nrow(scup_catch_data)
    
    
    #save trips with zero scup catch 
    scup_zero_catch = subset(scup_catch_data, tot_catch_scup == 0)
    
    
    #remove trips with zero scup catch
    scup_catch_data=scup_catch_data[scup_catch_data$tot_catch_scup!=0, ]
    rownames(scup_catch_data) = NULL
    
    
    #expand the scup_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(scup_catch_data))
    scup_catch_data = scup_catch_data[c(rep(row_inds, scup_catch_data$tot_catch_scup)), ]
    rownames(scup_catch_data) = NULL
    scup_catch_data$fishid = 1:nrow(scup_catch_data)
    
    
    #import and expand the scup_size_data so that each row represents a fish
    size_data_scup = subset(size_data, region==region1 & species=="scup", select= c(fitted_length, fitted_prob))
    size_data_scup$nfish = round(100000 * size_data_scup$fitted_prob, digits=0)
    sum(size_data_scup$nfish)
    
    row_inds <- seq_len(nrow(size_data_scup))
    size_data_scup <- size_data_scup[c(rep(row_inds, size_data_scup$nfish)), ]
    rownames(size_data_scup) = NULL
    size_data_scup = subset(size_data_scup, select=fitted_length)
    
    
    
    #draw random sample of sizes matching the number of fish caught
    random_sizes  = data.frame(size_data_scup[sample(nrow(size_data_scup), nrow(scup_catch_data)), ])
    colnames(random_sizes) = "fitted_length"
    random_sizes$fishid = 1:nrow(random_sizes)
    catch_size_data =  merge(random_sizes,scup_catch_data,by="fishid")
    
    
    # impose regulations, calculate keep and release
    bag = scup_bag
    minsize = scup_min
    catch_size_data$keep_scup = ifelse(catch_size_data$fitted_length>=minsize, 1,0) 
    catch_size_data$csum_keep_scup <- ave(catch_size_data$keep_scup, catch_size_data$tripid, FUN=cumsum)
    catch_size_data$keep_scup_adj = ifelse(catch_size_data$csum_keep_scup<=bag & catch_size_data$keep_scup==1, 1,0) 
    catch_size_data$release_scup = ifelse(catch_size_data$keep_scup_adj==1, 0,1) 
    
    
    catch_size_data= subset(catch_size_data, select=c(fishid, fitted_length, tot_catch_scup, tripid, keep_scup_adj, release_scup))
    names(catch_size_data)[names(catch_size_data) == "keep_scup_adj"] = "keep_scup"
    
    
    # generate sum of number of kept and released fish by tripid
    catch_size_data$tot_keep_scup=with(catch_size_data, ave(keep_scup, tripid, FUN = sum))
    catch_size_data$tot_rel_scup=with(catch_size_data, ave(release_scup, tripid, FUN = sum))
    
    
    # generate data set with total keep and release only
    catch_size_data1 = cbind(catch_size_data$tripid, catch_size_data$tot_keep_scup, catch_size_data$tot_rel_scup, catch_size_data$tot_catch_scup)
    colnames(catch_size_data1) = cbind("tripid", "tot_keep_scup", "tot_rel_scup", "tot_scup_cat")
    catch_size_data2 = as.data.frame(catch_size_data1[!duplicated(catch_size_data1), ])
    names(catch_size_data2)[names(catch_size_data2) == "tot_scup_cat"] = "tot_scup_catch"
    
    
    # append the zero bsb catch trip_cost_data
    catch_size_data2 = bind_rows(catch_size_data2, scup_zero_catch)
    catch_size_data2 = subset(catch_size_data2, select=c(tripid, tot_keep_scup, tot_rel_scup, tot_scup_catch))
    
    
    # merge the trip data (summer flounder catch + lengths, bsb catch, and cost) with the scup data (numbers kept and released))
    trip_data =  merge(trip_data,catch_size_data2,by="tripid")
    trip_data[is.na(trip_data)] = 0
    
    
    #####################
    #####   weakfish   ######
    #####################
    
    #import wf data
    wf_catch_data = data.frame(read_excel("other_species_fitted_catch.xlsx"))                                                                            
    wf_catch_data = as.data.frame(subset(wf_catch_data, region == region1 & species=="weakfish", select=c(tot_cat, fitted_prob)))
    wf_catch_data[is.na(wf_catch_data)] = 0
    wf_catch_data$nfish = round(100000 * wf_catch_data$fitted_prob, digits=0)
    
    #expand the wf_data so that each row represents a trip
    row_inds = seq_len(nrow(wf_catch_data))
    wf_catch_data = wf_catch_data[c(rep(row_inds, wf_catch_data$nfish)), ]
    wf_catch_data = subset(wf_catch_data, select=tot_cat)
    rownames(wf_catch_data) = NULL
    
    
    # Randomly select X number of these trips that match the number of trips drawn for fluke, bsb, and scup
    wf_catch_data <- as.data.frame(wf_catch_data[sample(1:nrow(wf_catch_data), n_draws), ])
    colnames(wf_catch_data) = "tot_catch_wf"
    wf_catch_data$tripid = 1:nrow(wf_catch_data)
    
    
    #save trips with zero wf catch 
    wf_zero_catch = subset(wf_catch_data, tot_catch_wf == 0)
    
    
    #remove trips with zero wf catch
    wf_catch_data=wf_catch_data[wf_catch_data$tot_catch_wf!=0, ]
    rownames(wf_catch_data) = NULL
    
    
    #expand the wf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(wf_catch_data))
    wf_catch_data = wf_catch_data[c(rep(row_inds, wf_catch_data$tot_catch_wf)), ]
    rownames(wf_catch_data) = NULL
    wf_catch_data$fishid = 1:nrow(wf_catch_data)
    
    
    #import and expand the wf_size_data so that each row represents a fish
    size_data_wf = subset(size_data, region==region1 & species=="weakfish", select= c(fitted_length, fitted_prob))
    size_data_wf$nfish = round(100000 * size_data_wf$fitted_prob, digits=0)
    sum(size_data_wf$nfish)
    
    row_inds <- seq_len(nrow(size_data_wf))
    size_data_wf <- size_data_wf[c(rep(row_inds, size_data_wf$nfish)), ]
    rownames(size_data_wf) = NULL
    size_data_wf = subset(size_data_wf, select=fitted_length)
    
    
    
    #draw random sample of sizes matching the number of fish caught
    random_sizes  = data.frame(size_data_wf[sample(nrow(size_data_wf), nrow(size_data_wf)), ])
    colnames(random_sizes) = "fitted_length"
    random_sizes$fishid = 1:nrow(random_sizes)
    catch_size_data =  merge(random_sizes,wf_catch_data,by="fishid")
    
    
    # impose regulations, calculate keep and release
    bag = wf_bag
    minsize = wf_min
    catch_size_data$keep_wf = ifelse(catch_size_data$fitted_length>=minsize, 1,0) 
    catch_size_data$csum_keep_wf <- ave(catch_size_data$keep_wf, catch_size_data$tripid, FUN=cumsum)
    catch_size_data$keep_wf_adj = ifelse(catch_size_data$csum_keep_wf<=bag & catch_size_data$keep_wf==1, 1,0) 
    catch_size_data$release_wf = ifelse(catch_size_data$keep_wf_adj==1, 0,1) 
    
    
    catch_size_data= subset(catch_size_data, select=c(fishid, fitted_length, tot_catch_wf, tripid, keep_wf_adj, release_wf))
    names(catch_size_data)[names(catch_size_data) == "keep_wf_adj"] = "keep_wf"
    
    
    
    # generate sum of number of kept and released fish by tripid
    catch_size_data$tot_keep_wf=with(catch_size_data, ave(keep_wf, tripid, FUN = sum))
    catch_size_data$tot_rel_wf=with(catch_size_data, ave(release_wf, tripid, FUN = sum))
    
    
    # generate data set with total keep and release only
    catch_size_data1 = cbind(catch_size_data$tripid, catch_size_data$tot_keep_wf, catch_size_data$tot_rel_wf, catch_size_data$tot_catch_wf)
    colnames(catch_size_data1) = cbind("tripid", "tot_keep_wf", "tot_rel_wf", "tot_wf_cat")
    catch_size_data2 = as.data.frame(catch_size_data1[!duplicated(catch_size_data1), ])
    names(catch_size_data2)[names(catch_size_data2) == "tot_wf_cat"] = "tot_wf_catch"
    
    
    # append the zero wf catch trip_cost_data
    catch_size_data2 = bind_rows(catch_size_data2, wf_zero_catch)
    catch_size_data2 = subset(catch_size_data2, select=c(tripid, tot_keep_wf, tot_rel_wf, tot_wf_catch))
    
    
    # merge the trip data (summer flounder catch + lengths, bsb catch,scup catch and cost) with the scup data (numbers kept and released))
    trip_data =  merge(trip_data,catch_size_data2,by="tripid")
    trip_data[is.na(trip_data)] = 0    
    
    
    trip_data$catch_draw=i
    dfs[[i]]=trip_data
    
  }
  
  #combine all the catch draw files 
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
costs_new_NJ = list()
pds_new = list()
for (p in min_period:max_period) {
  
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip)  
  
  # Add trip costs. Assign choice occasion a shore or boat trip cost in proportion to estimated number of
  # directed fluke trips by mode. I copied the proportions below from directed_trips_by_state_mode.dta
  
  max_trip=max(pds[[p]]$tripid)
  charter=round(max_trip*.01)
  headboat=round(max_trip*.01)
  private=round(max_trip*.48)
  shore = max_trip-charter-headboat-private
  
  charter_draws = trip_cost_data_chart[sample(nrow(trip_cost_data_chart), charter), ]
  headboat_draws = trip_cost_data_headb[sample(nrow(trip_cost_data_headb), headboat), ]
  privt_draws = trip_cost_data_privt[sample(nrow(trip_cost_data_privt), private), ]
  shore_draws = trip_cost_data_shore[sample(nrow(trip_cost_data_shore), shore), ]
  
  cost_data = bind_rows(charter_draws, headboat_draws,privt_draws, shore_draws )
  cost_data$tripid = 1:nrow(cost_data)
  cost_data= subset(cost_data, select=c(tripid, cost))
  trip_data =  merge(pds[[p]],cost_data,by="tripid")
  trip_data[is.na(trip_data)] = 0
  
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario. 
  # We will merge these data to the prediction year outcomes to calculate changes in CS. 
  costs_new_NJ[[p]] = subset(trip_data, select=c(tripid, cost, catch_draw, tot_keep, tot_rel, tot_sf_catch,
                                                 tot_keep_bsb,tot_rel_bsb,tot_keep_scup, tot_rel_scup, tot_keep_wf, tot_rel_wf  ))
  
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_keep"] = "tot_keep_sf_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_rel"] = "tot_rel_sf_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_keep_bsb"] = "tot_keep_bsb_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_rel_bsb"] = "tot_rel_bsb_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_keep_scup"] = "tot_keep_scup_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_rel_scup"] = "tot_rel_scup_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_keep_wf"] = "tot_keep_wf_base"
  names(costs_new_NJ[[p]])[names(costs_new_NJ[[p]]) == "tot_rel_wf"] = "tot_rel_wf_base"
  
  costs_new_NJ[[p]]$period = p
  
  
  
  
  #set up an output file for each draw of utility parameters
  parameter_draws = list()
  
  for(d in 1:1) {
    
    # Import utility parameter draws (d)
    # This data contains a set of utility parameters based on the means and VC matrix. 
    # We will draw from these data multiple times, but need to configure it with random draws from the population model. 
    # For now I am drawin only one set of utility parameters
    
    param_draws_NJ = data.frame(read_excel("utility_params_draws_NJ.xlsx"))                                                                            
    param_draws_NJ1 = subset(param_draws_NJ, n==d)
    
    #Expected utility
    trip_data$vA = param_draws_NJ1$sqrt_sf_keep*sqrt(trip_data$tot_keep) +
      param_draws_NJ1$sqrt_sf_release*sqrt(trip_data$tot_rel) +  
      param_draws_NJ1$sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
      param_draws_NJ1$sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
      param_draws_NJ1$sqrt_scup_keep*sqrt(trip_data$tot_keep_scup) +
      param_draws_NJ1$sqrt_scup_release*sqrt(trip_data$tot_rel_scup) + 
      param_draws_NJ1$sqrt_wf_keep*sqrt(trip_data$tot_keep_wf) +
      param_draws_NJ1$sqrt_wf_release*sqrt(trip_data$tot_rel_wf) + 
      param_draws_NJ1$cost*trip_data$cost 
    
    
    # Collapse data from the X catch draws so that each row contains mean values
    mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
    
    
    # Now expand the data to create three alternatives, representing the alternatives available in choice survey
    mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
    
    #Alt 1, 2, 3
    mean_trip_data$alt <- sequence(tabulate(mean_trip_data$tripid))
    
    #Alt 2 and 3 are the opt_out and other_fishing alternatives
    mean_trip_data$opt_out = ifelse(mean_trip_data$alt!=1 & mean_trip_data$alt!=2, 1,0) 
    mean_trip_data$striper_blue = ifelse(mean_trip_data$alt!=1 & mean_trip_data$alt!=3, 1,0) 
    
    #Caluculate the expected utility of alts 2 and 3 based on the parameters of the utility function
    mean_trip_data$vA_optout= param_draws_NJ1$optout*mean_trip_data$opt_out 
    mean_trip_data$vA_striper_blue= param_draws_NJ1$striper_blue*mean_trip_data$striper_blue 
    
    #Now put these three values in the same column, exponentiate, and caluculate their sum (vA_col_sum)
    mean_trip_data$vA[mean_trip_data$alt!=1] <- 0
    mean_trip_data$vA_row_sum = rowSums(mean_trip_data[,c("vA", "vA_striper_blue","vA_optout")])
    mean_trip_data$vA_row_sum = exp(mean_trip_data$vA_row_sum)
    mean_trip_data$vA_col_sum = ave(mean_trip_data$vA_row_sum, mean_trip_data$tripid, FUN = sum)
    
    
    
    # Caluculate the probability of a respondent selected each alternative based on 
    # exponentiated expected utility of the altenrative [exp(expected utility, alt=i] 
    # and the sum of exponentiated expected utility across the three altenratives.
    # You will notice the striper_blue alternative has a large proabability based on the utility parameters
    mean_trip_data$probA = mean_trip_data$vA_row_sum/mean_trip_data$vA_col_sum
    
    # Get rid of things we don't need. 
    mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum))
    
    
    # Multiply the trip probability by each of the catch variables (not the variable below) to get probability-weighted catch
    list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                          & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period"
                                          & colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA"
                                          & colnames(mean_trip_data) !="probA"  ]
    
    for (l in list_names){
      mean_trip_data[,l] = mean_trip_data[,l]*mean_trip_data$probA
    }
    
    
    # Multiply each choice occasion's trip outcomes (catch, trip probabilities) in mean_trip_pool 
    # by the expansion factor (expand), so that each choice occasion represents a certain number of choice occasions
    
    mean_prob=mean(mean_trip_data$probA)
    observed_trips=n_trips
    sims = round(observed_trips/mean_prob)
    ndraws = nrow(mean_trip_data)
    expand=sims/ndraws
    
    list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                          & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period"
                                          & colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" ]
    
    for (l in list_names){
      mean_trip_data[,l] = mean_trip_data[,l]*expand
    }
    
    
    #This should equal the observed # of trips in that period
    sum(mean_trip_data$probA)
    
    
    
    mean_trip_data$sim=1
    
    #sum probability weighted catch over all choice occasions
    aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
    aggregate_trip_data$n_choice_occasions = sims
    
    
    aggregate_trip_data = subset(aggregate_trip_data, select=-c(Group.1, tripid, catch_draw, period, cost, vA ,sim))
    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "observed_trips"
    
    
    aggregate_trip_data$sim = d
    
    parameter_draws[[d]]=aggregate_trip_data
    
  }
  
  # Combine the output from all the utility parameter draws (for now we only have one)
  parameter_draws_all = as.data.frame(bind_rows(parameter_draws[[1]]))
  parameter_draws_all[is.na(parameter_draws_all)] = 0
  rownames(parameter_draws_all) = NULL
  
  
  parameter_draws_all$period=p
  pds_new[[p]]=parameter_draws_all
  
}


pds_new_all_NJ = as.data.frame(bind_rows(pds_new[[1]], pds_new[[2]],pds_new[[3]],pds_new[[4]], pds_new[[5]], pds_new[[6]]))
pds_new_all_NJ[is.na(pds_new_all_NJ)] = 0
pds_new_all_NJ$state = state1
pds_new_all_NJ$alt_regs = 0
pds_new_all_NJ= subset(pds_new_all_NJ, select=-c(Group.1, tot_sf_catch, tot_bsb_catch, tot_scup_catch, tot_wf_catch))



# costs_new_all contain trip outcomes for the baseline period. Will use to calculate welfare changes, 
# and assign catch-per-trip in the prediction years. 
costs_new_all_NJ = as.data.frame(bind_rows(costs_new_NJ[[1]], costs_new_NJ[[2]],costs_new_NJ[[3]],costs_new_NJ[[4]], costs_new_NJ[[5]],  costs_new_NJ[[6]]))
costs_new_all_NJ[is.na(costs_new_all_NJ)] = 0




