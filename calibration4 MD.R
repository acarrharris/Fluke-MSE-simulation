
###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By state or region (MA-NY, NJ, DE-NC): sf catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 

# This code requires the following data:
# 1) Directed summer flounder by regulatory period in the baseline year: directed_trips_region_XX.xlsx
# 2) Distribution of trip costs by mode in each state: trip_costs_NE.xlsx
# 3) Catch-per-trip of SF and BSB from the copula model: XX_catch_data_sim1.xlsx
# 4) Catch-per-trip of species other than SF and BSB: other_species_fitted_catch.xlsx


state1="MD"
region1="SO"


######################################
##   Begin simulating trip outcomes ##
######################################


#Import directed trips file - gives directed trips by regulatory period in 2019
directed_trips = data.frame(read_excel("directed_trips_regions_bimonthly.xlsx"))                                                                            
directed_trips$dtrip=round(directed_trips$dtrip_2019)
directed_trips= subset(directed_trips, state == state1)

min_period=min(directed_trips$period)
max_period=max(directed_trips$period)




# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()
periodz=as.factor(directed_trips$period)
levels(periodz)

for(p in levels(periodz)){
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip_2019)
  #n_draws = min(1000,n_trips*2.5 )
  n_draws = n_drawz
  
  fluke_bag = mean(directed_trips_p$fluke_bag_2019)
  fluke_min = mean(directed_trips_p$fluke_min_2019)
  bsb_bag = mean(directed_trips_p$bsb_bag_2019)
  bsb_min = mean(directed_trips_p$bsb_min_2019)
  wf_bag = mean(directed_trips_p$wf_bag_2019)
  wf_min = mean(directed_trips_p$wf_min_2019) 
  
  
  # Set up an output file for catch draw files 
  dfs = list()
  
  #Run the catch loop X times to give X draws of catch for each species
  for(i in 1:10) {
    # Input catch-per-trip numbers 
    sf_catch_data = data.frame(read_excel("SO_catch_data_sim1.xlsx"))                                                                            
    tot_sf_catch = sf_catch_data$sf_t_nb
    tot_bsb_catch = sf_catch_data$bsb_t_nb
    sf_catch_data = data.frame(tot_sf_catch,tot_bsb_catch)
    
    # random draw of fluke and bsb catch
    sf_catch_data = as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_draws), ])
    sf_catch_data$tripid = 1:nrow(sf_catch_data)
    sf_bsb_catch_data = sf_catch_data
    
    
    # subset trips with zero catch, as no size draws are required
    sf_zero_catch = subset(sf_catch_data, tot_sf_catch == 0)
    
    
    #remove trips with zero summer flounder catch, will add them on later
    sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(sf_catch_data))
    sf_catch_data = sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
    rownames(sf_catch_data) = NULL
    sf_catch_data$fishid = 1:nrow(sf_catch_data)
    
    
    
    #Execute the following code if the seasonal period has a positive bag limit 
    if(fluke_bag>0){
      
      sf_catch_data1= as.data.frame(sf_catch_data)  
      sf_catch_data1$uniform=runif(nrow(sf_catch_data1))
      sf_catch_data1$keep = ifelse(sf_catch_data1$uniform>=0.92, 1,0) 
      
      sf_catch_data1$csum_keep <- ave(sf_catch_data1$keep, sf_catch_data1$tripid, FUN=cumsum)
      sf_catch_data1$keep_adj = ifelse(sf_catch_data1$csum_keep>fluke_bag, 0,sf_catch_data1$keep)
      
      #Add the following lines to end the trip once the bag limit is reached (rather than continuing to discard)
      ###
      sf_catch_data1$post_bag_fish=ifelse(sf_catch_data1$csum_keep>fluke_bag, 1,0)
      sf_catch_data1= subset(sf_catch_data1,post_bag_fish==0 )
      sf_catch_data1 <- subset(sf_catch_data1, select=-c(post_bag_fish ))
      ###
      
      sf_catch_data1 <- subset(sf_catch_data1, select=-c(keep, csum_keep))
      names(sf_catch_data1)[names(sf_catch_data1) == "keep_adj"] = "keep"
      
      sf_catch_data1$release = ifelse(sf_catch_data1$keep==0, 1,0) 
      
      sf_catch_data1=subset(sf_catch_data1, select=c(tripid, keep, release))
      sf_catch_data1 <-aggregate(sf_catch_data1, by=list(sf_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      sf_catch_data1 <-subset(sf_catch_data1, select=c(Group.1, keep, release))
      names(sf_catch_data1)[names(sf_catch_data1) == "Group.1"] = "tripid"
      names(sf_catch_data1)[names(sf_catch_data1) == "keep"] = "tot_keep"
      names(sf_catch_data1)[names(sf_catch_data1) == "release"] = "tot_rel"
      
    }
    
    if(fluke_bag==0){
      
      sf_catch_data1= as.data.frame(sf_catch_data)  
      sf_catch_data1$keep = 0
      sf_catch_data1$release = 1
      
      sf_catch_data1=subset(sf_catch_data1, select=c(tripid, keep, release))
      sf_catch_data1 <-aggregate(sf_catch_data1, by=list(sf_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      sf_catch_data1 <-subset(sf_catch_data1, select=c(Group.1, keep, release))
      names(sf_catch_data1)[names(sf_catch_data1) == "Group.1"] = "tripid"
      names(sf_catch_data1)[names(sf_catch_data1) == "keep"] = "tot_keep"
      names(sf_catch_data1)[names(sf_catch_data1) == "release"] = "tot_rel"
      
    }

    
    trip_data =  as.data.frame(sf_catch_data1)
    
    #add the zero catch trips 
    trip_data = bind_rows(trip_data, sf_zero_catch)
    
    #quick sort and cleanup 
    trip_data = trip_data[order(trip_data$tripid),]
    rownames(trip_data) <- NULL
    
    trip_data[is.na(trip_data)] = 0
    trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
    trip_data[is.na(trip_data)] = 0
    
    
    
    #########################
    ###  Black sea bass  ####
    #########################
    
    
    #draw sizes for black sea bass catch
    bsb_catch_data =subset(sf_bsb_catch_data, select=c(tripid, tot_bsb_catch))
    bsb_catch_data = bsb_catch_data[!duplicated(bsb_catch_data), ]
    
    #subset trips with zero bsb catch 
    bsb_zero_catch = subset(bsb_catch_data, tot_bsb_catch == 0, select=c(tripid, tot_bsb_catch))
    
    
    #remove trips with zero bsb catch, will add them on later
    bsb_catch_data=bsb_catch_data[bsb_catch_data$tot_bsb_catch!=0, ]
    rownames(bsb_catch_data) = NULL
    
    
    #expand the bsb_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(bsb_catch_data))
    bsb_catch_data[is.na(bsb_catch_data)] = 0
    bsb_catch_data = bsb_catch_data[c(rep(row_inds, bsb_catch_data$tot_bsb_catch)), ]
    rownames(bsb_catch_data) = NULL
    bsb_catch_data$fishid = 1:nrow(bsb_catch_data)
    
    
    
    #Execute the following code if the seasonal period has a positive bag limit 
    if(bsb_bag>0){
      
      bsb_catch_data1= as.data.frame(bsb_catch_data)  
      bsb_catch_data1$uniform=runif(nrow(bsb_catch_data1))
      bsb_catch_data1$keep = ifelse(bsb_catch_data1$uniform>=.96, 1,0)
      
      bsb_catch_data1$csum_keep <- ave(bsb_catch_data1$keep, bsb_catch_data1$tripid, FUN=cumsum)
      bsb_catch_data1$keep_adj = ifelse(bsb_catch_data1$csum_keep>bsb_bag, 0,bsb_catch_data1$keep)
      bsb_catch_data1 <- subset(bsb_catch_data1, select=-c(keep, csum_keep))
      names(bsb_catch_data1)[names(bsb_catch_data1) == "keep_adj"] = "keep"
      
      bsb_catch_data1$release = ifelse(bsb_catch_data1$keep==0, 1,0) 
      
      bsb_catch_data1=subset(bsb_catch_data1, select=c(tripid, keep, release))
      bsb_catch_data1 <-aggregate(bsb_catch_data1, by=list(bsb_catch_data$tripid),FUN=sum, na.rm=TRUE)
      bsb_catch_data1 <-subset(bsb_catch_data1, select=c(Group.1, keep, release))
      names(bsb_catch_data1)[names(bsb_catch_data1) == "Group.1"] = "tripid"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "keep"] = "tot_keep_bsb"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "release"] = "tot_rel_bsb"
      
    }
    
    if(bsb_bag==0){
      
      bsb_catch_data1= as.data.frame(bsb_catch_data)  
      bsb_catch_data1$keep = 0
      bsb_catch_data1$release = 1
      
      bsb_catch_data1=subset(bsb_catch_data1, select=c(tripid, keep, release))
      bsb_catch_data1 <-aggregate(bsb_catch_data1, by=list(bsb_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      bsb_catch_data1 <-subset(bsb_catch_data1, select=c(Group.1, keep, release))
      names(bsb_catch_data1)[names(bsb_catch_data1) == "Group.1"] = "tripid"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "keep"] = "tot_keep_bsb"
      names(bsb_catch_data1)[names(bsb_catch_data1) == "release"] = "tot_rel_bsb"
      
    }
    
    
    #add the zero catch trips 
    bsb_catch_data1 = bind_rows(bsb_catch_data1, bsb_zero_catch)
    bsb_catch_data1 = subset(bsb_catch_data1, select=-c(tot_bsb_catch))
    
    #quick sort and cleanup 
    bsb_catch_data1 = bsb_catch_data1[order(bsb_catch_data1$tripid),]
    rownames(bsb_catch_data1) <- NULL
    
    bsb_catch_data1[is.na(bsb_catch_data1)] = 0
    
    
    
    # merge the trip data (summer flounder catch, lengths, and cost) with the bsb data (numbers kept and released))
    trip_data =  merge(trip_data,bsb_catch_data1,by="tripid")
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
    
    #ensure at least one trip has positive wf catch
    wf_catch_data$tot_catch_wf[wf_catch_data$tripid==1] <- 1
    
    
    #save trips with zero scup catch 
    wf_zero_catch = subset(wf_catch_data, tot_catch_wf == 0)
    
    
    #remove trips with zero wf catch
    wf_catch_data=wf_catch_data[wf_catch_data$tot_catch_wf!=0, ]
    
    rownames(wf_catch_data) = NULL
    
    
    #expand the wf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(wf_catch_data))
    wf_catch_data = wf_catch_data[c(rep(row_inds, wf_catch_data$tot_catch_wf)), ]
    rownames(wf_catch_data) = NULL
    wf_catch_data$fishid = 1:nrow(wf_catch_data)
    
    
    #Execute the following code if the seasonal period has a positive bag limit 
    if(wf_bag>0){
      
      wf_catch_data1= as.data.frame(wf_catch_data)  
      wf_catch_data1$uniform=runif(nrow(wf_catch_data1))
      wf_catch_data1$keep = ifelse(wf_catch_data1$uniform>=1, 1,0) 
      
      wf_catch_data1$csum_keep <- ave(wf_catch_data1$keep, wf_catch_data1$tripid, FUN=cumsum)
      wf_catch_data1$keep_adj = ifelse(wf_catch_data1$csum_keep>wf_bag, 0,wf_catch_data1$keep)
      wf_catch_data1 <- subset(wf_catch_data1, select=-c(keep, csum_keep))
      names(wf_catch_data1)[names(wf_catch_data1) == "keep_adj"] = "keep"
      
      
      wf_catch_data1$release = ifelse(wf_catch_data1$keep==0, 1,0) 
      
      wf_catch_data1=subset(wf_catch_data1, select=c(tripid, keep, release))
      wf_catch_data1 <-aggregate(wf_catch_data1, by=list(wf_catch_data$tripid),FUN=sum, na.rm=TRUE)
      wf_catch_data1 <-subset(wf_catch_data1, select=c(Group.1, keep, release))
      names(wf_catch_data1)[names(wf_catch_data1) == "Group.1"] = "tripid"
      
    }
    
    if(wf_bag==0){
      
      wf_catch_data1= as.data.frame(wf_catch_data)  
      wf_catch_data1$keep = 0
      wf_catch_data1$release = 1
      
      wf_catch_data1=subset(wf_catch_data1, select=c(tripid, keep, release))
      wf_catch_data1 <-aggregate(wf_catch_data1, by=list(wf_catch_data1$tripid),FUN=sum, na.rm=TRUE)
      wf_catch_data1 <-subset(wf_catch_data1, select=c(Group.1, keep, release))
      names(wf_catch_data1)[names(wf_catch_data1) == "Group.1"] = "tripid"
      
    }
    
    
    #add the zero catch trips 
    wf_catch_data1 = bind_rows(wf_catch_data1, wf_zero_catch)
    wf_catch_data1 = subset(wf_catch_data1, select=-c(tot_catch_wf))
    
    #quick sort and cleanup 
    wf_catch_data1 = wf_catch_data1[order(wf_catch_data1$tripid),]
    rownames(wf_catch_data1) <- NULL
    
    wf_catch_data1[is.na(wf_catch_data1)] = 0
    names(wf_catch_data1)[names(wf_catch_data1) == "keep"] = "tot_keep_wf"
    names(wf_catch_data1)[names(wf_catch_data1) == "release"] = "tot_rel_wf"
    wf_catch_data1 = wf_catch_data1[order(wf_catch_data1$tripid),]
    
    
    # merge catch data with other tip data 
    trip_data =  merge(trip_data,wf_catch_data1,by="tripid")
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

pds_all= list.stack(pds, fill=TRUE)
pds_all[is.na(pds_all)] = 0
rm(pds)



#Create random draws of preference parameters based on the estimated means and SD from the choice model
utilites_MD = list()
for(d in 1:100) {
  
  param_draws_MD = as.data.frame(1:30000)
  names(param_draws_MD)[names(param_draws_MD) == "1:30000"] = "tripid"
  param_draws_MD$beta_sqrt_sf_keep = rnorm(30000, mean = 0.776, sd = 0.516)
  param_draws_MD$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.258)
  param_draws_MD$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.239, sd = 0.311)
  param_draws_MD$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0.139)
  param_draws_MD$beta_sqrt_wf_keep = rnorm(30000, mean = 0.360, sd =  0.251)
  param_draws_MD$beta_sqrt_wf_release = rnorm(30000, mean = 0.061, sd = 0.220)
  param_draws_MD$beta_opt_out = rnorm(30000, mean = -2.838, sd = 2.246)
  param_draws_MD$beta_striper_blue = rnorm(30000, mean = 0.606, sd = 1.752)
  param_draws_MD$beta_cost = rnorm(30000, mean = -0.009, sd = 0)
  param_draws_MD$parameter_draw=d
  param_draws_MD <- param_draws_MD[1:n_drawz, ] 
  
  utilites_MD[[d]]= param_draws_MD
  
}

utilites_MD_all=list.stack(utilites_MD, fill=TRUE)

# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion
costs_new_MD = list()
pds_new = list()
for(p in levels(periodz)){
  
  
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip)  
  
  # Add trip costs. These are mean and sd estimates from over all modes from the expenditure survey
  pds=subset(pds_all, period==p)
  
  trip_costs=data.frame(read_excel("trip_costs_state_summary.xlsx"))                
  mean_cost=trip_costs$mean_cost[trip_costs$state==state1]
  sd_cost=trip_costs$sd_cost[trip_costs$state==state1]
  trip_data=pds
  trip_data$cost=rnorm(nrow(trip_data), mean=mean_cost,sd= sd_cost)
  trip_data[is.na(trip_data)] = 0
  
  
  # Costs_new_state data sets will retain raw trip outcomes from the baseline scenario. 
  # We will merge these data to the prediction year outcomes to calculate changes in CS. 
  costs_new_MD[[p]] = subset(trip_data, select=c(tripid, cost, catch_draw, tot_keep, tot_rel, tot_sf_catch,
                                                 tot_keep_bsb,tot_rel_bsb, tot_keep_wf, tot_rel_wf  ))
  
  names(costs_new_MD[[p]])[names(costs_new_MD[[p]]) == "tot_keep"] = "tot_keep_sf_base"
  names(costs_new_MD[[p]])[names(costs_new_MD[[p]]) == "tot_rel"] = "tot_rel_sf_base"
  names(costs_new_MD[[p]])[names(costs_new_MD[[p]]) == "tot_keep_bsb"] = "tot_keep_bsb_base"
  names(costs_new_MD[[p]])[names(costs_new_MD[[p]]) == "tot_rel_bsb"] = "tot_rel_bsb_base"
  names(costs_new_MD[[p]])[names(costs_new_MD[[p]]) == "tot_keep_wf"] = "tot_keep_wf_base"
  names(costs_new_MD[[p]])[names(costs_new_MD[[p]]) == "tot_rel_wf"] = "tot_rel_wf_base"
  
  costs_new_MD[[p]]$period = p
  
  
  
  
  #set up an output file for each draw of utility parameters
  parameter_draws_MD = list()
  
  for(d in 1:1) {
    
    param_draws_MD= utilites_MD[[1]]
    trip_data =  merge(param_draws_MD,trip_data,by="tripid")   
    
    
    #Expected utility
    trip_data$vA = trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep) +
      trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel) +  
      trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
      trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
      trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf) +
      trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf) + 
      trip_data$beta_cost*trip_data$cost 
    
    trip_data$period=as.numeric(trip_data$period)
    
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
    mean_trip_data$vA_optout= mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
    mean_trip_data$vA_striper_blue= mean_trip_data$beta_striper_blue*mean_trip_data$striper_blue  +
                                    mean_trip_data$beta_cost*mean_trip_data$cost 
    
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
    mean_trip_data = subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum,
                                                              beta_cost, beta_striper_blue, beta_opt_out, 
                                                              beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep, 
                                                              beta_sqrt_wf_keep, beta_sqrt_wf_release)) 
    
    
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
    mean_trip_data$n_choice_occasions=1
    
    list_names = colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                          & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period"
                                          & colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" ]
    
    for (l in list_names){
      mean_trip_data[,l] = mean_trip_data[,l]*expand
    }
    

    mean_trip_data$sim=1
    
    #sum probability weighted catch over all choice occasions
    aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)

    
    aggregate_trip_data = subset(aggregate_trip_data, select=-c(Group.1, tripid, catch_draw, period, cost, vA ,sim))
    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] = "observed_trips"
    
    
    aggregate_trip_data$sim = d
    
    parameter_draws_MD[[d]]=aggregate_trip_data
    
  }
  
  # Combine the output from all the utility parameter draws (for now we only have one)
  parameter_draws_all_MD = as.data.frame(bind_rows(parameter_draws_MD[[1]]))
  parameter_draws_all_MD[is.na(parameter_draws_all_MD)] = 0
  rownames(parameter_draws_all_MD) = NULL
  
  
  parameter_draws_all_MD$period=p
  pds_new[[p]]=parameter_draws_all_MD
  
}


pds_new_all_MD=list.stack(pds_new, fill=TRUE)
pds_new_all_MD[is.na(pds_new_all_MD)] = 0
pds_new_all_MD$state = state1
pds_new_all_MD$alt_regs = 0
pds_new_all_MD= subset(pds_new_all_MD, select=-c(Group.1, tot_sf_catch, tot_bsb_catch))
rm(pds_new)



# costs_new_all contain trip outcomes for the baseline period. Will use to calculate welfare changes, 
# and assign catch-per-trip in the prediction years. 
costs_new_all_MD=list.stack(costs_new_MD, fill=TRUE)
costs_new_all_MD[is.na(costs_new_all_MD)] = 0
rm(costs_new_MD)




sum(pds_new_all_MD$tot_keep)
((79370.9-sum(pds_new_all_MD$tot_keep))/79370.9)*100

sum(pds_new_all_MD$tot_rel)
((938193.2-sum(pds_new_all_MD$tot_rel))/938193.2)*100

pds_new_all_MD$tot_sf_cat=pds_new_all_MD$tot_keep+pds_new_all_MD$tot_rel
sum(pds_new_all_MD$tot_sf_cat)
((1017564.2-sum(pds_new_all_MD$tot_sf_cat))/1017564.2)*100


sum(pds_new_all_MD$tot_keep_bsb)
((13271.8-sum(pds_new_all_MD$tot_keep_bsb))/13271.8)*100

sum(pds_new_all_MD$tot_rel_bsb)
((535686.1-sum(pds_new_all_MD$tot_rel_bsb))/535686.1)*100

pds_new_all_MD$tot_bsb_cat=pds_new_all_MD$tot_keep_bsb+pds_new_all_MD$tot_rel_bsb
sum(pds_new_all_MD$tot_bsb_cat)
((548957.9-sum(pds_new_all_MD$tot_sf_cat))/548957.9)*100




sum(pds_new_all_MD$tot_keep_wf)
((20.162733-sum(pds_new_all_MD$tot_keep_wf))/20.162733)*100

sum(pds_new_all_MD$tot_rel_wf)
((2079.7371-sum(pds_new_all_MD$tot_rel_wf))/2079.7371)*100

pds_new_all_MD$tot_wf_cat=pds_new_all_MD$tot_keep_wf+pds_new_all_MD$tot_rel_wf
sum(pds_new_all_MD$tot_wf_cat)
((2099.8997-sum(pds_new_all_MD$tot_wf_cat))/2099.8997)*100
