

###Notes:
# There are several pieces of information that vary by state or regions:
#     By state: trip costs, percent of trips taken by each mode (used for assigning trip costs), 
#               and regulations (which also vary within a a season for a given state)
#     By state: catch-per-trip and catch-at-length distributions
#     By region (MA-NY, NJ, DE-MD, VA-NC): utility parameters and target species 


# This code requires the following data:
# 1) The output from the calibration: calibration_output_by_period.xlsx
# 2) Dataset containing the alternative regulations to be imposed
# 3) Abundance adjusted catch-at-length for summer flounder. 
#    This consists of two output files from the "catch at length given stock structure" script:
#       a) "predicted_catch_(STATE)" which gives catch-per-trip based on population abundance
#       b)  "sf_fitted_sizes_y2plus.xlsx" which gives a new size distribution based on historical recreational 
#           selectivity and projected population abundances at length. 


state1="NC"
region1="SO"


# Input the calibration output which contains the number of choice occasions needed to simulate
calibration_data = data.frame(read_excel("calibration_output_by_period.xlsx"))
calibration_data = subset(calibration_data, state == state1, select=c(period, sim, state, n_choice_occasions))


# Input the data set containing alterntative regulations and directed trips
directed_trips= subset(directed_trip_alt_regs, state == state1)

min_period=min(directed_trips$period)
max_period=max(directed_trips$period)


######################################
##   Begin simulating trip outcomes ##
######################################

# Set up an output file for the separately simulated within-season regulatory periods  
pds = list()


periodz=as.factor(directed_trips$period)
levels(periodz)

for(p in levels(periodz)){
  directed_trips_p = subset(directed_trips, period == p)
  n_trips = mean(directed_trips_p$dtrip_2019)
  #n_draws = min(1000,n_trips*2.5 )
  n_draws = n_drawz
  fluke_bag = mean(directed_trips_p$fluke_bag)
  fluke_min = mean(directed_trips_p$fluke_min)
  fluke_max = mean(directed_trips_p$fluke_max)
  bsb_bag = mean(directed_trips_p$bsb_bag)
  bsb_min = mean(directed_trips_p$bsb_min)
  wf_bag = mean(directed_trips_p$wf_bag)
  wf_min = mean(directed_trips_p$wf_min)
  rd_bag = mean(directed_trips_p$rd_bag)
  rd_min = mean(directed_trips_p$rd_min)
  rd_max = mean(directed_trips_p$rd_max)
  
  
  # Set up an output file for the separate catch draw files 
  dfs = list()
  
  for(i in 1:10) {
    
    # Input catch-per-trip numbers 
    sf_catch_data = data.frame(read_excel("predicted_catch_NC.xlsx"))                                                                            
    tot_sf_catch = sf_catch_data$sf_t_nb
    tot_bsb_catch = sf_catch_data$bsb_t_nb
    sf_catch_data = data.frame(tot_sf_catch,tot_bsb_catch)
    
    # random draw of fluke and bsb catch
    sf_catch_data = as.data.frame(sf_catch_data[sample(1:nrow(sf_catch_data), n_draws), ])
    sf_catch_data$tripid = 1:nrow(sf_catch_data)
    sf_bsb_catch_data = sf_catch_data
    
    
    
    # subset trips with zero catch, as no size draws are required
    sf_zero_catch = subset(sf_catch_data, tot_sf_catch == 0)
    
    
    
    #remove trips with zero summer flounder catch
    sf_catch_data=sf_catch_data[sf_catch_data$tot_sf_catch!=0, ]
    
    
    
    #expand the sf_catch_data so that each row represents a fish
    row_inds = seq_len(nrow(sf_catch_data))
    sf_catch_data = sf_catch_data[c(rep(row_inds, sf_catch_data$tot_sf_catch)), ]
    rownames(sf_catch_data) = NULL
    sf_catch_data$fishid = 1:nrow(sf_catch_data)
    
    
    #Start Gavin code insert
    size_data_read <- readRDS("sf_fitted_sizes_y2plus.rds") %>% tibble()
    size_data <- size_data_read %>% filter(region == state1)
    
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
      #left_join(regs, by = "period") %>% 
      mutate(posskeep = ifelse(fitted_length>=fluke_min & fitted_length<=fluke_max,1,0)) %>% 
      group_by(tripid) %>% 
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
    
    
    catch_size_data= subset(catch_size_data, select=c(fishid, fitted_length, tot_sf_catch, tot_bsb_catch, tripid, keep_adj, release)) %>% 
      rename(keep = keep_adj)
    #names(catch_size_data)[names(catch_size_data) == "keep_adj"] = "keep"
    
    new_size_data <- catch_size_data %>% 
      group_by( tripid, fitted_length) %>% 
      summarize(keep = sum(keep),
                release = sum(release), .groups = "drop") #%>% 
    
    # generate sum of number of kept and released fish by tripid
    summed_catch_data <- catch_size_data %>% 
      group_by( tripid) %>% 
      summarize(tot_keep = sum(keep),
                tot_rel = sum(release),
                tot_bsb_catch = mean(tot_bsb_catch),
                .groups = "drop") #%>% 
    
    
    keep_size_data <- new_size_data %>%
      ungroup() %>%
      dplyr::select(-release) %>% 
      pivot_wider(names_from = fitted_length, #_length,
                  names_glue = "keep_length_{fitted_length}",
                  names_sort = TRUE,
                  values_from = keep, 
                  values_fill = 0)# %>% 
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
    
    trip_data <- summed_catch_data %>% 
      left_join(keep_size_data, by = c( "tripid")) %>% 
      left_join(release_size_data, by = c( "tripid")) #%>% 
    #I()
    #trip_data
    
    #add the zero catch trips 
    trip_data <- bind_rows(trip_data, sf_zero_catch) %>% 
      #arrange(period, catch_draw, tripid) %>% 
      mutate_if(is.numeric, replace_na, replace = 0) %>% 
      mutate_if(is.character, replace_na, replace = region1) #%>%
    trip_data$tot_sf_catch = trip_data$tot_keep+trip_data$tot_rel
    
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
      bsb_catch_data1$keep = ifelse(bsb_catch_data1$uniform>=.92, 1,0) 
      
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
    
    
    
    # merge catch information for other species. Assume per-trip catch outcomes for these species are the same as the calibration. 
    # This info is contained in the costs_new_all_state datasets
    wf_rd_data=subset(costs_new_all_NC, period ==p & catch_draw==i, select=c(tripid, tot_keep_wf_base, tot_rel_wf_base, tot_keep_rd_base, tot_rel_rd_base )) 
    
    
    names(wf_rd_data)[names(wf_rd_data) == "tot_keep_wf_base"] = "tot_keep_wf"
    names(wf_rd_data)[names(wf_rd_data) == "tot_rel_wf_base"] = "tot_rel_wf"
    names(wf_rd_data)[names(wf_rd_data) == "tot_keep_rd_base"] = "tot_keep_rd"
    names(wf_rd_data)[names(wf_rd_data) == "tot_rel_rd_base"] = "tot_rel_rd" 
    
    # merge the trip data (summer flounder catch + lengths) with the other species data (numbers kept and released))
    trip_data =  merge(trip_data,wf_rd_data,by="tripid")
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

pds_all= list.stack(pds, fill=TRUE)
pds_all[is.na(pds_all)] = 0
rm(pds)

######################################
##   End simulating trip outcomes   ##
######################################

# Now calculate trip probabilities and utilities based on the multiple catch draws for each choice occasion

pds_new <- list()
for(p in levels(periodz)){
  
  # Merge the prediction year data to the calibration data
  pds <- subset(pds_all, period==p)
  
  cost_data <- subset(costs_new_all_NC, period == p, select=-c(period, tot_sf_catch))
  trip_data <-  merge(pds,cost_data,by=c("tripid", "catch_draw"))
  trip_data[is.na(trip_data)] <- 0
  
  
  #set up an output file for each draw of utility parameters. For now, only taking one draw. 
  parameter_draws <- list()
  
  for(d in 1:1) {
    
    # Use the previously drawn set of utility parameters to calculate expected utility, welfare, and effort in the prediction year
    #param_draws_NC_prediction <- subset(param_draws_NC, parameter_draw==d)
    param_draws_NC_prediction <- subset(param_draws_NC)
    
    trip_data <-  merge(param_draws_NC_prediction,trip_data,by="tripid")
    
    
    # Expected utility (prediction year)
    trip_data$vA <- trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep) +
      trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel) +  
      trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb) +
      trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb) +  
      trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf) +
      trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf) +   
      trip_data$beta_sqrt_rd_keep*sqrt(trip_data$tot_keep_rd) +
      trip_data$beta_sqrt_rd_release*sqrt(trip_data$tot_rel_rd) +  
      trip_data$beta_cost*trip_data$cost 
    
    # Expected utility (base year)
    trip_data$v0 <- trip_data$beta_sqrt_sf_keep*sqrt(trip_data$tot_keep_sf_base) +
      trip_data$beta_sqrt_sf_release*sqrt(trip_data$tot_rel_sf_base) +  
      trip_data$beta_sqrt_bsb_keep*sqrt(trip_data$tot_keep_bsb_base) +
      trip_data$beta_sqrt_bsb_release*sqrt(trip_data$tot_rel_bsb_base) +  
      trip_data$beta_sqrt_wf_keep*sqrt(trip_data$tot_keep_wf_base) +
      trip_data$beta_sqrt_wf_release*sqrt(trip_data$tot_rel_wf_base) + 
      trip_data$beta_sqrt_rd_keep*sqrt(trip_data$tot_keep_rd_base) +
      trip_data$beta_sqrt_rd_release*sqrt(trip_data$tot_rel_rd_base) +  
      trip_data$beta_cost*trip_data$cost 
    
    trip_data$period <- as.numeric(trip_data$period)
    
    # Collapse data from the X catch draws so that each row contains mean values
    mean_trip_data <-aggregate(trip_data, by=list(trip_data$tripid),FUN=mean, na.rm=TRUE)
    
    # Now expand the data to create three alternatives, representing the alternatives available in choice survey
    mean_trip_data <- expandRows(mean_trip_data, 3, count.is.col = FALSE)
    
    #Alt 1, 2, 3
    mean_trip_data$alt <- sequence(tabulate(mean_trip_data$tripid))
    
    #Alt 2 and 3 are the opt_out and other_fishing alternatives
    mean_trip_data$opt_out <- ifelse(mean_trip_data$alt!=1 & mean_trip_data$alt!=2, 1,0) 
    mean_trip_data$striper_blue <- ifelse(mean_trip_data$alt!=1 & mean_trip_data$alt!=3, 1,0) 
    
    #Caluculate the expected utility of alts 2 and 3 based on the parameters of the utility function
    #These will be the same for both v0 and v1
    mean_trip_data$vA_optout <- mean_trip_data$beta_opt_out*mean_trip_data$opt_out 
    mean_trip_data$vA_striper_blue <- mean_trip_data$beta_striper_blue*mean_trip_data$striper_blue  +
                                      mean_trip_data$beta_cost*mean_trip_data$cost 
    
    #Now put these three values in the same column, exponentiate, and caluculate their sum (vA_col_sum)
    mean_trip_data$vA[mean_trip_data$alt!=1] <- 0
    mean_trip_data$v0[mean_trip_data$alt!=1] <- 0
    
    mean_trip_data$vA_row_sum <- rowSums(mean_trip_data[,c("vA", "vA_striper_blue","vA_optout")])
    mean_trip_data$vA_row_sum <- exp(mean_trip_data$vA_row_sum)
    mean_trip_data$vA_col_sum <- ave(mean_trip_data$vA_row_sum, mean_trip_data$tripid, FUN = sum)
    
    mean_trip_data$v0_row_sum <- rowSums(mean_trip_data[,c("v0", "vA_striper_blue","vA_optout")])
    mean_trip_data$v0_row_sum <- exp(mean_trip_data$v0_row_sum)
    mean_trip_data$v0_col_sum <- ave(mean_trip_data$v0_row_sum, mean_trip_data$tripid, FUN = sum)
    
    
    #change in Consmer surplus between prediction year and baseline year 
    mean_trip_data$change_CS <- (1/mean_trip_data$beta_cost)*(log(mean_trip_data$vA_col_sum) - log(mean_trip_data$v0_col_sum))
    
    
    # Caluculate the probability of a respondent selected each alternative based on 
    # exponentiated expected utility of the altenrative [exp(expected utility, alt=i] 
    # and the sum of exponentiated expected utility across the three altenratives.
    # You will notice the striper_blue alternative has a large proabability based on the utility parameters
    mean_trip_data$probA <- mean_trip_data$vA_row_sum/mean_trip_data$vA_col_sum
    mean_trip_data$prob0 <- mean_trip_data$v0_row_sum/mean_trip_data$v0_col_sum
    
    # Get rid of things we don't need. 
    mean_trip_data <- subset(mean_trip_data, alt==1, select=-c(alt, opt_out, striper_blue, vA_optout, vA_striper_blue, vA_row_sum, vA_col_sum, v0_row_sum, v0_col_sum,
                                                               beta_cost, beta_striper_blue, beta_opt_out, 
                                                               beta_sqrt_bsb_release, beta_sqrt_bsb_keep, beta_sqrt_sf_release, beta_sqrt_sf_keep, 
                                                               beta_sqrt_wf_release, beta_sqrt_wf_keep, beta_sqrt_rd_release, beta_sqrt_rd_keep))
    
    
    # Multiply the average trip probability by each of the catch variables (not the variable below) to get probability-weighted catch
    list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                           & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period" & colnames(mean_trip_data) !="cost" 
                                           & colnames(mean_trip_data) !="tot_keep_sf_base" & colnames(mean_trip_data) !="tot_rel_sf_base" 
                                           & colnames(mean_trip_data) !="tot_keep_bsb_base" & colnames(mean_trip_data) !="tot_rel_bsb_base" 
                                           & colnames(mean_trip_data) !="tot_keep_wf_base" & colnames(mean_trip_data) !="tot_rel_wf_base" 
                                           & colnames(mean_trip_data) !="tot_keep_rd_base" & colnames(mean_trip_data) !="tot_rel_rd_base" 
                                           & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"  & colnames(mean_trip_data) !="probA"
                                           & colnames(mean_trip_data) !="prob0" & colnames(mean_trip_data) !="change_CS"  ]
    
    for (l in list_names){
      mean_trip_data[,l] <- mean_trip_data[,l]*mean_trip_data$probA
    }
    
    #Now multiply the trip outcomes (catch, trip probabilities) for each choice occasion in 
    #mean_trip_pool by the expansion factor (expand), so that  each choice occasion represents a certain number of choice occasions
    sims <- subset(calibration_data, period==p & sim==d)
    n_choice_occasions <- mean(sims$n_choice_occasions)
    ndraws <- nrow(mean_trip_data)
    expand <- n_choice_occasions/ndraws
    mean_trip_data$n_choice_occasions <- 1
    
    list_names <- colnames(mean_trip_data)[colnames(mean_trip_data) !="Group.1" & colnames(mean_trip_data) !="tripid" 
                                           & colnames(mean_trip_data) !="catch_draw" & colnames(mean_trip_data) !="period"
                                           & colnames(mean_trip_data) !="tot_keep_sf_base" & colnames(mean_trip_data) !="tot_rel_sf_base" 
                                           & colnames(mean_trip_data) !="tot_keep_bsb_base" & colnames(mean_trip_data) !="tot_rel_bsb_base" 
                                           & colnames(mean_trip_data) !="tot_keep_wf_base" & colnames(mean_trip_data) !="tot_rel_wf_base" 
                                           & colnames(mean_trip_data) !="tot_keep_rd_base" & colnames(mean_trip_data) !="tot_rel_rd_base" 
                                           & colnames(mean_trip_data) !="cost" & colnames(mean_trip_data) !="vA" & colnames(mean_trip_data) !="v0"]
    
    for (l in list_names){
      mean_trip_data[,l] <- mean_trip_data[,l]*expand
    }
    
    
    mean_trip_data$sim <- 1
    
    #sum probability weighted catch over all choice occasions
    aggregate_trip_data <-aggregate(mean_trip_data, by=list(mean_trip_data$sim),FUN=sum, na.rm=TRUE)
    
    
    aggregate_trip_data <- subset(aggregate_trip_data, select=-c(Group.1, Group.1, tripid, catch_draw, period, cost, vA , v0, sim, prob0))
    names(aggregate_trip_data)[names(aggregate_trip_data) == "probA"] <- "observed_trips"
    
    
    aggregate_trip_data$sim <- d
    
    parameter_draws[[d]] <- aggregate_trip_data
    
  }
  
  parameter_draws_all <- as.data.frame(bind_rows(parameter_draws[[1]]))
  
  
  parameter_draws_all[is.na(parameter_draws_all)] <- 0
  rownames(parameter_draws_all) <- NULL
  
  
  parameter_draws_all$period <- p
  pds_new[[p]] <- parameter_draws_all
  
}


pds_new_all_NC <- list.stack(pds_new, fill=TRUE)

pds_new_all_NC[is.na(pds_new_all_NC)] <- 0
pds_new_all_NC$state <- state1
pds_new_all_NC$alt_regs <- 1
pds_new_all_NC <- subset(pds_new_all_NC, select=-c(Group.1, tot_keep_sf_base, tot_rel_sf_base, 
                                                   tot_keep_wf_base, tot_rel_wf_base, 
                                                   tot_keep_rd_base, tot_rel_rd_base, 
                                                   tot_keep_bsb_base, tot_rel_bsb_base, tot_sf_catch))