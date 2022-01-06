

# This script will create tables comparing harvest, discards, and catch by state and coastwide 
# between the simulation model and  MRIP for fluke and other species. Note that MRIP catch 
# statistics for other species are based on trips that caught or primarily targeted fluke. 

# The first set of code, under "Calibration year statistics", compute comparative statistics 
# in the calibration year 2019. Requires MRIP statistics contained in "catch_trips_prediction_years.xlsx"

# The second set of code, under "Prediction year statistics",
# computes comparative statistics for "in-sample" predictions. That is, it compares predicted catch 
# statistics from the simulation model to observed MRIP statistics for 2016, 2017, or 2018. This 
# portion of the code is only relevant when computing in-sample predictions for these years. Predictions
# for these years require their relevant numbers-at-age and regulation file to be incorporated into the 
# simulation model. I have commented this portion of the code out for now. Requires MRIP statistics 
# contained in "catch_trips_prediction_years.xlsx" and "catch_trips_prediction_years_wide.xlsx"

# The code creates multiple .txt tables, which can then be automatically read into an 
# excel file for easy viewing. 

# For in-sample prediction year code, select the comparison year (2018, 2017, or 2016)
prediction_year = 2018





##############################
##Calibration year statistics 
##############################

state_calibration_output= subset(calibration_output_by_period, select=c(tot_keep, tot_rel,tot_keep_bsb, tot_rel_bsb,tot_keep_scup, tot_rel_scup,
                                                                        tot_keep_wf, tot_rel_wf, tot_keep_rd, tot_rel_rd, observed_trips, n_choice_occasions, period, state))

state_calibration_output$state1=with(state_calibration_output, match(state, unique(state)))
state_calibration_output1= subset(state_calibration_output, select=-c(state, period))
state_calibration_output1=aggregate(state_calibration_output1, by=list(state_calibration_output1$state1),FUN=sum, na.rm=TRUE)
state_calibration_output1= subset(state_calibration_output1, select=-c(state1))
names(state_calibration_output1)[names(state_calibration_output1) == "Group.1"] = "state1"
state_calibration_output1$year=2019

state_names=subset(state_calibration_output, select=c(state, state1))
state_names = state_names[!duplicated(state_names), ]
state_calibration_output1 =  merge(state_calibration_output1,state_names,by="state1", all.x=TRUE, all.y=TRUE)

MRIP_data = data.frame(read_excel("catch_trips_prediction_years.xlsx"))                                                                            
MRIP_data_2019 = subset(MRIP_data, year==2019)                                                                           

#correct for outlying observations in MRIP data for VA 2019
MRIP_data_2019$sf_release_MRIP[MRIP_data_2019$state=="VA"] <- 1367379
MRIP_data_2019$sf_tot_cat_MRIP[MRIP_data_2019$state=="VA"] <- 1517164

output_sim_MRIP_2019 =  merge(MRIP_data_2019,state_calibration_output1,by=c("state", "year"), all.x=TRUE, all.y=TRUE)

#make sure trips mach up
output_sim_MRIP_2019$diff_trips=output_sim_MRIP_2019$dtrip_MRIP - output_sim_MRIP_2019$observed_trips

#summer flounder stats (MRIP minus simulation)
output_sim_MRIP_2019$diff_sf_keep = output_sim_MRIP_2019$sf_keep_MRIP - output_sim_MRIP_2019$tot_keep  
output_sim_MRIP_2019$diff_sf_rel =  output_sim_MRIP_2019$sf_release_MRIP-output_sim_MRIP_2019$tot_rel 
output_sim_MRIP_2019$sf_tot_cat = output_sim_MRIP_2019$tot_keep+output_sim_MRIP_2019$tot_rel
output_sim_MRIP_2019$diff_sf_tot_cat =  output_sim_MRIP_2019$sf_tot_cat_MRIP-output_sim_MRIP_2019$sf_tot_cat 

output_sim_MRIP_2019$perc_diff_sf_keep = ((output_sim_MRIP_2019$sf_keep_MRIP-output_sim_MRIP_2019$tot_keep  )/output_sim_MRIP_2019$sf_keep_MRIP)*100
output_sim_MRIP_2019$perc_diff_sf_rel = ((  output_sim_MRIP_2019$sf_release_MRIP-output_sim_MRIP_2019$tot_rel)/output_sim_MRIP_2019$sf_release_MRIP)*100
output_sim_MRIP_2019$perc_diff_sf_tot_cat =  ((output_sim_MRIP_2019$sf_tot_cat_MRIP-output_sim_MRIP_2019$sf_tot_cat)/output_sim_MRIP_2019$sf_tot_cat_MRIP)*100


#black sea bass stats (MRIP minus simulation)
output_sim_MRIP_2019$diff_bsb_keep = output_sim_MRIP_2019$bsb_keep_MRIP -output_sim_MRIP_2019$tot_keep_bsb  
output_sim_MRIP_2019$diff_bsb_rel =  output_sim_MRIP_2019$bsb_release_MRIP-output_sim_MRIP_2019$tot_rel_bsb 
output_sim_MRIP_2019$bsb_tot_cat = output_sim_MRIP_2019$tot_keep_bsb+output_sim_MRIP_2019$tot_rel_bsb
output_sim_MRIP_2019$diff_bsb_tot_cat = output_sim_MRIP_2019$bsb_tot_cat_MRIP-output_sim_MRIP_2019$bsb_tot_cat

output_sim_MRIP_2019$perc_diff_bsb_keep = ((output_sim_MRIP_2019$bsb_keep_MRIP-output_sim_MRIP_2019$tot_keep_bsb )/output_sim_MRIP_2019$bsb_keep_MRIP)*100
output_sim_MRIP_2019$perc_diff_bsb_rel = (( output_sim_MRIP_2019$bsb_release_MRIP-output_sim_MRIP_2019$tot_rel_bsb )/output_sim_MRIP_2019$bsb_release_MRIP)*100
output_sim_MRIP_2019$perc_diff_bsb_tot_cat =  ((output_sim_MRIP_2019$bsb_tot_cat_MRIP-output_sim_MRIP_2019$bsb_tot_cat)/output_sim_MRIP_2019$bsb_tot_cat_MRIP)*100


#scup stats (MRIP minus simulation)
output_sim_MRIP_2019$diff_scup_keep = output_sim_MRIP_2019$scup_keep_MRIP-output_sim_MRIP_2019$tot_keep_scup  
output_sim_MRIP_2019$diff_scup_rel =  output_sim_MRIP_2019$scup_release_MRIP-output_sim_MRIP_2019$tot_rel_scup 
output_sim_MRIP_2019$scup_tot_cat = output_sim_MRIP_2019$tot_keep_scup+output_sim_MRIP_2019$tot_rel_scup
output_sim_MRIP_2019$diff_scup_tot_cat =  output_sim_MRIP_2019$scup_tot_cat_MRIP-output_sim_MRIP_2019$scup_tot_cat

output_sim_MRIP_2019$perc_diff_scup_keep = ((output_sim_MRIP_2019$scup_keep_MRIP-output_sim_MRIP_2019$tot_keep_scup  )/output_sim_MRIP_2019$scup_keep_MRIP)*100
output_sim_MRIP_2019$perc_diff_scup_rel = (( output_sim_MRIP_2019$scup_release_MRIP-output_sim_MRIP_2019$tot_rel_scup )/output_sim_MRIP_2019$scup_release_MRIP)*100
output_sim_MRIP_2019$perc_diff_scup_tot_cat =  ((output_sim_MRIP_2019$scup_tot_cat_MRIP-output_sim_MRIP_2019$scup_tot_cat)/output_sim_MRIP_2019$scup_tot_cat_MRIP)*100


#wf stats (MRIP minus simulation)
output_sim_MRIP_2019$diff_wf_keep = output_sim_MRIP_2019$wf_keep_MRIP-output_sim_MRIP_2019$tot_keep_wf 
output_sim_MRIP_2019$diff_wf_rel =  output_sim_MRIP_2019$wf_release_MRIP-output_sim_MRIP_2019$tot_rel_wf 
output_sim_MRIP_2019$wf_tot_cat = output_sim_MRIP_2019$tot_keep_wf+output_sim_MRIP_2019$tot_rel_wf
output_sim_MRIP_2019$diff_wf_tot_cat =  output_sim_MRIP_2019$wf_tot_cat_MRIP-output_sim_MRIP_2019$wf_tot_cat 

output_sim_MRIP_2019$perc_diff_wf_keep = ((output_sim_MRIP_2019$wf_keep_MRIP-output_sim_MRIP_2019$tot_keep_wf  )/output_sim_MRIP_2019$wf_keep_MRIP)*100
output_sim_MRIP_2019$perc_diff_wf_rel = (( output_sim_MRIP_2019$wf_release_MRIP-output_sim_MRIP_2019$tot_rel_wf )/output_sim_MRIP_2019$wf_release_MRIP)*100
output_sim_MRIP_2019$perc_diff_wf_tot_cat =  ((output_sim_MRIP_2019$wf_tot_cat_MRIP-output_sim_MRIP_2019$wf_tot_cat )/output_sim_MRIP_2019$wf_tot_cat_MRIP)*100


#rd stats (MRIP minus simulation)
output_sim_MRIP_2019$diff_rd_keep = output_sim_MRIP_2019$rd_keep_MRIP -output_sim_MRIP_2019$tot_keep_rd  
output_sim_MRIP_2019$diff_rd_rel =  output_sim_MRIP_2019$rd_release_MRIP -output_sim_MRIP_2019$tot_rel_rd 
output_sim_MRIP_2019$rd_tot_cat = output_sim_MRIP_2019$tot_keep_rd+output_sim_MRIP_2019$tot_rel_rd
output_sim_MRIP_2019$diff_rd_tot_cat = output_sim_MRIP_2019$rd_tot_cat_MRIP-output_sim_MRIP_2019$rd_tot_cat 

output_sim_MRIP_2019$perc_diff_rd_keep = ((output_sim_MRIP_2019$rd_keep_MRIP-output_sim_MRIP_2019$tot_keep_rd  )/output_sim_MRIP_2019$rd_keep_MRIP)*100
output_sim_MRIP_2019$perc_diff_rd_rel = (( output_sim_MRIP_2019$rd_release_MRIP-output_sim_MRIP_2019$tot_rel_rd )/output_sim_MRIP_2019$rd_release_MRIP)*100
output_sim_MRIP_2019$perc_diff_rd_tot_cat =  ((output_sim_MRIP_2019$rd_tot_cat_MRIP-output_sim_MRIP_2019$rd_tot_cat )/output_sim_MRIP_2019$rd_tot_cat_MRIP)*100


#Create calibration tables
#SF harvest
sf_harvest_calibration=subset(output_sim_MRIP_2019, select=c(state, sf_keep_MRIP, tot_keep, diff_sf_keep, perc_diff_sf_keep))
sf_harvest_calibration_total_sim=sum(sf_harvest_calibration$tot_keep)
sf_harvest_calibration_total_MRIP=sum(sf_harvest_calibration$sf_keep_MRIP)
state="All"
sf_harvest_calibration_totals=data.frame(state, sf_harvest_calibration_total_sim,sf_harvest_calibration_total_MRIP)
names(sf_harvest_calibration_totals)<-c("state","tot_keep", "sf_keep_MRIP")
sf_harvest_calibration_totals$diff_sf_keep=sf_harvest_calibration_totals$sf_keep_MRIP - sf_harvest_calibration_totals$tot_keep
sf_harvest_calibration_totals$perc_diff_sf_keep = ((sf_harvest_calibration_totals$sf_keep_MRIP-sf_harvest_calibration_totals$tot_keep  )/sf_harvest_calibration_totals$sf_keep_MRIP)*100


sf_harvest_calibration=rbind(sf_harvest_calibration,sf_harvest_calibration_totals)
sf_harvest_calibration <- data.frame(lapply(sf_harvest_calibration,    # Using Base R functions
                                            function(x) if(is.numeric(x)) round(x, 1) else x))

names(sf_harvest_calibration)<-c("state","SF harvest: MRIP", "SF harvest: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
sf_harvest_calibration =  merge(sf_harvest_calibration,state_names,by="state", all.x=TRUE, all.y=TRUE)
sf_harvest_calibration = sf_harvest_calibration[order(sf_harvest_calibration$state1),]  
sf_harvest_calibration = subset(sf_harvest_calibration, select=-c(state1))
write.table(sf_harvest_calibration, file = "sf_harvest_calibration.txt", sep = ",", quote = FALSE, row.names = F)

#SF discards
sf_releases_calibration=subset(output_sim_MRIP_2019, select=c(state, sf_release_MRIP, tot_rel, diff_sf_rel, perc_diff_sf_rel))
sf_releases_calibration_total_sim=sum(sf_releases_calibration$tot_rel)
sf_releases_calibration_total_MRIP=sum(sf_releases_calibration$sf_release_MRIP)
state="All"
sf_releases_calibration_totals=data.frame(state, sf_releases_calibration_total_sim,sf_releases_calibration_total_MRIP)
names(sf_releases_calibration_totals)<-c("state","tot_rel", "sf_release_MRIP")
sf_releases_calibration_totals$diff_sf_rel=sf_releases_calibration_totals$sf_release_MRIP -sf_releases_calibration_totals$tot_rel
sf_releases_calibration_totals$perc_diff_sf_rel = ((sf_releases_calibration_totals$sf_release_MRIP-sf_releases_calibration_totals$tot_rel )/sf_releases_calibration_totals$sf_release_MRIP)*100
sf_releases_calibration=rbind(sf_releases_calibration,sf_releases_calibration_totals)
sf_releases_calibration <- data.frame(lapply(sf_releases_calibration,    # Using Base R functions
                                             function(x) if(is.numeric(x)) round(x, 1) else x))

names(sf_releases_calibration)<-c("state","SF releases: MRIP", "SF releases: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
sf_releases_calibration =  merge(sf_releases_calibration,state_names,by="state", all.x=TRUE, all.y=TRUE)
sf_releases_calibration = sf_releases_calibration[order(sf_releases_calibration$state1),]  
sf_releases_calibration = subset(sf_releases_calibration, select=-c(state1))
write.table(sf_releases_calibration, file = "sf_releases_calibration.txt", sep = ",", quote = FALSE, row.names = F)


#SF total catch
sf_tot_cat_calibration=subset(output_sim_MRIP_2019, select=c(state, sf_tot_cat_MRIP, sf_tot_cat, diff_sf_tot_cat, perc_diff_sf_tot_cat))
sf_tot_cat_calibration_total_sim=sum(sf_tot_cat_calibration$sf_tot_cat)
sf_tot_cat_calibration_total_MRIP=sum(sf_tot_cat_calibration$sf_tot_cat_MRIP)
state="All"
sf_tot_cat_calibration_totals=data.frame(state, sf_tot_cat_calibration_total_sim,sf_tot_cat_calibration_total_MRIP)
names(sf_tot_cat_calibration_totals)<-c("state","sf_tot_cat", "sf_tot_cat_MRIP")
sf_tot_cat_calibration_totals$diff_sf_tot_cat=sf_tot_cat_calibration_totals$sf_tot_cat_MRIP - sf_tot_cat_calibration_totals$sf_tot_cat
sf_tot_cat_calibration_totals$perc_diff_sf_tot_cat = (( sf_tot_cat_calibration_totals$sf_tot_cat_MRIP-sf_tot_cat_calibration_totals$sf_tot_cat )/sf_tot_cat_calibration_totals$sf_tot_cat_MRIP)*100
sf_tot_cat_calibration=rbind(sf_tot_cat_calibration,sf_tot_cat_calibration_totals)
sf_tot_cat_calibration <- data.frame(lapply(sf_tot_cat_calibration,    # Using Base R functions
                                            function(x) if(is.numeric(x)) round(x, 1) else x))

names(sf_tot_cat_calibration)<-c("state","SF total catch: MRIP", "SF total catch: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
sf_tot_cat_calibration =  merge(sf_tot_cat_calibration,state_names,by="state", all.x=TRUE, all.y=TRUE)
sf_tot_cat_calibration = sf_tot_cat_calibration[order(sf_tot_cat_calibration$state1),]  
sf_tot_cat_calibration = subset(sf_tot_cat_calibration, select=-c(state1))
write.table(sf_tot_cat_calibration, file = "sf_tot_cat_calibration.txt", sep = ",", quote = FALSE, row.names = F)


#bsb harvest
bsb_harvest_calibration=subset(output_sim_MRIP_2019, select=c(state, bsb_keep_MRIP, tot_keep_bsb, diff_bsb_keep, perc_diff_bsb_keep))
bsb_harvest_calibration_total_sim=sum(bsb_harvest_calibration$tot_keep_bsb)
bsb_harvest_calibration_total_MRIP=sum(bsb_harvest_calibration$bsb_keep_MRIP)
state="All"
bsb_harvest_calibration_totals=data.frame(state, bsb_harvest_calibration_total_sim,bsb_harvest_calibration_total_MRIP)
names(bsb_harvest_calibration_totals)<-c("state","tot_keep_bsb", "bsb_keep_MRIP")
bsb_harvest_calibration_totals$diff_bsb_keep=bsb_harvest_calibration_totals$bsb_keep_MRIP-bsb_harvest_calibration_totals$tot_keep_bsb
bsb_harvest_calibration_totals$perc_diff_bsb_keep = ((bsb_harvest_calibration_totals$bsb_keep_MRIP-bsb_harvest_calibration_totals$tot_keep_bsb  )/bsb_harvest_calibration_totals$bsb_keep_MRIP)*100
bsb_harvest_calibration=rbind(bsb_harvest_calibration,bsb_harvest_calibration_totals)
bsb_harvest_calibration <- data.frame(lapply(bsb_harvest_calibration,    # Using Base R functions
                                             function(x) if(is.numeric(x)) round(x, 1) else x))

names(bsb_harvest_calibration)<-c("state","BSB harvest: MRIP", "BSB harvest: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
bsb_harvest_calibration =  merge(bsb_harvest_calibration,state_names,by="state", all.x=TRUE, all.y=TRUE)
bsb_harvest_calibration = bsb_harvest_calibration[order(bsb_harvest_calibration$state1),]  
bsb_harvest_calibration = subset(bsb_harvest_calibration, select=-c(state1))
write.table(bsb_harvest_calibration, file = "bsb_harvest_calibration.txt", sep = ",", quote = FALSE, row.names = F)


#BSB discards
bsb_releases_calibration=subset(output_sim_MRIP_2019, select=c(state, bsb_release_MRIP, tot_rel_bsb, diff_bsb_rel, perc_diff_bsb_rel))
bsb_releases_calibration_total_sim=sum(bsb_releases_calibration$tot_rel_bsb)
bsb_releases_calibration_total_MRIP=sum(bsb_releases_calibration$bsb_release_MRIP)
state="All"
bsb_releases_calibration_totals=data.frame(state, bsb_releases_calibration_total_sim,bsb_releases_calibration_total_MRIP)
names(bsb_releases_calibration_totals)<-c("state","tot_rel_bsb", "bsb_release_MRIP")
bsb_releases_calibration_totals$diff_bsb_rel=bsb_releases_calibration_totals$bsb_release_MRIP-bsb_releases_calibration_totals$tot_rel_bsb
bsb_releases_calibration_totals$perc_diff_bsb_rel = ((bsb_releases_calibration_totals$bsb_release_MRIP-bsb_releases_calibration_totals$tot_rel_bsb  )/bsb_releases_calibration_totals$bsb_release_MRIP)*100
bsb_releases_calibration=rbind(bsb_releases_calibration,bsb_releases_calibration_totals)
bsb_releases_calibration <- data.frame(lapply(bsb_releases_calibration,    # Using Base R functions
                                              function(x) if(is.numeric(x)) round(x, 1) else x))

names(bsb_releases_calibration)<-c("state","BSB releases: MRIP", "BSB releases: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
bsb_releases_calibration =  merge(bsb_releases_calibration,state_names,by="state", all.x=TRUE, all.y=TRUE)
bsb_releases_calibration = bsb_releases_calibration[order(bsb_releases_calibration$state1),]  
bsb_releases_calibration = subset(bsb_releases_calibration, select=-c(state1))
write.table(bsb_releases_calibration, file = "bsb_releases_calibration.txt", sep = ",", quote = FALSE, row.names = F)


#BSB total catch
bsb_tot_cat_calibration=subset(output_sim_MRIP_2019, select=c(state, bsb_tot_cat_MRIP, bsb_tot_cat, diff_bsb_tot_cat, perc_diff_bsb_tot_cat))
bsb_tot_cat_calibration_total_sim=sum(bsb_tot_cat_calibration$bsb_tot_cat)
bsb_tot_cat_calibration_total_MRIP=sum(bsb_tot_cat_calibration$bsb_tot_cat_MRIP)
state="All"
bsb_tot_cat_calibration_totals=data.frame(state, bsb_tot_cat_calibration_total_sim,bsb_tot_cat_calibration_total_MRIP)
names(bsb_tot_cat_calibration_totals)<-c("state","bsb_tot_cat", "bsb_tot_cat_MRIP")
bsb_tot_cat_calibration_totals$diff_bsb_tot_cat=bsb_tot_cat_calibration_totals$bsb_tot_cat_MRIP-bsb_tot_cat_calibration_totals$bsb_tot_cat
bsb_tot_cat_calibration_totals$perc_diff_bsb_tot_cat = ((bsb_tot_cat_calibration_totals$bsb_tot_cat_MRIP-bsb_tot_cat_calibration_totals$bsb_tot_cat  )/bsb_tot_cat_calibration_totals$bsb_tot_cat_MRIP)*100
bsb_tot_cat_calibration=rbind(bsb_tot_cat_calibration,bsb_tot_cat_calibration_totals)
bsb_tot_cat_calibration <- data.frame(lapply(bsb_tot_cat_calibration,    # Using Base R functions
                                             function(x) if(is.numeric(x)) round(x, 1) else x))

names(bsb_tot_cat_calibration)<-c("state","BSB total catch: MRIP", "BSB total catch: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
bsb_tot_cat_calibration =  merge(bsb_tot_cat_calibration,state_names,by="state", all.x=TRUE, all.y=TRUE)
bsb_tot_cat_calibration = bsb_tot_cat_calibration[order(bsb_tot_cat_calibration$state1),]  
bsb_tot_cat_calibration = subset(bsb_tot_cat_calibration, select=-c(state1))
write.table(bsb_tot_cat_calibration, file = "bsb_tot_cat_calibration.txt", sep = ",", quote = FALSE, row.names = F)



##############################
##Prediction year statistics 
##############################
state_prediction_output= subset(prediction_output_by_period, select=c(tot_keep, tot_rel,tot_keep_bsb, tot_rel_bsb,tot_keep_scup, tot_rel_scup,
                                                                      tot_keep_wf, tot_rel_wf, tot_keep_rd, tot_rel_rd, observed_trips, n_choice_occasions, period, state))

state_prediction_output$state1=with(state_prediction_output, match(state, unique(state)))
state_prediction_output1= subset(state_prediction_output, select=-c(state, period))
state_prediction_output1=aggregate(state_prediction_output1, by=list(state_prediction_output1$state1),FUN=sum, na.rm=TRUE)
state_prediction_output1= subset(state_prediction_output1, select=-c(state1))
names(state_prediction_output1)[names(state_prediction_output1) == "Group.1"] = "state1"
state_prediction_output1$year=prediction_year
state_prediction_output1 =  merge(state_prediction_output1,state_names,by="state1", all.x=TRUE, all.y=TRUE)

MRIP_data = data.frame(read_excel("catch_trips_prediction_years.xlsx"))
MRIP_data_prediction_yr = subset(MRIP_data, year==prediction_year)


output_sim_MRIP_prediction_yr =  merge(MRIP_data_prediction_yr,state_prediction_output1,by=c("state", "year"), all.x=TRUE, all.y=TRUE)


#summer flounder stats (MRIP minus simulation)
output_sim_MRIP_prediction_yr$diff_sf_keep = output_sim_MRIP_prediction_yr$sf_keep_MRIP - output_sim_MRIP_prediction_yr$tot_keep
output_sim_MRIP_prediction_yr$diff_sf_rel =  output_sim_MRIP_prediction_yr$sf_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel
output_sim_MRIP_prediction_yr$sf_tot_cat = output_sim_MRIP_prediction_yr$tot_keep+output_sim_MRIP_prediction_yr$tot_rel
output_sim_MRIP_prediction_yr$diff_sf_tot_cat =  output_sim_MRIP_prediction_yr$sf_tot_cat_MRIP-output_sim_MRIP_prediction_yr$sf_tot_cat

output_sim_MRIP_prediction_yr$perc_diff_sf_keep = ((output_sim_MRIP_prediction_yr$sf_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep  )/output_sim_MRIP_prediction_yr$sf_keep_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_sf_rel = ((  output_sim_MRIP_prediction_yr$sf_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel)/output_sim_MRIP_prediction_yr$sf_release_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_sf_tot_cat =  ((output_sim_MRIP_prediction_yr$sf_tot_cat_MRIP-output_sim_MRIP_prediction_yr$sf_tot_cat)/output_sim_MRIP_prediction_yr$sf_tot_cat_MRIP)*100


#black sea bass stats (MRIP minus simulation)
output_sim_MRIP_prediction_yr$diff_bsb_keep = output_sim_MRIP_prediction_yr$bsb_keep_MRIP -output_sim_MRIP_prediction_yr$tot_keep_bsb
output_sim_MRIP_prediction_yr$diff_bsb_rel =  output_sim_MRIP_prediction_yr$bsb_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_bsb
output_sim_MRIP_prediction_yr$bsb_tot_cat = output_sim_MRIP_prediction_yr$tot_keep_bsb+output_sim_MRIP_prediction_yr$tot_rel_bsb
output_sim_MRIP_prediction_yr$diff_bsb_tot_cat = output_sim_MRIP_prediction_yr$bsb_tot_cat_MRIP-output_sim_MRIP_prediction_yr$bsb_tot_cat

output_sim_MRIP_prediction_yr$perc_diff_bsb_keep = ((output_sim_MRIP_prediction_yr$bsb_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep_bsb )/output_sim_MRIP_prediction_yr$bsb_keep_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_bsb_rel = (( output_sim_MRIP_prediction_yr$bsb_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_bsb )/output_sim_MRIP_prediction_yr$bsb_release_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_bsb_tot_cat =  ((output_sim_MRIP_prediction_yr$bsb_tot_cat_MRIP-output_sim_MRIP_prediction_yr$bsb_tot_cat)/output_sim_MRIP_prediction_yr$bsb_tot_cat_MRIP)*100


#scup stats (MRIP minus simulation)
output_sim_MRIP_prediction_yr$diff_scup_keep = output_sim_MRIP_prediction_yr$scup_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep_scup
output_sim_MRIP_prediction_yr$diff_scup_rel =  output_sim_MRIP_prediction_yr$scup_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_scup
output_sim_MRIP_prediction_yr$scup_tot_cat = output_sim_MRIP_prediction_yr$tot_keep_scup+output_sim_MRIP_prediction_yr$tot_rel_scup
output_sim_MRIP_prediction_yr$diff_scup_tot_cat =  output_sim_MRIP_prediction_yr$scup_tot_cat_MRIP-output_sim_MRIP_prediction_yr$scup_tot_cat

output_sim_MRIP_prediction_yr$perc_diff_scup_keep = ((output_sim_MRIP_prediction_yr$scup_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep_scup  )/output_sim_MRIP_prediction_yr$scup_keep_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_scup_rel = (( output_sim_MRIP_prediction_yr$scup_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_scup )/output_sim_MRIP_prediction_yr$scup_release_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_scup_tot_cat =  ((output_sim_MRIP_prediction_yr$scup_tot_cat_MRIP-output_sim_MRIP_prediction_yr$scup_tot_cat)/output_sim_MRIP_prediction_yr$scup_tot_cat_MRIP)*100


#wf stats (MRIP minus simulation)
output_sim_MRIP_prediction_yr$diff_wf_keep = output_sim_MRIP_prediction_yr$wf_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep_wf
output_sim_MRIP_prediction_yr$diff_wf_rel =  output_sim_MRIP_prediction_yr$wf_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_wf
output_sim_MRIP_prediction_yr$wf_tot_cat = output_sim_MRIP_prediction_yr$tot_keep_wf+output_sim_MRIP_prediction_yr$tot_rel_wf
output_sim_MRIP_prediction_yr$diff_wf_tot_cat =  output_sim_MRIP_prediction_yr$wf_tot_cat_MRIP-output_sim_MRIP_prediction_yr$wf_tot_cat

output_sim_MRIP_prediction_yr$perc_diff_wf_keep = ((output_sim_MRIP_prediction_yr$wf_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep_wf  )/output_sim_MRIP_prediction_yr$wf_keep_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_wf_rel = (( output_sim_MRIP_prediction_yr$wf_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_wf )/output_sim_MRIP_prediction_yr$wf_release_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_wf_tot_cat =  ((output_sim_MRIP_prediction_yr$wf_tot_cat_MRIP-output_sim_MRIP_prediction_yr$wf_tot_cat )/output_sim_MRIP_prediction_yr$wf_tot_cat_MRIP)*100


#rd stats (MRIP minus simulation)
output_sim_MRIP_prediction_yr$diff_rd_keep = output_sim_MRIP_prediction_yr$rd_keep_MRIP -output_sim_MRIP_prediction_yr$tot_keep_rd
output_sim_MRIP_prediction_yr$diff_rd_rel =  output_sim_MRIP_prediction_yr$rd_release_MRIP -output_sim_MRIP_prediction_yr$tot_rel_rd
output_sim_MRIP_prediction_yr$rd_tot_cat = output_sim_MRIP_prediction_yr$tot_keep_rd+output_sim_MRIP_prediction_yr$tot_rel_rd
output_sim_MRIP_prediction_yr$diff_rd_tot_cat = output_sim_MRIP_prediction_yr$rd_tot_cat_MRIP-output_sim_MRIP_prediction_yr$rd_tot_cat

output_sim_MRIP_prediction_yr$perc_diff_rd_keep = ((output_sim_MRIP_prediction_yr$rd_keep_MRIP-output_sim_MRIP_prediction_yr$tot_keep_rd  )/output_sim_MRIP_prediction_yr$rd_keep_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_rd_rel = (( output_sim_MRIP_prediction_yr$rd_release_MRIP-output_sim_MRIP_prediction_yr$tot_rel_rd )/output_sim_MRIP_prediction_yr$rd_release_MRIP)*100
output_sim_MRIP_prediction_yr$perc_diff_rd_tot_cat =  ((output_sim_MRIP_prediction_yr$rd_tot_cat_MRIP-output_sim_MRIP_prediction_yr$rd_tot_cat )/output_sim_MRIP_prediction_yr$rd_tot_cat_MRIP)*100


#Create tables
#SF harvest
sf_harvest_prediction=subset(output_sim_MRIP_prediction_yr, select=c(state, sf_keep_MRIP, tot_keep, diff_sf_keep, perc_diff_sf_keep))
sf_harvest_prediction_total_sim=sum(sf_harvest_prediction$tot_keep)
sf_harvest_prediction_total_MRIP=sum(sf_harvest_prediction$sf_keep_MRIP)
state="All"
sf_harvest_prediction_totals=data.frame(state, sf_harvest_prediction_total_sim,sf_harvest_prediction_total_MRIP)
names(sf_harvest_prediction_totals)<-c("state","tot_keep", "sf_keep_MRIP")
sf_harvest_prediction_totals$diff_sf_keep=sf_harvest_prediction_totals$sf_keep_MRIP - sf_harvest_prediction_totals$tot_keep
sf_harvest_prediction_totals$perc_diff_sf_keep = ((sf_harvest_prediction_totals$sf_keep_MRIP-sf_harvest_prediction_totals$tot_keep  )/sf_harvest_prediction_totals$sf_keep_MRIP)*100


sf_harvest_prediction=rbind(sf_harvest_prediction,sf_harvest_prediction_totals)
sf_harvest_prediction <- data.frame(lapply(sf_harvest_prediction,    # Using Base R functions
                                           function(x) if(is.numeric(x)) round(x, 1) else x))

names(sf_harvest_prediction)<-c("state","SF harvest: MRIP", "SF harvest: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
sf_harvest_prediction =  merge(sf_harvest_prediction,state_names,by="state", all.x=TRUE, all.y=TRUE)
sf_harvest_prediction = sf_harvest_prediction[order(sf_harvest_prediction$state1),]
sf_harvest_prediction = subset(sf_harvest_prediction, select=-c(state1))
write.table(sf_harvest_prediction, file = "sf_harvest_prediction.txt", sep = ",", quote = FALSE, row.names = F)

#SF discards
sf_releases_prediction=subset(output_sim_MRIP_prediction_yr, select=c(state, sf_release_MRIP, tot_rel, diff_sf_rel, perc_diff_sf_rel))
sf_releases_prediction_total_sim=sum(sf_releases_prediction$tot_rel)
sf_releases_prediction_total_MRIP=sum(sf_releases_prediction$sf_release_MRIP)
state="All"
sf_releases_prediction_totals=data.frame(state, sf_releases_prediction_total_sim,sf_releases_prediction_total_MRIP)
names(sf_releases_prediction_totals)<-c("state","tot_rel", "sf_release_MRIP")
sf_releases_prediction_totals$diff_sf_rel=sf_releases_prediction_totals$sf_release_MRIP -sf_releases_prediction_totals$tot_rel
sf_releases_prediction_totals$perc_diff_sf_rel = ((sf_releases_prediction_totals$sf_release_MRIP-sf_releases_prediction_totals$tot_rel )/sf_releases_prediction_totals$sf_release_MRIP)*100
sf_releases_prediction=rbind(sf_releases_prediction,sf_releases_prediction_totals)
sf_releases_prediction <- data.frame(lapply(sf_releases_prediction,    # Using Base R functions
                                            function(x) if(is.numeric(x)) round(x, 1) else x))

names(sf_releases_prediction)<-c("state","SF releases: MRIP", "SF releases: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
sf_releases_prediction =  merge(sf_releases_prediction,state_names,by="state", all.x=TRUE, all.y=TRUE)
sf_releases_prediction = sf_releases_prediction[order(sf_releases_prediction$state1),]
sf_releases_prediction = subset(sf_releases_prediction, select=-c(state1))
write.table(sf_releases_prediction, file = "sf_releases_prediction.txt", sep = ",", quote = FALSE, row.names = F)


#SF total catch
sf_tot_cat_prediction=subset(output_sim_MRIP_prediction_yr, select=c(state, sf_tot_cat_MRIP, sf_tot_cat, diff_sf_tot_cat, perc_diff_sf_tot_cat))
sf_tot_cat_prediction_total_sim=sum(sf_tot_cat_prediction$sf_tot_cat)
sf_tot_cat_prediction_total_MRIP=sum(sf_tot_cat_prediction$sf_tot_cat_MRIP)
state="All"
sf_tot_cat_prediction_totals=data.frame(state, sf_tot_cat_prediction_total_sim,sf_tot_cat_prediction_total_MRIP)
names(sf_tot_cat_prediction_totals)<-c("state","sf_tot_cat", "sf_tot_cat_MRIP")
sf_tot_cat_prediction_totals$diff_sf_tot_cat=sf_tot_cat_prediction_totals$sf_tot_cat_MRIP - sf_tot_cat_prediction_totals$sf_tot_cat
sf_tot_cat_prediction_totals$perc_diff_sf_tot_cat = (( sf_tot_cat_prediction_totals$sf_tot_cat_MRIP-sf_tot_cat_prediction_totals$sf_tot_cat )/sf_tot_cat_prediction_totals$sf_tot_cat_MRIP)*100
sf_tot_cat_prediction=rbind(sf_tot_cat_prediction,sf_tot_cat_prediction_totals)
sf_tot_cat_prediction <- data.frame(lapply(sf_tot_cat_prediction,    # Using Base R functions
                                           function(x) if(is.numeric(x)) round(x, 1) else x))

names(sf_tot_cat_prediction)<-c("state","SF total catch: MRIP", "SF total catch: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
sf_tot_cat_prediction =  merge(sf_tot_cat_prediction,state_names,by="state", all.x=TRUE, all.y=TRUE)
sf_tot_cat_prediction = sf_tot_cat_prediction[order(sf_tot_cat_prediction$state1),]
sf_tot_cat_prediction = subset(sf_tot_cat_prediction, select=-c(state1))
write.table(sf_tot_cat_prediction, file = "sf_tot_cat_prediction.txt", sep = ",", quote = FALSE, row.names = F)


#bsb harvest
bsb_harvest_prediction=subset(output_sim_MRIP_prediction_yr, select=c(state, bsb_keep_MRIP, tot_keep_bsb, diff_bsb_keep, perc_diff_bsb_keep))
bsb_harvest_prediction_total_sim=sum(bsb_harvest_prediction$tot_keep_bsb)
bsb_harvest_prediction_total_MRIP=sum(bsb_harvest_prediction$bsb_keep_MRIP)
state="All"
bsb_harvest_prediction_totals=data.frame(state, bsb_harvest_prediction_total_sim,bsb_harvest_prediction_total_MRIP)
names(bsb_harvest_prediction_totals)<-c("state","tot_keep_bsb", "bsb_keep_MRIP")
bsb_harvest_prediction_totals$diff_bsb_keep=bsb_harvest_prediction_totals$bsb_keep_MRIP-bsb_harvest_prediction_totals$tot_keep_bsb
bsb_harvest_prediction_totals$perc_diff_bsb_keep = ((bsb_harvest_prediction_totals$bsb_keep_MRIP-bsb_harvest_prediction_totals$tot_keep_bsb  )/bsb_harvest_prediction_totals$bsb_keep_MRIP)*100
bsb_harvest_prediction=rbind(bsb_harvest_prediction,bsb_harvest_prediction_totals)
bsb_harvest_prediction <- data.frame(lapply(bsb_harvest_prediction,    # Using Base R functions
                                            function(x) if(is.numeric(x)) round(x, 1) else x))

names(bsb_harvest_prediction)<-c("state","BSB harvest: MRIP", "BSB harvest: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
bsb_harvest_prediction =  merge(bsb_harvest_prediction,state_names,by="state", all.x=TRUE, all.y=TRUE)
bsb_harvest_prediction = bsb_harvest_prediction[order(bsb_harvest_prediction$state1),]
bsb_harvest_prediction = subset(bsb_harvest_prediction, select=-c(state1))
write.table(bsb_harvest_prediction, file = "bsb_harvest_prediction.txt", sep = ",", quote = FALSE, row.names = F)


#BSB discards
bsb_releases_prediction=subset(output_sim_MRIP_prediction_yr, select=c(state, bsb_release_MRIP, tot_rel_bsb, diff_bsb_rel, perc_diff_bsb_rel))
bsb_releases_prediction_total_sim=sum(bsb_releases_prediction$tot_rel_bsb)
bsb_releases_prediction_total_MRIP=sum(bsb_releases_prediction$bsb_release_MRIP)
state="All"
bsb_releases_prediction_totals=data.frame(state, bsb_releases_prediction_total_sim,bsb_releases_prediction_total_MRIP)
names(bsb_releases_prediction_totals)<-c("state","tot_rel_bsb", "bsb_release_MRIP")
bsb_releases_prediction_totals$diff_bsb_rel=bsb_releases_prediction_totals$bsb_release_MRIP-bsb_releases_prediction_totals$tot_rel_bsb
bsb_releases_prediction_totals$perc_diff_bsb_rel = ((bsb_releases_prediction_totals$bsb_release_MRIP-bsb_releases_prediction_totals$tot_rel_bsb  )/bsb_releases_prediction_totals$bsb_release_MRIP)*100
bsb_releases_prediction=rbind(bsb_releases_prediction,bsb_releases_prediction_totals)
bsb_releases_prediction <- data.frame(lapply(bsb_releases_prediction,    # Using Base R functions
                                             function(x) if(is.numeric(x)) round(x, 1) else x))

names(bsb_releases_prediction)<-c("state","BSB releases: MRIP", "BSB releases: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
bsb_releases_prediction =  merge(bsb_releases_prediction,state_names,by="state", all.x=TRUE, all.y=TRUE)
bsb_releases_prediction = bsb_releases_prediction[order(bsb_releases_prediction$state1),]
bsb_releases_prediction = subset(bsb_releases_prediction, select=-c(state1))
write.table(bsb_releases_prediction, file = "bsb_releases_prediction.txt", sep = ",", quote = FALSE, row.names = F)


#BSB total catch
bsb_tot_cat_prediction=subset(output_sim_MRIP_prediction_yr, select=c(state, bsb_tot_cat_MRIP, bsb_tot_cat, diff_bsb_tot_cat, perc_diff_bsb_tot_cat))
bsb_tot_cat_prediction_total_sim=sum(bsb_tot_cat_prediction$bsb_tot_cat)
bsb_tot_cat_prediction_total_MRIP=sum(bsb_tot_cat_prediction$bsb_tot_cat_MRIP)
state="All"
bsb_tot_cat_prediction_totals=data.frame(state, bsb_tot_cat_prediction_total_sim,bsb_tot_cat_prediction_total_MRIP)
names(bsb_tot_cat_prediction_totals)<-c("state","bsb_tot_cat", "bsb_tot_cat_MRIP")
bsb_tot_cat_prediction_totals$diff_bsb_tot_cat=bsb_tot_cat_prediction_totals$bsb_tot_cat_MRIP-bsb_tot_cat_prediction_totals$bsb_tot_cat
bsb_tot_cat_prediction_totals$perc_diff_bsb_tot_cat = ((bsb_tot_cat_prediction_totals$bsb_tot_cat_MRIP-bsb_tot_cat_prediction_totals$bsb_tot_cat  )/bsb_tot_cat_prediction_totals$bsb_tot_cat_MRIP)*100
bsb_tot_cat_prediction=rbind(bsb_tot_cat_prediction,bsb_tot_cat_prediction_totals)
bsb_tot_cat_prediction <- data.frame(lapply(bsb_tot_cat_prediction,    # Using Base R functions
                                            function(x) if(is.numeric(x)) round(x, 1) else x))

names(bsb_tot_cat_prediction)<-c("state","BSB total catch: MRIP", "BSB total catch: model", "Difference (MRIP-model)", "% Difference ((MRIP-model)/MRIP)*100")
bsb_tot_cat_prediction =  merge(bsb_tot_cat_prediction,state_names,by="state", all.x=TRUE, all.y=TRUE)
bsb_tot_cat_prediction = bsb_tot_cat_prediction[order(bsb_tot_cat_prediction$state1),]
bsb_tot_cat_prediction = subset(bsb_tot_cat_prediction, select=-c(state1))
write.table(bsb_tot_cat_prediction, file = "bsb_tot_cat_prediction.txt", sep = ",", quote = FALSE, row.names = F)
###########





# #Calculate changes between calibration year and prediction year, merge these to observed changes
# MRIP_changes_data = data.frame(read_excel("catch_trips_prediction_years_wide.xlsx"))                                                                            
# MRIP_changes_data_18 =subset(MRIP_changes_data, select=c(state, diff_dtrip_MRIP_19_18, diff_sf_tot_cat_MRIP_19_18, diff_sf_keep_MRIP_19_18, diff_sf_release_MRIP_19_18, 
#                                                          diff_bsb_tot_cat_MRIP_19_18, diff_bsb_keep_MRIP_19_18, diff_bsb_release_MRIP_19_18))       
# 
# # Now calulate chnages in trips and catch from the simulation model 
# state_prediction_output1=subset(state_prediction_output1, select=c(state, state1, tot_keep, tot_rel, tot_keep_bsb, tot_rel_bsb, observed_trips, n_choice_occasions))
# state_prediction_output1$tot_cat_sf = state_prediction_output1$tot_keep+state_prediction_output1$tot_rel
# state_prediction_output1$tot_cat_bsb = state_prediction_output1$tot_keep_bsb+state_prediction_output1$tot_rel_bsb
# names(state_prediction_output1)[names(state_prediction_output1) == "tot_keep"] = "tot_keep_sf_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "tot_rel"] = "tot_rel_sf_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "tot_cat_sf"] = "tot_cat_sf_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "tot_keep_bsb"] = "tot_keep_bsb_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "tot_rel_bsb"] = "tot_rel_bsb_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "tot_cat_bsb"] = "tot_cat_bsb_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "observed_trips"] = "observed_trips_18"
# names(state_prediction_output1)[names(state_prediction_output1) == "n_choice_occasions"] = "n_choice_occasions_18"
# 
# 
# state_calibration_output1=subset(state_calibration_output1, select=c(state, state1, tot_keep, tot_rel, tot_keep_bsb, tot_rel_bsb, observed_trips, n_choice_occasions))
# state_calibration_output1$tot_cat_sf = state_calibration_output1$tot_keep+state_calibration_output1$tot_rel
# state_calibration_output1$tot_cat_bsb = state_calibration_output1$tot_keep_bsb+state_calibration_output1$tot_rel_bsb
# names(state_calibration_output1)[names(state_calibration_output1) == "tot_keep"] = "tot_keep_sf_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "tot_rel"] = "tot_rel_sf_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "tot_cat_sf"] = "tot_cat_sf_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "tot_keep_bsb"] = "tot_keep_bsb_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "tot_rel_bsb"] = "tot_rel_bsb_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "tot_cat_bsb"] = "tot_cat_bsb_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "observed_trips"] = "observed_trips_19"
# names(state_calibration_output1)[names(state_calibration_output1) == "n_choice_occasions"] = "n_choice_occasions_19"
# 
# 
# simulation_changes = merge(state_calibration_output1,state_prediction_output1,by=c("state", "state1"), all.x=TRUE, all.y=TRUE)
# simulation_changes$diff_dtrip_sim_19_18=simulation_changes$observed_trips_19-simulation_changes$observed_trips_18
# simulation_changes$diff_sf_keep_sim_19_18=simulation_changes$tot_keep_sf_19-simulation_changes$tot_keep_sf_18
# simulation_changes$diff_sf_rel_sim_19_18=simulation_changes$tot_rel_sf_19-simulation_changes$tot_rel_sf_18
# simulation_changes$diff_sf_tot_cat_sim_19_18=simulation_changes$tot_cat_sf_19-simulation_changes$tot_cat_sf_18
# simulation_changes$diff_bsb_keep_sim_19_18=simulation_changes$tot_keep_bsb_19-simulation_changes$tot_keep_bsb_18
# simulation_changes$diff_bsb_rel_sim_19_18=simulation_changes$tot_rel_bsb_19-simulation_changes$tot_rel_bsb_18
# simulation_changes$diff_bsb_tot_cat_sim_19_18=simulation_changes$tot_cat_bsb_19-simulation_changes$tot_cat_bsb_18
# simulation_changes=subset(simulation_changes, select=c(state, state1,diff_dtrip_sim_19_18,diff_sf_keep_sim_19_18, diff_sf_rel_sim_19_18,
#                                                        diff_sf_tot_cat_sim_19_18, diff_bsb_keep_sim_19_18,diff_bsb_rel_sim_19_18,diff_bsb_tot_cat_sim_19_18)) 
# 
# MRIP_simulation_changes= merge(simulation_changes,MRIP_changes_data_18,by=c("state"), all.x=TRUE, all.y=TRUE)
# MRIP_simulation_changes = MRIP_simulation_changes[order(MRIP_simulation_changes$state1),]  
# 
# 
# #now make some tables
# #Directed trips
# simulation_MRIP_changes_dtrip=subset(MRIP_simulation_changes, select=c(state, diff_dtrip_sim_19_18, diff_dtrip_MRIP_19_18))
# simulation_MRIP_changes_dtrip_sum=subset(simulation_MRIP_changes_dtrip, select=-c(state))
# simulation_MRIP_changes_dtrip_sum$tab=1
# simulation_MRIP_changes_dtrip_sum=aggregate(simulation_MRIP_changes_dtrip_sum,by=list(simulation_MRIP_changes_dtrip_sum$tab), FUN=sum, na.rm=TRUE)
# simulation_MRIP_changes_dtrip_sum=subset(simulation_MRIP_changes_dtrip_sum, select=c(diff_dtrip_sim_19_18, diff_dtrip_MRIP_19_18))
# simulation_MRIP_changes_dtrip_sum$state="All"
# simulation_MRIP_changes_dtrip=rbind(simulation_MRIP_changes_dtrip,simulation_MRIP_changes_dtrip_sum)
# write.table(simulation_MRIP_changes_dtrip, file = "simulation_MRIP_changes_dtrip.txt", sep = ",", quote = FALSE, row.names = F)
# 
# 
# #SF keep, release, total catch
# simulation_MRIP_changes_sf_catch=subset(MRIP_simulation_changes, select=c(state, diff_sf_keep_sim_19_18, diff_sf_keep_MRIP_19_18, 
#                                                                           diff_sf_rel_sim_19_18, diff_sf_release_MRIP_19_18, 
#                                                                           diff_sf_tot_cat_sim_19_18, diff_sf_tot_cat_MRIP_19_18))
# simulation_MRIP_changes_catch_sum=subset(simulation_MRIP_changes_sf_catch, select=-c(state))
# simulation_MRIP_changes_catch_sum$tab=1
# simulation_MRIP_changes_catch_sum=aggregate(simulation_MRIP_changes_catch_sum,by=list(simulation_MRIP_changes_catch_sum$tab), FUN=sum, na.rm=TRUE)
# simulation_MRIP_changes_catch_sum=subset(simulation_MRIP_changes_catch_sum, select=-c(tab, Group.1))
# simulation_MRIP_changes_catch_sum$state="All"
# simulation_MRIP_changes_sf_catch=rbind(simulation_MRIP_changes_sf_catch,simulation_MRIP_changes_catch_sum)
# write.table(simulation_MRIP_changes_sf_catch, file = "simulation_MRIP_changes_sf_catch.txt", sep = ",", quote = FALSE, row.names = F)
# 
# 
# #BSB keep, release, total catch
# simulation_MRIP_changes_bsb_catch=subset(MRIP_simulation_changes, select=c(state, diff_bsb_keep_sim_19_18, diff_bsb_keep_MRIP_19_18, 
#                                                                            diff_bsb_rel_sim_19_18, diff_bsb_release_MRIP_19_18, 
#                                                                            diff_bsb_tot_cat_sim_19_18, diff_bsb_tot_cat_MRIP_19_18))
# simulation_MRIP_changes_catch_sum=subset(simulation_MRIP_changes_bsb_catch, select=-c(state))
# simulation_MRIP_changes_catch_sum$tab=1
# simulation_MRIP_changes_catch_sum=aggregate(simulation_MRIP_changes_catch_sum,by=list(simulation_MRIP_changes_catch_sum$tab), FUN=sum, na.rm=TRUE)
# simulation_MRIP_changes_catch_sum=subset(simulation_MRIP_changes_catch_sum, select=-c(tab, Group.1))
# simulation_MRIP_changes_catch_sum$state="All"
# simulation_MRIP_changes_bsb_catch=rbind(simulation_MRIP_changes_bsb_catch,simulation_MRIP_changes_catch_sum)
# write.table(simulation_MRIP_changes_bsb_catch, file = "simulation_MRIP_changes_bsb_catch.txt", sep = ",", quote = FALSE, row.names = F)

