

# This code uses  recreational selectivity (q) in the baseline year to determine catch-at-length given any size structure of the stock

# Code requires the following data: 
# AgE length key (for now using 2018 commercial survey ALK ): com_sv_len_age_adj_2018.xlsx
# Numbers at age file, in 1000s of fish: numbers_at_age_2018.csv <-- This will be the output from the operating model
# Baseline recreational selectivity: rec_selectivity.xlsx





# # Import the 2018 ALK (in centimeters) provided by M. Terceiro
# age_length_key = data.frame(read_excel("com_sv_len_age_adj_2018.xlsx"))                                                                            
# 
# 
# #import a test numbers-at-age dataset
# # THIS IS WHERE TO IMPORT THE NUMBERS AT AGE FROM THE OPERATING MODEL
# numbers_at_age = data.frame(read_excel("numbers_at_age_2018.xlsx"))
# 
# 
# # Merge the two above datasets and create population numbers-at-length (inches)
# numbers_at_length =  merge(age_length_key,numbers_at_age,by="age", all.x=TRUE, all.y=TRUE)
# numbers_at_length$N_l = numbers_at_length$proportion*numbers_at_length$Na
# 
# numbers_at_length = aggregate(numbers_at_length, by=list(numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
# numbers_at_length <-subset(numbers_at_length, select=c(Group.1,N_l))
# names(numbers_at_length)[names(numbers_at_length) == "Group.1"] = "l_in_bin"
# 
# 
# # Translate cms's to inches 
# numbers_at_length$l_in_bin = round(numbers_at_length$l_in_bin/2.54)
# numbers_at_length = aggregate(numbers_at_length, by=list(numbers_at_length$l_in_bin),FUN=sum, na.rm=TRUE)
# numbers_at_length <-subset(numbers_at_length, select=c(Group.1,N_l))
# names(numbers_at_length)[names(numbers_at_length) == "Group.1"] = "l_in_bin"

numbers_at_length <- tibble(l_in_bin = lenbinuse,
                            N_l = om_length_in[1,])
#Translate numbers from 1,000's of fish
numbers_at_length$N_l=numbers_at_length$N_l*1000
sum(numbers_at_length$N_l)


# Import and merge the selectivity data to this file 
#selectivity = data.frame(read_excel("rec_selectivity.xlsx"))
selectivity <- readRDS("rec_selectivity.rds")
selectivity <-subset(selectivity, select=c(l_in_bin, region, q, E, C_l))
numbers_at_length_new =  merge(selectivity,numbers_at_length,by="l_in_bin", all.x=TRUE, all.y=TRUE)

numbers_at_length_new[is.na(numbers_at_length_new)] = 0
numbers_at_length_new <-subset(numbers_at_length_new, N_l!=0 & region!=0)


# Create catch-at-length based on the new numbers-at-length
numbers_at_length_new$q = as.numeric(numbers_at_length_new$q)
numbers_at_length_new$C_l_new = (numbers_at_length_new$q)*(numbers_at_length_new$N_l)*(numbers_at_length_new$E)
sum(numbers_at_length_new$C_l_new)
#write_xlsx(numbers_at_length_new,"numbers_at_length_new.xlsx")

# subset the catch-at-length new datstet by region and fit gamma to the distirbution 
numbers_at_length_NO <-subset(numbers_at_length_new, region=="NO", select=c(l_in_bin, C_l_new)) %>% filter(C_l_new >0)
numbers_at_length_NJ <-subset(numbers_at_length_new, region=="NJ", select=c(l_in_bin, C_l_new)) %>% filter(C_l_new >0)
numbers_at_length_SO <-subset(numbers_at_length_new, region=="SO", select=c(l_in_bin, C_l_new)) %>% filter(C_l_new >0)

 
numbers_at_length_NO$C_l_new=round(numbers_at_length_NO$C_l_new)
numbers_at_length_NJ$C_l_new=round(numbers_at_length_NJ$C_l_new)
numbers_at_length_SO$C_l_new=round(numbers_at_length_SO$C_l_new)



tot_cat_NO_predicted=sum(numbers_at_length_NO$C_l_new)
tot_cat_NJ_predicted=sum(numbers_at_length_NJ$C_l_new)
tot_cat_SO_predicted=sum(numbers_at_length_SO$C_l_new)


tot_cat_NO_base=sum(subset(selectivity, region == "NO")$C_l)
tot_cat_NJ_base=sum(subset(selectivity, region == "NJ")$C_l)
tot_cat_SO_base=sum(subset(selectivity, region == "SO")$C_l)

tot_cat_base = tot_cat_NO_base+tot_cat_NJ_base+tot_cat_SO_base
tot_cat_predicted = tot_cat_NO_predicted + tot_cat_NJ_predicted+tot_cat_SO_predicted
tot_cat_predicted/tot_cat_base

#Create a factor that expands total catch in the prediction year
catch_expansion_factor_NO=tot_cat_NO_predicted/tot_cat_NO_base
catch_expansion_factor_NJ=tot_cat_NJ_predicted/tot_cat_NJ_base
catch_expansion_factor_SO=tot_cat_SO_predicted/tot_cat_SO_base



##########
# Here, execute the catch-per trip file. 
# This file adjusts the expected number of catch per trip by population abundances. 
source("predicted_catch_per_trip.R")


#####
numbers_at_length_NO <- numbers_at_length_NO %>% 
  rename(fitted_length = l_in_bin,
         nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         region = rep("NO",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

numbers_at_length_NJ <- numbers_at_length_NJ %>% 
  rename(fitted_length = l_in_bin,
         nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         region = rep("NJ",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

numbers_at_length_SO <- numbers_at_length_SO %>% 
  rename(fitted_length = l_in_bin,
         nfish = C_l_new) %>% 
  mutate(fitted_prob = nfish/sum(.$nfish),
         region = rep("SO",nrow(.)),
         year = rep("y2", nrow(.))) %>% 
  I()

# 
# #expand the sf_catch_data so that each row represents a fish
# # north region
# row_inds = seq_len(nrow(numbers_at_length_NO))
# numbers_at_length_NO = numbers_at_length_NO[c(rep(row_inds, numbers_at_length_NO$C_l_new)), ]
# rownames(numbers_at_length_NO) = NULL
# 
# 
# fit = fitdistr(numbers_at_length_NO$l_in_bin, 'gamma')
# n = nrow(numbers_at_length_NO)
# shape=coef(fit)[1]
# rate=coef(fit)[2]
# 
# numbers_at_length_NO$gammafit = rgamma(n, shape=shape,  rate=rate)
# numbers_at_length_NO$gammafit = round(numbers_at_length_NO$gammafit)
# numbers_at_length_NO$nfish = 1
# numbers_at_length_NO <-aggregate(numbers_at_length_NO, by=list(numbers_at_length_NO$gammafit),FUN=sum, na.rm=TRUE)
# numbers_at_length_NO$region="NO"
# 
# names(numbers_at_length_NO)[names(numbers_at_length_NO) == "Group.1"] = "fitted_length"
# numbers_at_length_NO <-subset(numbers_at_length_NO,  select=c(fitted_length, nfish))
# sum_nfish= sum(numbers_at_length_NO$nfish)
# numbers_at_length_NO$fitted_prob = numbers_at_length_NO$nfish/sum_nfish
# numbers_at_length_NO$region = "NO"
# numbers_at_length_NO$year = "y2"
# 
# 
# 
# # New York
# # row_inds = seq_len(nrow(numbers_at_length_NY))
# # numbers_at_length_NY = numbers_at_length_NY[c(rep(row_inds, numbers_at_length_NY$C_l_new)), ]
# # rownames(numbers_at_length_NO) = NULL
# # 
# # 
# # fit = fitdistr(numbers_at_length_NY$l_in_bin, 'gamma')
# # n = nrow(numbers_at_length_NY)
# # shape=coef(fit)[1]
# # rate=coef(fit)[2]
# # 
# # numbers_at_length_NY$gammafit = rgamma(n, shape=shape,  rate=rate)
# # numbers_at_length_NY$gammafit = round(numbers_at_length_NY$gammafit)
# # numbers_at_length_NY$nfish = 1
# # numbers_at_length_NY <-aggregate(numbers_at_length_NY, by=list(numbers_at_length_NY$gammafit),FUN=sum, na.rm=TRUE)
# # numbers_at_length_NY$region="NO"
# # 
# # names(numbers_at_length_NY)[names(numbers_at_length_NY) == "Group.1"] = "fitted_length"
# # numbers_at_length_NY <-subset(numbers_at_length_NY,  select=c(fitted_length, nfish))
# # sum_nfish= sum(numbers_at_length_NY$nfish)
# # numbers_at_length_NY$fitted_prob = numbers_at_length_NY$nfish/sum_nfish
# # numbers_at_length_NY$region = "NY"
# # numbers_at_length_NY$year = "y2"
# 
# 
# #New Jersey
# row_inds = seq_len(nrow(numbers_at_length_NJ))
# numbers_at_length_NJ = numbers_at_length_NJ[c(rep(row_inds, numbers_at_length_NJ$C_l_new)), ]
# rownames(numbers_at_length_NJ) = NULL
# 
# 
# fit = fitdistr(numbers_at_length_NJ$l_in_bin, 'gamma')
# n = nrow(numbers_at_length_NJ)
# shape=coef(fit)[1]
# rate=coef(fit)[2]
# 
# numbers_at_length_NJ$gammafit = rgamma(n, shape=shape,  rate=rate)
# numbers_at_length_NJ$gammafit = round(numbers_at_length_NJ$gammafit)
# numbers_at_length_NJ$nfish = 1
# numbers_at_length_NJ <-aggregate(numbers_at_length_NJ, by=list(numbers_at_length_NJ$gammafit),FUN=sum, na.rm=TRUE)
# numbers_at_length_NJ$region="NJ"
# 
# names(numbers_at_length_NJ)[names(numbers_at_length_NJ) == "Group.1"] = "fitted_length"
# numbers_at_length_NJ <-subset(numbers_at_length_NJ,  select=c(fitted_length, nfish))
# sum_nfish= sum(numbers_at_length_NJ$nfish)
# numbers_at_length_NJ$fitted_prob = numbers_at_length_NJ$nfish/sum_nfish
# numbers_at_length_NJ$region = "NJ"
# numbers_at_length_NJ$year = "y2"
# 
# 
# 
# #Southern region
# row_inds = seq_len(nrow(numbers_at_length_SO))
# numbers_at_length_SO = numbers_at_length_SO[c(rep(row_inds, numbers_at_length_SO$C_l_new)), ]
# rownames(numbers_at_length_SO) = NULL
# 
# 
# fit = fitdistr(numbers_at_length_SO$l_in_bin, 'gamma')
# n = nrow(numbers_at_length_SO)
# shape=coef(fit)[1]
# rate=coef(fit)[2]
# 
# numbers_at_length_SO$gammafit = rgamma(n, shape=shape,  rate=rate)
# numbers_at_length_SO$gammafit = round(numbers_at_length_SO$gammafit)
# numbers_at_length_SO$nfish = 1
# numbers_at_length_SO <-aggregate(numbers_at_length_SO, by=list(numbers_at_length_SO$gammafit),FUN=sum, na.rm=TRUE)
# numbers_at_length_SO$region="SO"
# 
# 
# names(numbers_at_length_SO)[names(numbers_at_length_SO) == "Group.1"] = "fitted_length"
# numbers_at_length_SO <-subset(numbers_at_length_SO,  select=c(fitted_length, nfish))
# sum_nfish= sum(numbers_at_length_SO$nfish)
# numbers_at_length_SO$fitted_prob = numbers_at_length_SO$nfish/sum_nfish
# numbers_at_length_SO$region = "SO"
# numbers_at_length_SO$year = "y2"


#combine the datasets
fitted_sizes_region_all_y2 = bind_rows(numbers_at_length_SO, numbers_at_length_NJ, numbers_at_length_NO)
fitted_sizes_region_all_y2 = subset(fitted_sizes_region_all_y2, select=c(fitted_prob, fitted_length, region, year))


# This file contains the new catch-at-length distribution for the prediction year
#write_xlsx(fitted_sizes_region_all_y2,"sf_fitted_sizes_y2plus.xlsx")
saveRDS(fitted_sizes_region_all_y2,file = "sf_fitted_sizes_y2plus.rds")

