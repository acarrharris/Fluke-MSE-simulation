#The following creates an catch-per-trip dataset adjusted to reflect the population size

###Massachusetts
# catch_data <- read_excel("observed_catch_NO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_MA*prop_rel[1]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb

nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

fit <- readRDS("catch_copula_NO_19.rds")

# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))


sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="MA"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_ma <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_MA.xlsx") 
#saveRDS(catch_data_sim, "predicted_catch_MA.rds")





###Rhode Island
# catch_data <- read_excel("observed_catch_NO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
#nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_RI*prop_rel[1]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb

#nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

#fit <- readRDS("catch_copula_NO_19.rds")

# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))


sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="RI"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_ri <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_RI.xlsx") 
#saveRDS(catch_data_sim, "predicted_catch_RI.rds")




###Connecticut
# catch_data <- read_excel("observed_catch_NO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
#nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_CT*prop_rel[1]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb

#nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

#fit <- readRDS("catch_copula_NO_19.rds")

# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))


sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="CT"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_ct <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_CT.xlsx") 
#saveRDS(catch_data_sim, "predicted_catch_CT.rds")



###NEw York
# catch_data <- read_excel("observed_catch_NO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
#nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_NY*prop_rel[1]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb

#nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

#fit <- readRDS("catch_copula_NO_19.rds")

# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))


sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="NY"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_ny <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_NY.xlsx") 
#saveRDS(catch_data_sim, "predicted_catch_NY.rds")




###New Jersey
# catch_data <- read_excel("observed_catch_NJ_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)

nbfit_sf <- readRDS("nb_catch_parameters_sf_NJ_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_NJ*prop_rel[2]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb

nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NJ_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

fit <- readRDS("catch_copula_NJ_19.rds")

# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="NJ"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_nj <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_NJ.xlsx") 
#saveRDS(catch_data_sim, "predicted_catch_NJ.rds")



###Delaware
# catch_data <- read_excel("observed_catch_SO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)

nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_DE*prop_rel[3]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb
nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

fit <- readRDS("catch_copula_SO_19.rds")


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="DE"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_de <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_DE.xlsx")
#saveRDS(catch_data_sim, "predicted_catch_DE.rds")



###Maryland
# catch_data <- read_excel("observed_catch_SO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)

#nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_MD*prop_rel[3]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb
#nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

#fit <- readRDS("catch_copula_SO_19.rds")


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="MD"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_md <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_MD.xlsx")
#saveRDS(catch_data_sim, "predicted_catch_MD.rds")




###Virginia
# catch_data <- read_excel("observed_catch_SO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)

#nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_VA*prop_rel[3]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb
#nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

#fit <- readRDS("catch_copula_SO_19.rds")


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="VA"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_va <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_VA.xlsx")
#saveRDS(catch_data_sim, "predicted_catch_VA.rds")




###North Carolina
# catch_data <- read_excel("observed_catch_SO_19.xlsx")
# 
# sf <- catch_data$sf_tot_cat
# bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)

#nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size


# We now adjust mean catch per trip by the expansion factor. 
# We will assume that in the prediction year under a new mean catch per trip, 
# the SD relative to the mean remains as it was in the baseline year. 
# We therefore adjust the 'size' parameter for the prediction year distribution such that: coef. var. prediction year = Coef. var. 2019  

# From ?NegBinomial:
# "An alternative parametrization (often used in ecology) is by the mean mu (see above), 
# and size, the dispersion parameter, where prob = size/(size+mu). The variance is mu + mu^2/size 
# in this parametrization." 

#calculate variance and CV in baseline year
var_sf=sf_mu+(sf_mu^2)/sf_size
cv_sf_base = sqrt(var_sf)/sf_mu
cv_sf_base

# Now adjust the mu parameter for prediction year distribution by the expansion factor
sf_mu_new=sf_mu*catch_expansion_factor_NC*prop_rel[3]

#solve for new size parameter 
sf_size_new=(sf_mu_new^2)/(((cv_sf_base*sf_mu_new)^2)-sf_mu_new)

#new variance and CV
var_sf_new=sf_mu_new+(sf_mu_new^2)/sf_size_new
cv_sf_new = sqrt(var_sf_new)/sf_mu_new

#Check that CV old and CV new are the same
cv_sf_base
cv_sf_new


# Get the black sea bass parameters 
# nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
# nbfit_bsb
#nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
# t_cop_model <- tCopula(dim = 2)
# m <- pobs(as.matrix(cbind(sf,bsb)))
# fit <- fitCopula(t_cop_model, m, method = 'ml')
# fit
# coef(fit)

#fit <- readRDS("catch_copula_SO_19.rds")


# Set the parameters
rho <- coef(fit)[1]
df <- coef(fit)[2]


t_copula_nb <- mvdc(copula=tCopula(rho,dim=2,df=df),  margins = c("nbinom","nbinom"), 
                    paramMargins=list(list(mu=sf_mu_new, size=sf_size_new),
                                      list(mu=bsb_mu, size=bsb_size)))

sim_t_cop_nb <- rMvdc(30000, t_copula_nb )

sf_t_nb=sim_t_cop_nb[,1]
bsb_t_nb=sim_t_cop_nb[,2]

mean(sf_t_nb)

region="NC"
#catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
sf_catch_data_nc <- tibble(tot_sf_catch = sf_t_nb, tot_bsb_catch = bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_NC.xlsx")
#saveRDS(catch_data_sim, "predicted_catch_NC.rds")
