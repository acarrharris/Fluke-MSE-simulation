#The following creates an catch-per-trip dataset adjusted to reflect the population size

###Northern states
#catch_data <- read_excel("observed_catch_NO_19.xlsx")
catch_data <- readRDS("observed_catch_NO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
# 
# sf_mu <- nbfit_sf$estimate['mu']
# sf_mu
# 
# sf_size <- nbfit_sf$estimate['size']
# sf_size


# Read in the nb parameters for sf
nbfit_sf <- readRDS("nb_catch_parameters_sf_NO_19.rds")
sf_mu <- nbfit_sf$estimate['mu']
sf_size <- nbfit_sf$estimate['size']

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
sf_mu_new=sf_mu*catch_expansion_factor_NO

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
# 
# bsb_mu <- nbfit_bsb$estimate['mu']
# bsb_mu
# bsb_size <- nbfit_bsb$estimate['size']
# bsb_size
# bsb_size

# Read in the nb parameters for bsb
nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NO_19.rds")
bsb_mu <- nbfit_bsb$estimate['mu']
bsb_size <- nbfit_bsb$estimate['size']


# #t copula
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

region="NO"
catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_NO.xlsx")
saveRDS(catch_data_sim, "predicted_catch_NO.rdss")


###New Jersey
#catch_data <- read_excel("observed_catch_NJ_19.xlsx")
catch_data <- readRDS("observed_catch_NJ_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
# 
# sf_mu <- nbfit_sf$estimate['mu']
# sf_mu
# 
# sf_size <- nbfit_sf$estimate['size']
# sf_size

# Read in the nb parameters for sf
nbfit_sf <- readRDS("nb_catch_parameters_sf_NJ_19.rds")
sf_mu <- nbfit_sf$estimate['mu']
sf_size <- nbfit_sf$estimate['size']

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
sf_mu_new=sf_mu*catch_expansion_factor_NJ

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
# 
# bsb_mu <- nbfit_bsb$estimate['mu']
# bsb_mu
# bsb_size <- nbfit_bsb$estimate['size']
# bsb_size
# bsb_size

# Read in the nb parameters for bsb
nbfit_bsb <- readRDS("nb_catch_parameters_bsb_NJ_19.rds")
bsb_mu <- nbfit_bsb$estimate['mu']
bsb_size <- nbfit_bsb$estimate['size']


# #t copula
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
catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_NJ.xlsx") 
saveRDS(catch_data_sim, "predicted_catch_NJ.rds")


###Southern states
#catch_data <- read_excel("observed_catch_SO_19.xlsx")
catch_data <- readRDS("observed_catch_SO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
# nbfit_sf = fitdistr(sf, "Negative Binomial")
# summary(nbfit_sf)
# 
# sf_mu <- nbfit_sf$estimate['mu']
# sf_mu
# 
# sf_size <- nbfit_sf$estimate['size']
# sf_size

# Read in the nb parameters for sf
nbfit_sf <- readRDS("nb_catch_parameters_sf_SO_19.rds")
sf_mu <- nbfit_sf$estimate['mu']
sf_size <- nbfit_sf$estimate['size']


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
sf_mu_new=sf_mu*catch_expansion_factor_SO

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
# 
# bsb_mu <- nbfit_bsb$estimate['mu']
# bsb_mu
# bsb_size <- nbfit_bsb$estimate['size']
# bsb_size
# bsb_size

# Read in the nb parameters for bsb
nbfit_bsb <- readRDS("nb_catch_parameters_bsb_SO_19.rds")
bsb_mu <- nbfit_bsb$estimate['mu']
bsb_size <- nbfit_bsb$estimate['size']


# #t copula
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

region="SO"
catch_data_sim=data.frame(sf_t_nb, bsb_t_nb, region)
#write_xlsx(catch_data_sim, "predicted_catch_SO.xlsx")
saveRDS(catch_data_sim, "predicted_catch_SO.rds")
