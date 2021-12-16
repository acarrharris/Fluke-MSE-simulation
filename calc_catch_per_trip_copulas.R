#The following creates an cathc-per-trip dataset adjusted to reflect the population size

###Northern states
#catch_data <- read_excel("observed_catch_NO_19.xlsx")
catch_data <- readRDS("observed_catch_NO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
nbfit_sf

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
nbfit_bsb

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size

#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_NO_19.rds")

###New Jersey
#catch_data <- read_excel("observed_catch_NJ_19.xlsx")
catch_data <- readRDS("observed_catch_NJ_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
nbfit_sf

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
nbfit_bsb

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_NJ_19.rds")


###Southern states
#catch_data <- read_excel("observed_catch_SO_19.xlsx")
catch_data <- readRDS("observed_catch_SO_19.rds")

sf <- catch_data$sf_tot_cat
bsb <- catch_data$bsb_tot_cat


#estimate the nb parameters
nbfit_sf = fitdistr(sf, "Negative Binomial")
nbfit_sf

sf_mu <- nbfit_sf$estimate['mu']
sf_mu

sf_size <- nbfit_sf$estimate['size']
sf_size

nbfit_bsb <- fitdistr(bsb, "Negative Binomial")
nbfit_bsb

bsb_mu <- nbfit_bsb$estimate['mu']
bsb_mu
bsb_size <- nbfit_bsb$estimate['size']
bsb_size


#t copula
t_cop_model <- tCopula(dim = 2)
m <- pobs(as.matrix(cbind(sf,bsb)))
fit <- fitCopula(t_cop_model, m, method = 'ml')
fit
coef(fit)

saveRDS(fit, "catch_copula_SO_19.rds")
