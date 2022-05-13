
param_draws_MA = as.data.frame(1:30000)
names(param_draws_MA)[names(param_draws_MA) == "1:30000"] = "tripid"
param_draws_MA$beta_sqrt_sf_keep = rnorm(30000, mean = 0.559, sd = 0.678)
param_draws_MA$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.336)
param_draws_MA$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.275, sd = 0.261)
param_draws_MA$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_MA$beta_sqrt_scup_keep = rnorm(30000, mean = 0.075, sd = 0.143)
param_draws_MA$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0)
param_draws_MA$beta_opt_out = rnorm(30000, mean = -2.641, sd = 2.554)
param_draws_MA$beta_striper_blue = rnorm(30000, mean = 1.429, sd = 1.920)
param_draws_MA$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_MA$parameter_draw=1

param_draws_RI = as.data.frame(1:30000)
names(param_draws_RI)[names(param_draws_RI) == "1:30000"] = "tripid"
param_draws_RI$beta_sqrt_sf_keep = rnorm(30000, mean = 0.559, sd = 0.678)
param_draws_RI$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.336)
param_draws_RI$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.275, sd = 0.261)
param_draws_RI$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_RI$beta_sqrt_scup_keep = rnorm(30000, mean = 0.075, sd = 0.143)
param_draws_RI$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0)
param_draws_RI$beta_opt_out = rnorm(30000, mean = -2.641, sd = 2.554)
param_draws_RI$beta_striper_blue = rnorm(30000, mean = 1.429, sd = 1.920)
param_draws_RI$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_RI$parameter_draw=1

param_draws_CT = as.data.frame(1:30000)
names(param_draws_CT)[names(param_draws_CT) == "1:30000"] = "tripid"
param_draws_CT$beta_sqrt_sf_keep = rnorm(30000, mean = 0.559, sd = 0.678)
param_draws_CT$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.336)
param_draws_CT$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.275, sd = 0.261)
param_draws_CT$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_CT$beta_sqrt_scup_keep = rnorm(30000, mean = 0.075, sd = 0.143)
param_draws_CT$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0)
param_draws_CT$beta_opt_out = rnorm(30000, mean = -2.641, sd = 2.554)
param_draws_CT$beta_striper_blue = rnorm(30000, mean = 1.429, sd = 1.920)
param_draws_CT$beta_cost = rnorm(30000, mean = -0.012, sd = 0)

param_draws_CT$parameter_draw=1

param_draws_NY = as.data.frame(1:30000)
names(param_draws_NY)[names(param_draws_NY) == "1:30000"] = "tripid"

param_draws_NY$beta_sqrt_sf_keep = rnorm(30000, mean = 0.559, sd = 0.678)
param_draws_NY$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.336)
param_draws_NY$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.275, sd = 0.261)
param_draws_NY$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_NY$beta_sqrt_scup_keep = rnorm(30000, mean = 0.075, sd = 0.143)
param_draws_NY$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0)
param_draws_NY$beta_opt_out = rnorm(30000, mean = -2.641, sd = 2.554)
param_draws_NY$beta_striper_blue = rnorm(30000, mean = 1.429, sd = 1.920)
param_draws_NY$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_NY$parameter_draw=1

param_draws_NJ = as.data.frame(1:30000)
names(param_draws_NJ)[names(param_draws_NJ) == "1:30000"] = "tripid"

param_draws_NJ$beta_sqrt_sf_keep = rnorm(30000, mean = 0.762, sd = 0.677)
param_draws_NJ$beta_sqrt_sf_release = rnorm(30000, mean = 0.0, sd = 0.181)
param_draws_NJ$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.174, sd = 0.334)
param_draws_NJ$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_NJ$beta_sqrt_scup_keep = rnorm(30000, mean = 0.097, sd =  0.113)
param_draws_NJ$beta_sqrt_scup_release = rnorm(30000, mean = -0.039, sd = 0.117)
param_draws_NJ$beta_sqrt_wf_keep = rnorm(30000, mean = 0.394, sd =  0.199)
param_draws_NJ$beta_sqrt_wf_release = rnorm(30000, mean = 0.093, sd = 0.278)
param_draws_NJ$beta_opt_out = rnorm(30000, mean = -2.095, sd = 2.394)
param_draws_NJ$beta_striper_blue = rnorm(30000, mean = 1.139, sd = 1.832)
param_draws_NJ$beta_cost = rnorm(30000, mean = -0.009, sd = 0)
param_draws_NJ$parameter_draw=1

param_draws_MD = as.data.frame(1:30000)
names(param_draws_MD)[names(param_draws_MD) == "1:30000"] = "tripid"

param_draws_MD$beta_sqrt_sf_keep = rnorm(30000, mean = 0.807, sd = 0.599)
param_draws_MD$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.317)
param_draws_MD$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.239, sd = 0.287)
param_draws_MD$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0.160)
param_draws_MD$beta_sqrt_wf_keep = rnorm(30000, mean = 0.379, sd =  0.381)
param_draws_MD$beta_sqrt_wf_release = rnorm(30000, mean = 0.064, sd = 0.227)
param_draws_MD$beta_opt_out = rnorm(30000, mean = -2.963, sd = 2.448)
param_draws_MD$beta_striper_blue = rnorm(30000, mean = 0.645, sd = 1.900)
param_draws_MD$beta_cost = rnorm(30000, mean = -0.009, sd = 0)
param_draws_MD$parameter_draw=1

param_draws_DE = as.data.frame(1:30000)
names(param_draws_DE)[names(param_draws_DE) == "1:30000"] = "tripid"

param_draws_DE$beta_sqrt_sf_keep = rnorm(30000, mean = 0.807, sd = 0.599)
param_draws_DE$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.317)
param_draws_DE$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.239, sd = 0.287)
param_draws_DE$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0.160)
param_draws_DE$beta_sqrt_wf_keep = rnorm(30000, mean = 0.379, sd =  0.381)
param_draws_DE$beta_sqrt_wf_release = rnorm(30000, mean = 0.064, sd = 0.227)
param_draws_DE$beta_opt_out = rnorm(30000, mean = -2.963, sd = 2.448)
param_draws_DE$beta_striper_blue = rnorm(30000, mean = 0.645, sd = 1.900)
param_draws_DE$beta_cost = rnorm(30000, mean = -0.009, sd = 0)
param_draws_DE$parameter_draw=1

param_draws_VA = as.data.frame(1:30000)
names(param_draws_VA)[names(param_draws_VA) == "1:30000"] = "tripid"

param_draws_VA$beta_sqrt_sf_keep = rnorm(30000, mean = 0.521, sd = 0.464)
param_draws_VA$beta_sqrt_sf_release = rnorm(30000, mean = 0.108, sd = 0.221)
param_draws_VA$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.192, sd = 0.200)
param_draws_VA$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0.131)
param_draws_VA$beta_sqrt_wf_keep = rnorm(30000, mean = 0.231, sd =  0.393)
param_draws_VA$beta_sqrt_wf_release = rnorm(30000, mean = 0, sd = 0.146)
param_draws_VA$beta_sqrt_rd_keep = rnorm(30000, mean = 0.454, sd =  0.601)
param_draws_VA$beta_sqrt_rd_release = rnorm(30000, mean = 0.081, sd = 0.356)
param_draws_VA$beta_opt_out = rnorm(30000, mean = -3.908, sd = 2.918)
param_draws_VA$beta_striper_blue = rnorm(30000, mean = 0.454, sd = 1.991)
param_draws_VA$beta_cost = rnorm(30000, mean = -0.008, sd = 0)
param_draws_VA$parameter_draw=1

param_draws_NC = as.data.frame(1:30000)
names(param_draws_NC)[names(param_draws_NC) == "1:30000"] = "tripid"

param_draws_NC$beta_sqrt_sf_keep = rnorm(30000, mean = 0.521, sd = 0.464)
param_draws_NC$beta_sqrt_sf_release = rnorm(30000, mean = 0.108, sd = 0.221)
param_draws_NC$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.192, sd = 0.200)
param_draws_NC$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0.131)
param_draws_NC$beta_sqrt_wf_keep = rnorm(30000, mean = 0.231, sd =  0.393)
param_draws_NC$beta_sqrt_wf_release = rnorm(30000, mean = 0, sd = 0.146)
param_draws_NC$beta_sqrt_rd_keep = rnorm(30000, mean = 0.454, sd =  0.601)
param_draws_NC$beta_sqrt_rd_release = rnorm(30000, mean = 0.081, sd = 0.356)
param_draws_NC$beta_opt_out = rnorm(30000, mean = -3.908, sd = 2.918)
param_draws_NC$beta_striper_blue = rnorm(30000, mean = 0.454, sd = 1.991)
param_draws_NC$beta_cost = rnorm(30000, mean = -0.008, sd = 0)
param_draws_NC$parameter_draw=1


param_draws_all <- NULL
param_draws_all[[1]] <- param_draws_MA
param_draws_all[[2]] <- param_draws_RI
param_draws_all[[3]] <- param_draws_CT
param_draws_all[[4]] <- param_draws_NY
param_draws_all[[5]] <- param_draws_NJ
param_draws_all[[6]] <- param_draws_DE
param_draws_all[[7]] <- param_draws_MD
param_draws_all[[8]] <- param_draws_VA
param_draws_all[[9]] <- param_draws_NC