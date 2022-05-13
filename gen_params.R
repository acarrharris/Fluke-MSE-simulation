
param_draws_MA = as.data.frame(1:30000)
names(param_draws_MA)[names(param_draws_MA) == "1:30000"] = "tripid"
param_draws_MA$beta_sqrt_sf_keep = rnorm(30000, mean = 0.535, sd = 0.692)
param_draws_MA$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.358)
param_draws_MA$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.273, sd = 0.245)
param_draws_MA$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_MA$beta_sqrt_scup_keep = rnorm(30000, mean = 0.078, sd = 0.096)
param_draws_MA$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0.077)
param_draws_MA$beta_opt_out = rnorm(30000, mean = -2.398, sd = 2.193)
param_draws_MA$beta_striper_blue = rnorm(30000, mean = 1.272, sd = 1.652)
param_draws_MA$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_MA$parameter_draw=1


param_draws_RI = as.data.frame(1:30000)
names(param_draws_RI)[names(param_draws_RI) == "1:30000"] = "tripid"
param_draws_RI$beta_sqrt_sf_keep = rnorm(30000, mean = 0.535, sd = 0.692)
param_draws_RI$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.358)
param_draws_RI$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.273, sd = 0.245)
param_draws_RI$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_RI$beta_sqrt_scup_keep = rnorm(30000, mean = 0.078, sd = 0.096)
param_draws_RI$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0.077)
param_draws_RI$beta_opt_out = rnorm(30000, mean = -2.398, sd = 2.193)
param_draws_RI$beta_striper_blue = rnorm(30000, mean = 1.272, sd = 1.652)
param_draws_RI$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_RI$parameter_draw=1


param_draws_CT = as.data.frame(1:30000)
names(param_draws_CT)[names(param_draws_CT) == "1:30000"] = "tripid"
param_draws_CT$beta_sqrt_sf_keep = rnorm(30000, mean = 0.535, sd = 0.692)
param_draws_CT$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.358)
param_draws_CT$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.273, sd = 0.245)
param_draws_CT$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_CT$beta_sqrt_scup_keep = rnorm(30000, mean = 0.078, sd = 0.096)
param_draws_CT$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0.077)
param_draws_CT$beta_opt_out = rnorm(30000, mean = -2.398, sd = 2.193)
param_draws_CT$beta_striper_blue = rnorm(30000, mean = 1.272, sd = 1.652)
param_draws_CT$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_CT$parameter_draw=1


param_draws_NY = as.data.frame(1:30000)
names(param_draws_NY)[names(param_draws_NY) == "1:30000"] = "tripid"
param_draws_NY$beta_sqrt_sf_keep = rnorm(30000, mean = 0.535, sd = 0.692)
param_draws_NY$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.358)
param_draws_NY$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.273, sd = 0.245)
param_draws_NY$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_NY$beta_sqrt_scup_keep = rnorm(30000, mean = 0.078, sd = 0.096)
param_draws_NY$beta_sqrt_scup_release = rnorm(30000, mean = 0, sd = 0.077)
param_draws_NY$beta_opt_out = rnorm(30000, mean = -2.398, sd = 2.193)
param_draws_NY$beta_striper_blue = rnorm(30000, mean = 1.272, sd = 1.652)
param_draws_NY$beta_cost = rnorm(30000, mean = -0.012, sd = 0)
param_draws_NY$parameter_draw=1


param_draws_NJ = as.data.frame(1:30000)
names(param_draws_NJ)[names(param_draws_NJ) == "1:30000"] = "tripid"
param_draws_NJ$beta_sqrt_sf_keep = rnorm(30000, mean = 0.721, sd = 0.630)
param_draws_NJ$beta_sqrt_sf_release = rnorm(30000, mean = 0.0, sd = 0)
param_draws_NJ$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.175, sd = 0.283)
param_draws_NJ$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0)
param_draws_NJ$beta_sqrt_scup_keep = rnorm(30000, mean = 0.096, sd =  0.128)
param_draws_NJ$beta_sqrt_scup_release = rnorm(30000, mean = -0.033, sd = 0.120)
param_draws_NJ$beta_sqrt_wf_keep = rnorm(30000, mean = 0.367, sd =  0.220)
param_draws_NJ$beta_sqrt_wf_release = rnorm(30000, mean = 0.096, sd = 0.223)
param_draws_NJ$beta_opt_out = rnorm(30000, mean = -1.877, sd = 1.969)
param_draws_NJ$beta_striper_blue = rnorm(30000, mean = 1.049, sd = 1.799)
param_draws_NJ$beta_cost = rnorm(30000, mean = -0.008, sd = 0)
param_draws_NJ$parameter_draw=1


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
param_draws_MD$parameter_draw=1


param_draws_DE = as.data.frame(1:30000)
names(param_draws_DE)[names(param_draws_DE) == "1:30000"] = "tripid"
param_draws_DE$beta_sqrt_sf_keep = rnorm(30000, mean = 0.776, sd = 0.516)
param_draws_DE$beta_sqrt_sf_release = rnorm(30000, mean = 0, sd = 0.258)
param_draws_DE$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.239, sd = 0.311)
param_draws_DE$beta_sqrt_bsb_release = rnorm(30000, mean = 0, sd = 0.139)
param_draws_DE$beta_sqrt_wf_keep = rnorm(30000, mean = 0.360, sd =  0.251)
param_draws_DE$beta_sqrt_wf_release = rnorm(30000, mean = 0.061, sd = 0.220)
param_draws_DE$beta_opt_out = rnorm(30000, mean = -2.838, sd = 2.246)
param_draws_DE$beta_striper_blue = rnorm(30000, mean = 0.606, sd = 1.752)
param_draws_DE$beta_cost = rnorm(30000, mean = -0.009, sd = 0)
param_draws_DE$parameter_draw=1


param_draws_VA = as.data.frame(1:30000)
names(param_draws_VA)[names(param_draws_VA) == "1:30000"] = "tripid"
param_draws_VA$beta_sqrt_sf_keep = rnorm(30000, mean = 0.507, sd = 0.457)
param_draws_VA$beta_sqrt_sf_release = rnorm(30000, mean = 0.105, sd = 0.230)
param_draws_VA$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.178, sd = 0.189)
param_draws_VA$beta_sqrt_bsb_release = rnorm(30000, mean = 0.025, sd = 0.087)
param_draws_VA$beta_sqrt_wf_keep = rnorm(30000, mean = 0.231, sd =  0.283)
param_draws_VA$beta_sqrt_wf_release = rnorm(30000, mean = 0, sd = 0.142)
param_draws_VA$beta_sqrt_rd_keep = rnorm(30000, mean = 0.428, sd =  0.472)
param_draws_VA$beta_sqrt_rd_release = rnorm(30000, mean = 0.081, sd = 0.324)
param_draws_VA$beta_opt_out = rnorm(30000, mean = -3.573, sd = 2.676)
param_draws_VA$beta_striper_blue = rnorm(30000, mean = 0.493, sd = 1.839)
param_draws_VA$beta_cost = rnorm(30000, mean = -0.007, sd = 0)
param_draws_VA$parameter_draw=1


param_draws_NC = as.data.frame(1:30000)
names(param_draws_NC)[names(param_draws_NC) == "1:30000"] = "tripid"
param_draws_NC$beta_sqrt_sf_keep = rnorm(30000, mean = 0.507, sd = 0.457)
param_draws_NC$beta_sqrt_sf_release = rnorm(30000, mean = 0.105, sd = 0.230)
param_draws_NC$beta_sqrt_bsb_keep = rnorm(30000, mean = 0.178, sd = 0.189)
param_draws_NC$beta_sqrt_bsb_release = rnorm(30000, mean = 0.025, sd = 0.087)
param_draws_NC$beta_sqrt_wf_keep = rnorm(30000, mean = 0.231, sd =  0.283)
param_draws_NC$beta_sqrt_wf_release = rnorm(30000, mean = 0, sd = 0.142)
param_draws_NC$beta_sqrt_rd_keep = rnorm(30000, mean = 0.428, sd =  0.472)
param_draws_NC$beta_sqrt_rd_release = rnorm(30000, mean = 0.081, sd = 0.324)
param_draws_NC$beta_opt_out = rnorm(30000, mean = -3.573, sd = 2.676)
param_draws_NC$beta_striper_blue = rnorm(30000, mean = 0.493, sd = 1.839)
param_draws_NC$beta_cost = rnorm(30000, mean = -0.007, sd = 0)
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