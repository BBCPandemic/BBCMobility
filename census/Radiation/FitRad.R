require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

scotland_census_mobility_dat$N = scotland_census_mobility_dat$N[-c(9)]
scotland_census_mobility_dat$flux = scotland_census_mobility_dat$flux[-c(9)]
scotland_census_mobility_dat$A = scotland_census_mobility_dat$A[-c(9)]
scotland_census_mobility_dat$mv = scotland_census_mobility_dat$mv[-c(9),-c(9)]
scotland_census_mobility_dat$r = scotland_census_mobility_dat$r[-c(9),-c(9)]
scotland_census_mobility_dat$s = scotland_census_mobility_dat$s[-c(9),-c(9)]
scotland_census_mobility_dat$no_patches = length(scotland_census_mobility_dat$N)
scotland_census_mobility_dat$L = array(c(-1))
scotland_census_mobility_dat$Lno = 0

scotland_census_mobility_dat$non_zero = scotland_census_mobility_dat$flux>0
scotland_census_mobility_dat$no_non_zero = sum(scotland_census_mobility_dat$non_zero)

Ecensus_mobility_dat$N = Ecensus_mobility_dat$N[-c(294,326)]
Ecensus_mobility_dat$flux = Ecensus_mobility_dat$flux[-c(294,326)]
Ecensus_mobility_dat$A = Ecensus_mobility_dat$A[-c(294,326)]
Ecensus_mobility_dat$mv = Ecensus_mobility_dat$mv[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$r = Ecensus_mobility_dat$r[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$s = Ecensus_mobility_dat$s[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$no_patches = length(Ecensus_mobility_dat$N)
Ecensus_mobility_dat$L = array(c(-1))
Ecensus_mobility_dat$Lno = 0

Ecensus_mobility_dat$non_zero = Ecensus_mobility_dat$flux>0
Ecensus_mobility_dat$no_non_zero = sum(Ecensus_mobility_dat$non_zero)

fitRad_CS <- stan(file = '../../stan/Radnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 2000, chains = 4)

log_likRad_CS   <- extract_log_lik(fitRad_CS  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CS   <- loo(log_likRad_CS  , r_eff=relative_eff(exp(log_likRad_CS)),cores=4,save_psis = TRUE)
print(loo_Rad_CS )

save(fitRad_CS,log_likRad_CS,loo_Rad_CS,file='Rad_SCensus.RData')

fitRad_CNI <- stan(file = '../../stan/Radnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 2000, chains = 4)

log_likRad_CNI   <- extract_log_lik(fitRad_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CNI   <- loo(log_likRad_CNI  , r_eff=relative_eff(exp(log_likRad_CNI)),cores=4,save_psis = TRUE)
print(loo_Rad_CNI)

save(fitRad_CNI,log_likRad_CNI,loo_Rad_CNI,file='Rad_NICensus.RData')

fitRad_CW <- stan(file = '../../stan/Radnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likRad_CW   <- extract_log_lik(fitRad_CW  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CW   <- loo(log_likRad_CW  , r_eff=relative_eff(exp(log_likRad_CW)),cores=4,save_psis = TRUE)
print(loo_Rad_CW)

save(fitRad_CW,log_likRad_CW,loo_Rad_CW,file='Rad_WCensus.RData')

fitRad_CE <- stan(file = '../../stan/Radnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likRad_CE   <- extract_log_lik(fitRad_CE  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CE   <- loo(log_likRad_CE  , r_eff=relative_eff(exp(log_likRad_CE)),cores=4,save_psis = TRUE)
print(loo_Rad_CE)

save(fitRad_CE,log_likRad_CE,loo_Rad_CE,file='Rad_ECensus.RData')
