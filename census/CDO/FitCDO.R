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
Ecensus_mobility_dat$no_patches = length(Ecensus_mobility_dat$N)
Ecensus_mobility_dat$L = array(c(-1))
Ecensus_mobility_dat$Lno = 0

Ecensus_mobility_dat$non_zero = Ecensus_mobility_dat$flux>0
Ecensus_mobility_dat$no_non_zero = sum(Ecensus_mobility_dat$non_zero)


fitCDO_CS <- stan(file = '../../stan/CDOnegbin.stan',
                  data = scotland_census_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDO_CS   <- extract_log_lik(fitCDO_CS  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CS   <- loo(log_likCDO_CS  , r_eff=relative_eff(exp(log_likCDO_CS)),cores=4,save_psis = TRUE)
print(loo_CDO_CS )

save(fitCDO_CS,log_likCDO_CS,loo_CDO_CS,file='CDO_SCensus.RData')

fitCDO_CNI <- stan(file = '../../stan/CDOnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likCDO_CNI   <- extract_log_lik(fitCDO_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CNI   <- loo(log_likCDO_CNI  , r_eff=relative_eff(exp(log_likCDO_CNI)),cores=4,save_psis = TRUE)
print(loo_CDO_CNI)

save(fitCDO_CNI,log_likCDO_CNI,loo_CDO_CNI,file='CDO_NICensus.RData')


fitCDO_CW <- stan(file = '../../stan/CDOnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDO_CW   <- extract_log_lik(fitCDO_CW  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CW   <- loo(log_likCDO_CW  , r_eff=relative_eff(exp(log_likCDO_CW)),cores=4,save_psis = TRUE)
print(loo_CDO_CW)

save(fitCDO_CW,log_likCDO_CW,loo_CDO_CW,file='CDO_WCensus.RData')

fitCDO_CE <- stan(file = '../../stan/CDOnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDO_CE   <- extract_log_lik(fitCDO_CE  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CE   <- loo(log_likCDO_CE  , r_eff=relative_eff(exp(log_likCDO_CE)),cores=4,save_psis = TRUE)
print(loo_CDO_CE)

save(fitCDO_CE,log_likCDO_CE,loo_CDO_CE,file='CDO_ECensus.RData')