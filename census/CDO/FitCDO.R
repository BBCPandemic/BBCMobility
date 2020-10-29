require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../DropLADSCensus.R')

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