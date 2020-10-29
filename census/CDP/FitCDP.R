require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSCensus.R')


fitCDP_CS <- stan(file = '../../stan/CDPnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4)

log_likCDP_CS   <- extract_log_lik(fitCDP_CS  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CS   <- loo(log_likCDP_CS  , r_eff=relative_eff(exp(log_likCDP_CS)),cores=4,save_psis = TRUE)
print(loo_CDP_CS )

save(fitCDP_CS,log_likCDP_CS,loo_CDP_CS,file='CDP_SCensus.RData')

fitCDP_CNI <- stan(file = '../../stan/CDPnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likCDP_CNI   <- extract_log_lik(fitCDP_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CNI   <- loo(log_likCDP_CNI  , r_eff=relative_eff(exp(log_likCDP_CNI)),cores=4,save_psis = TRUE)
print(loo_CDP_CNI)

save(fitCDP_CNI,log_likCDP_CNI,loo_CDP_CNI,file='CDP_NICensus.RData')


fitCDP_CW <- stan(file = '../../stan/CDPnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDP_CW   <- extract_log_lik(fitCDP_CW  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CW   <- loo(log_likCDP_CW  , r_eff=relative_eff(exp(log_likCDP_CW)),cores=4,save_psis = TRUE)
print(loo_CDP_CW)

save(fitCDP_CW,log_likCDP_CW,loo_CDP_CW,file='CDP_WCensus.RData')

fitCDP_CE <- stan(file = '../../stan/CDPnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDP_CE   <- extract_log_lik(fitCDP_CE  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CE   <- loo(log_likCDP_CE  , r_eff=relative_eff(exp(log_likCDP_CE)),cores=4,save_psis = TRUE)
print(loo_CDP_CE)

save(fitCDP_CE,log_likCDP_CE,loo_CDP_CE,file='CDP_ECensus.RData')


