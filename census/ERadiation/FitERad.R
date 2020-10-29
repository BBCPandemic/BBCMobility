require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSCensus.R')

fitERad_CS <- stan(file = '../../stan/ERadnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 2000, chains = 4)

log_likERad_CS   <- extract_log_lik(fitERad_CS  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CS   <- loo(log_likERad_CS  , r_eff=relative_eff(exp(log_likERad_CS)),cores=4,save_psis = TRUE)
print(loo_ERad_CS )

save(fitERad_CS,log_likERad_CS,loo_ERad_CS,file='ERad_SCensus.RData')

fitERad_CNI <- stan(file = '../../stan/ERadnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 2000, chains = 4)

log_likERad_CNI   <- extract_log_lik(fitERad_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CNI   <- loo(log_likERad_CNI  , r_eff=relative_eff(exp(log_likERad_CNI)),cores=4,save_psis = TRUE)
print(loo_ERad_CNI)

save(fitERad_CNI,log_likERad_CNI,loo_ERad_CNI,file='ERad_NICensus.RData')

fitERad_CW <- stan(file = '../../stan/ERadnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likERad_CW   <- extract_log_lik(fitERad_CW  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CW   <- loo(log_likERad_CW  , r_eff=relative_eff(exp(log_likERad_CW)),cores=4,save_psis = TRUE)
print(loo_ERad_CW)

save(fitERad_CW,log_likERad_CW,loo_ERad_CW,file='ERad_WCensus.RData')

fitERad_CE <- stan(file = '../../stan/ERadnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likERad_CE   <- extract_log_lik(fitERad_CE  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CE   <- loo(log_likERad_CE  , r_eff=relative_eff(exp(log_likERad_CE)),cores=4,save_psis = TRUE)
print(loo_ERad_CE)

save(fitERad_CE,log_likERad_CE,loo_ERad_CE,file='ERad_ECensus.RData')
