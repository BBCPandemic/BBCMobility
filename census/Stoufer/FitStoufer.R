require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../DropLADSCensus.R')


fitStoufer_CS <- stan(file = '../../stan/Stoufernegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4)

log_likStoufer_CS   <- extract_log_lik(fitStoufer_CS  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CS   <- loo(log_likStoufer_CS  , r_eff=relative_eff(exp(log_likStoufer_CS)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CS )

save(fitStoufer_CS,log_likStoufer_CS,loo_Stoufer_CS,file='Sto_CSCensus.RData')

fitStoufer_CNI <- stan(file = '../../stan/Stoufernegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likStoufer_CNI   <- extract_log_lik(fitStoufer_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CNI   <- loo(log_likStoufer_CNI  , r_eff=relative_eff(exp(log_likStoufer_CNI)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CNI)

save(fitStoufer_CNI,log_likStoufer_CNI,loo_Stoufer_CNI,file='Sto_NICensus.RData')


fitStoufer_CW <- stan(file = '../../stan/Stoufernegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likStoufer_CW   <- extract_log_lik(fitStoufer_CW  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CW   <- loo(log_likStoufer_CW  , r_eff=relative_eff(exp(log_likStoufer_CW)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CW)

save(fitStoufer_CW,log_likStoufer_CW,loo_Stoufer_CW,file='Sto_WCensus.RData')

fitStoufer_CE <- stan(file = '../../stan/Stoufernegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likStoufer_CE   <- extract_log_lik(fitStoufer_CE  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CE   <- loo(log_likStoufer_CE  , r_eff=relative_eff(exp(log_likStoufer_CE)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CE)

save(fitStoufer_CE,log_likStoufer_CE,loo_Stoufer_CE,file='Sto_ECensus.RData')
