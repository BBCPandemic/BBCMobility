require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSCensus.R')

fitImp_CS <- stan(file = '../../stan/Impnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4)

log_likImp_CS   <- extract_log_lik(fitImp_CS  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CS   <- loo(log_likImp_CS  , r_eff=relative_eff(exp(log_likImp_CS)),cores=4,save_psis = TRUE)
print(loo_Imp_CS )

save(fitImp_CS,log_likImp_CS,loo_Imp_CS,file='Imp_SCensus.RData')

fitImp_CNI <- stan(file = '../../stan/Impnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likImp_CNI   <- extract_log_lik(fitImp_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CNI   <- loo(log_likImp_CNI  , r_eff=relative_eff(exp(log_likImp_CNI)),cores=4,save_psis = TRUE)
print(loo_Imp_CNI)

save(fitImp_CNI,log_likImp_CNI,loo_Imp_CNI,file='Imp_NICensus.RData')


fitImp_CW <- stan(file = '../../stan/Impnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likImp_CW   <- extract_log_lik(fitImp_CW  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CW   <- loo(log_likImp_CW  , r_eff=relative_eff(exp(log_likImp_CW)),cores=4,save_psis = TRUE)
print(loo_Imp_CW)

save(fitImp_CW,log_likImp_CW,loo_Imp_CW,file='Imp_WCensus.RData')

fitImp_CE <- stan(file = '../../stan/Impnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likImp_CE   <- extract_log_lik(fitImp_CE  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CE   <- loo(log_likImp_CE  , r_eff=relative_eff(exp(log_likImp_CE)),cores=4,save_psis = TRUE)
print(loo_Imp_CE)

save(fitImp_CE,log_likImp_CE,loo_Imp_CE,file='Imp_ECensus.RData')
