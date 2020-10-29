require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSage.R')


fitImpU <- stan(file = '../../../stan/Impnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likImpU <- extract_log_lik(fitImpU,c('log_lik'),merge_chains=FALSE)
loo_ImpU <- loo(log_likImpU, r_eff=relative_eff(exp(log_likImpU)),cores=4,save_psis = TRUE)
print(loo_ImpU)

save(under18_mobility_dat,fitImpU,log_likImpU,loo_ImpU,file='ImpUnder18.RData')

fitImp18_30 <- stan(file = '../../../stan/Impnegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4)

log_likImp18_30 <- extract_log_lik(fitImp18_30,c('log_lik'),merge_chains=FALSE)
loo_Imp18_30 <- loo(log_likImp18_30, r_eff=relative_eff(exp(log_likImp18_30)),cores=4,save_psis = TRUE)
print(loo_Imp18_30)

save(a18_30_mobility_dat,fitImp18_30,log_likImp18_30,loo_Imp18_30,file='Imp18_30.RData')

fitImp30_60 <- stan(file = '../../../stan/Impnegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4)

log_likImp30_60 <- extract_log_lik(fitImp30_60,c('log_lik'),merge_chains=FALSE)
loo_Imp30_60 <- loo(log_likImp30_60, r_eff=relative_eff(exp(log_likImp30_60)),cores=4,save_psis = TRUE)
print(loo_Imp30_60)

save(a30_60_mobility_dat,fitImp30_60,log_likImp30_60,loo_Imp30_60,file='Imp30_60.RData')

fitImp60_100 <- stan(file = '../../../stan/Impnegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4)

log_likImp60_100 <- extract_log_lik(fitImp60_100,c('log_lik'),merge_chains=FALSE)
loo_Imp60_100 <- loo(log_likImp60_100, r_eff=relative_eff(exp(log_likImp60_100)),cores=4,save_psis = TRUE)
print(loo_Imp60_100)

save(a60_100_mobility_dat,fitImp60_100,log_likImp60_100,loo_Imp60_100,file='Imp60_100.RData')

