require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSage.R')


fitCDPU <- stan(file = '../../../stan/CDPnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likCDPU <- extract_log_lik(fitCDPU,c('log_lik'),merge_chains=FALSE)
loo_CDPU <- loo(log_likCDPU, r_eff=relative_eff(exp(log_likCDPU)),cores=4,save_psis = TRUE)
print(loo_CDPU)

save(under18_mobility_dat,fitCDPU,log_likCDPU,loo_CDPU,file='CDPUnder18.RData')

fitCDP18_30 <- stan(file = '../../../stan/CDPnegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDP18_30 <- extract_log_lik(fitCDP18_30,c('log_lik'),merge_chains=FALSE)
loo_CDP18_30 <- loo(log_likCDP18_30, r_eff=relative_eff(exp(log_likCDP18_30)),cores=4,save_psis = TRUE)
print(loo_CDP18_30)

save(a18_30_mobility_dat,fitCDP18_30,log_likCDP18_30,loo_CDP18_30,file='CDP18_30.RData')

fitCDP30_60 <- stan(file = '../../../stan/CDPnegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDP30_60 <- extract_log_lik(fitCDP30_60,c('log_lik'),merge_chains=FALSE)
loo_CDP30_60 <- loo(log_likCDP30_60, r_eff=relative_eff(exp(log_likCDP30_60)),cores=4,save_psis = TRUE)
print(loo_CDP30_60)

save(a30_60_mobility_dat,fitCDP30_60,log_likCDP30_60,loo_CDP30_60,file='CDP30_60.RData')

fitCDP60_100 <- stan(file = '../../../stan/CDPnegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDP60_100 <- extract_log_lik(fitCDP60_100,c('log_lik'),merge_chains=FALSE)
loo_CDP60_100 <- loo(log_likCDP60_100, r_eff=relative_eff(exp(log_likCDP60_100)),cores=4,save_psis = TRUE)
print(loo_CDP60_100)

save(a60_100_mobility_dat,fitCDP60_100,log_likCDP60_100,loo_CDP60_100,file='CDP60_100.RData')

