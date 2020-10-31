require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSage.R')


fitCDORU <- stan(file = '../../../stan/CDORnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likCDORU <- extract_log_lik(fitCDORU,c('log_lik'),merge_chains=FALSE)
loo_CDORU <- loo(log_likCDORU, r_eff=relative_eff(exp(log_likCDORU)),cores=4,save_psis = TRUE)
print(loo_CDORU)

save(under18_mobility_dat,fitCDORU,log_likCDORU,loo_CDORU,file='CDORUnder18.RData')

fitCDOR18_30 <- stan(file = '../../../stan/CDORnegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDOR18_30 <- extract_log_lik(fitCDOR18_30,c('log_lik'),merge_chains=FALSE)
loo_CDOR18_30 <- loo(log_likCDOR18_30, r_eff=relative_eff(exp(log_likCDOR18_30)),cores=4,save_psis = TRUE)
print(loo_CDOR18_30)

save(a18_30_mobility_dat,fitCDOR18_30,log_likCDOR18_30,loo_CDOR18_30,file='CDOR18_30.RData')

fitCDOR30_60 <- stan(file = '../../../stan/CDORnegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDOR30_60 <- extract_log_lik(fitCDOR30_60,c('log_lik'),merge_chains=FALSE)
loo_CDOR30_60 <- loo(log_likCDOR30_60, r_eff=relative_eff(exp(log_likCDOR30_60)),cores=4,save_psis = TRUE)
print(loo_CDOR30_60)

save(a30_60_mobility_dat,fitCDOR30_60,log_likCDOR30_60,loo_CDOR30_60,file='CDOR30_60.RData')

fitCDOR60_100 <- stan(file = '../../../stan/CDORnegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDOR60_100 <- extract_log_lik(fitCDOR60_100,c('log_lik'),merge_chains=FALSE)
loo_CDOR60_100 <- loo(log_likCDOR60_100, r_eff=relative_eff(exp(log_likCDOR60_100)),cores=4,save_psis = TRUE)
print(loo_CDOR60_100)

save(a60_100_mobility_dat,fitCDOR60_100,log_likCDOR60_100,loo_CDOR60_100,file='CDOR60_100.RData')

