require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSage.R')


fitCDOU <- stan(file = '../../../stan/CDOnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likCDOU <- extract_log_lik(fitCDOU,c('log_lik'),merge_chains=FALSE)
loo_CDOU <- loo(log_likCDOU, r_eff=relative_eff(exp(log_likCDOU)),cores=4,save_psis = TRUE)
print(loo_CDOU)

save(under18_mobility_dat,fitCDOU,log_likCDOU,loo_CDOU,file='CDOUnder18.RData')

fitCDO18_30 <- stan(file = '../../../stan/CDOnegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDO18_30 <- extract_log_lik(fitCDO18_30,c('log_lik'),merge_chains=FALSE)
loo_CDO18_30 <- loo(log_likCDO18_30, r_eff=relative_eff(exp(log_likCDO18_30)),cores=4,save_psis = TRUE)
print(loo_CDO18_30)

save(a18_30_mobility_dat,fitCDO18_30,log_likCDO18_30,loo_CDO18_30,file='CDO18_30.RData')

fitCDO30_60 <- stan(file = '../../../stan/CDOnegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDO30_60 <- extract_log_lik(fitCDO30_60,c('log_lik'),merge_chains=FALSE)
loo_CDO30_60 <- loo(log_likCDO30_60, r_eff=relative_eff(exp(log_likCDO30_60)),cores=4,save_psis = TRUE)
print(loo_CDO30_60)

save(a30_60_mobility_dat,fitCDO30_60,log_likCDO30_60,loo_CDO30_60,file='CDO30_60.RData')

fitCDO60_100 <- stan(file = '../../../stan/CDOnegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDO60_100 <- extract_log_lik(fitCDO60_100,c('log_lik'),merge_chains=FALSE)
loo_CDO60_100 <- loo(log_likCDO60_100, r_eff=relative_eff(exp(log_likCDO60_100)),cores=4,save_psis = TRUE)
print(loo_CDO60_100)

save(a60_100_mobility_dat,fitCDO60_100,log_likCDO60_100,loo_CDO60_100,file='CDO60_100.RData')

