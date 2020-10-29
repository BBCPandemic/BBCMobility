require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSage.R')

fitStoU <- stan(file = '../../../stan/Stoufernegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likStoU <- extract_log_lik(fitStoU,c('log_lik'),merge_chains=FALSE)
loo_StoU <- loo(log_likStoU, r_eff=relative_eff(exp(log_likStoU)),cores=4,save_psis = TRUE)
print(loo_StoU)

save(under18_mobility_dat,fitStoU,log_likStoU,loo_StoU,file='StoUnder18.RData')

fitSto18_30 <- stan(file = '../../../stan/Stoufernegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4)

log_likSto18_30 <- extract_log_lik(fitSto18_30,c('log_lik'),merge_chains=FALSE)
loo_Sto18_30 <- loo(log_likSto18_30, r_eff=relative_eff(exp(log_likSto18_30)),cores=4,save_psis = TRUE)
print(loo_Sto18_30)

save(a18_30_mobility_dat,fitSto18_30,log_likSto18_30,loo_Sto18_30,file='Sto18_30.RData')

fitSto30_60 <- stan(file = '../../../stan/Stoufernegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4)

log_likSto30_60 <- extract_log_lik(fitSto30_60,c('log_lik'),merge_chains=FALSE)
loo_Sto30_60 <- loo(log_likSto30_60, r_eff=relative_eff(exp(log_likSto30_60)),cores=4,save_psis = TRUE)
print(loo_Sto30_60)

save(a30_60_mobility_dat,fitSto30_60,log_likSto30_60,loo_Sto30_60,file='Sto30_60.RData')

fitSto60_100 <- stan(file = '../../../stan/Stoufernegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4)

log_likSto60_100 <- extract_log_lik(fitSto60_100,c('log_lik'),merge_chains=FALSE)
loo_Sto60_100 <- loo(log_likSto60_100, r_eff=relative_eff(exp(log_likSto60_100)),cores=4,save_psis = TRUE)
print(loo_Sto60_100)

save(a60_100_mobility_dat,fitSto60_100,log_likSto60_100,loo_Sto60_100,file='Sto60_100.RData')

