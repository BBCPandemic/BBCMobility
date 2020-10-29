require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/scotland/S_mobility_flux_next.RData')
load('../../../flux/wales/W_mobility_flux_next.RData')
load('../../../flux/ni/NI_mobility_flux_next.RData')
load('../../../flux/england/E_mobility_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSsubnat.R')

fit_S_CDOT <- stan(file = '../../../stan/CDOnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_CDOT <- extract_log_lik(fit_S_CDOT,c('log_lik'),merge_chains=FALSE)
loo_S_CDOT <- loo(log_lik_S_CDOT, r_eff=relative_eff(exp(log_lik_S_CDOT)),cores=4,save_psis = TRUE)
print(loo_S_CDOT)

save(S_total_mobility_dat,fit_S_CDOT,log_lik_S_CDOT,loo_S_CDOT,file='S_CDOTotal.RData')

fit_W_CDOT <- stan(file = '../../../stan/CDOnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_CDOT <- extract_log_lik(fit_W_CDOT,c('log_lik'),merge_chains=FALSE)
loo_W_CDOT <- loo(log_lik_W_CDOT, r_eff=relative_eff(exp(log_lik_W_CDOT)),cores=4,save_psis = TRUE)
print(loo_W_CDOT)

save(W_total_mobility_dat,fit_W_CDOT,log_lik_W_CDOT,loo_W_CDOT,file='W_CDOTotal.RData')

fit_NI_CDOT <- stan(file = '../../../stan/CDOnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_CDOT <- extract_log_lik(fit_NI_CDOT,c('log_lik'),merge_chains=FALSE)
loo_NI_CDOT <- loo(log_lik_NI_CDOT, r_eff=relative_eff(exp(log_lik_NI_CDOT)),cores=4,save_psis = TRUE)
print(loo_NI_CDOT)

save(NI_total_mobility_dat,fit_NI_CDOT,log_lik_NI_CDOT,loo_NI_CDOT,file='NI_CDOTotal.RData')

fit_E_CDOT <- stan(file = '../../../stan/CDOnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_CDOT <- extract_log_lik(fit_E_CDOT,c('log_lik'),merge_chains=FALSE)
loo_E_CDOT <- loo(log_lik_E_CDOT, r_eff=relative_eff(exp(log_lik_E_CDOT)),cores=4,save_psis = TRUE)
print(loo_E_CDOT)

save(E_total_mobility_dat,fit_E_CDOT,log_lik_E_CDOT,loo_E_CDOT,file='E_CDOTotal.RData')




