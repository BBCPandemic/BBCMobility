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


fit_S_CDPT <- stan(file = '../../../stan/CDPnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_CDPT <- extract_log_lik(fit_S_CDPT,c('log_lik'),merge_chains=FALSE)
loo_S_CDPT <- loo(log_lik_S_CDPT, r_eff=relative_eff(exp(log_lik_S_CDPT)),cores=4,save_psis = TRUE)
print(loo_S_CDPT)

save(S_total_mobility_dat,fit_S_CDPT,log_lik_S_CDPT,loo_S_CDPT,file='S_CDPTotal.RData')

fit_W_CDPT <- stan(file = '../../../stan/CDPnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_CDPT <- extract_log_lik(fit_W_CDPT,c('log_lik'),merge_chains=FALSE)
loo_W_CDPT <- loo(log_lik_W_CDPT, r_eff=relative_eff(exp(log_lik_W_CDPT)),cores=4,save_psis = TRUE)
print(loo_W_CDPT)

save(W_total_mobility_dat,fit_W_CDPT,log_lik_W_CDPT,loo_W_CDPT,file='W_CDPTotal.RData')

fit_NI_CDPT <- stan(file = '../../../stan/CDPnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_CDPT <- extract_log_lik(fit_NI_CDPT,c('log_lik'),merge_chains=FALSE)
loo_NI_CDPT <- loo(log_lik_NI_CDPT, r_eff=relative_eff(exp(log_lik_NI_CDPT)),cores=4,save_psis = TRUE)
print(loo_NI_CDPT)

save(NI_total_mobility_dat,fit_NI_CDPT,log_lik_NI_CDPT,loo_NI_CDPT,file='NI_CDPTotal.RData')

fit_E_CDPT <- stan(file = '../../../stan/CDPnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_CDPT <- extract_log_lik(fit_E_CDPT,c('log_lik'),merge_chains=FALSE)
loo_E_CDPT <- loo(log_lik_E_CDPT, r_eff=relative_eff(exp(log_lik_E_CDPT)),cores=4,save_psis = TRUE)
print(loo_E_CDPT)

save(E_total_mobility_dat,fit_E_CDPT,log_lik_E_CDPT,loo_E_CDPT,file='E_CDPTotal.RData')




