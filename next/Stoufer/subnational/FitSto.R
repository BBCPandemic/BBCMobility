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

fit_S_StoT <- stan(file = '../../../stan/Stoufernegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_StoT <- extract_log_lik(fit_S_StoT,c('log_lik'),merge_chains=FALSE)
loo_S_StoT <- loo(log_lik_S_StoT, r_eff=relative_eff(exp(log_lik_S_StoT)),cores=4,save_psis = TRUE)
print(loo_S_StoT)

save(S_total_mobility_dat,fit_S_StoT,log_lik_S_StoT,loo_S_StoT,file='S_StoTotal.RData')

fit_W_StoT <- stan(file = '../../../stan/Stoufernegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_StoT <- extract_log_lik(fit_W_StoT,c('log_lik'),merge_chains=FALSE)
loo_W_StoT <- loo(log_lik_W_StoT, r_eff=relative_eff(exp(log_lik_W_StoT)),cores=4,save_psis = TRUE)
print(loo_W_StoT)

save(W_total_mobility_dat,fit_W_StoT,log_lik_W_StoT,loo_W_StoT,file='W_StoTotal.RData')

fit_NI_StoT <- stan(file = '../../../stan/Stoufernegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_StoT <- extract_log_lik(fit_NI_StoT,c('log_lik'),merge_chains=FALSE)
loo_NI_StoT <- loo(log_lik_NI_StoT, r_eff=relative_eff(exp(log_lik_NI_StoT)),cores=4,save_psis = TRUE)
print(loo_NI_StoT)

save(NI_total_mobility_dat,fit_NI_StoT,log_lik_NI_StoT,loo_NI_StoT,file='NI_StoTotal.RData')

fit_E_StoT <- stan(file = '../../../stan/Stoufernegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_StoT <- extract_log_lik(fit_E_StoT,c('log_lik'),merge_chains=FALSE)
loo_E_StoT <- loo(log_lik_E_StoT, r_eff=relative_eff(exp(log_lik_E_StoT)),cores=4,save_psis = TRUE)
print(loo_E_StoT)

save(E_total_mobility_dat,fit_E_StoT,log_lik_E_StoT,loo_E_StoT,file='E_StoTotal.RData')




