require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/scotland/S_mobility_flux_furthest.RData')
load('../../../flux/wales/W_mobility_flux_furthest.RData')
load('../../../flux/ni/NI_mobility_flux_furthest.RData')
load('../../../flux/england/E_mobility_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSsubnat.R')



fit_S_ERadT <- stan(file = '../../../stan/ERadnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_ERadT <- extract_log_lik(fit_S_ERadT,c('log_lik'),merge_chains=FALSE)
loo_S_ERadT <- loo(log_lik_S_ERadT, r_eff=relative_eff(exp(log_lik_S_ERadT)),cores=4,save_psis = TRUE)
print(loo_S_ERadT)

save(S_total_mobility_dat,fit_S_ERadT,log_lik_S_ERadT,loo_S_ERadT,file='S_ERadTotal.RData')

fit_W_ERadT <- stan(file = '../../../stan/ERadnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_ERadT <- extract_log_lik(fit_W_ERadT,c('log_lik'),merge_chains=FALSE)
loo_W_ERadT <- loo(log_lik_W_ERadT, r_eff=relative_eff(exp(log_lik_W_ERadT)),cores=4,save_psis = TRUE)
print(loo_W_ERadT)

save(W_total_mobility_dat,fit_W_ERadT,log_lik_W_ERadT,loo_W_ERadT,file='W_ERadTotal.RData')

fit_NI_ERadT <- stan(file = '../../../stan/ERadnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_ERadT <- extract_log_lik(fit_NI_ERadT,c('log_lik'),merge_chains=FALSE)
loo_NI_ERadT <- loo(log_lik_NI_ERadT, r_eff=relative_eff(exp(log_lik_NI_ERadT)),cores=4,save_psis = TRUE)
print(loo_NI_ERadT)

save(NI_total_mobility_dat,fit_NI_ERadT,log_lik_NI_ERadT,loo_NI_ERadT,file='NI_ERadTotal.RData')

fit_E_ERadT <- stan(file = '../../../stan/ERadnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_ERadT <- extract_log_lik(fit_E_ERadT,c('log_lik'),merge_chains=FALSE)
loo_E_ERadT <- loo(log_lik_E_ERadT, r_eff=relative_eff(exp(log_lik_E_ERadT)),cores=4,save_psis = TRUE)
print(loo_E_ERadT)

save(E_total_mobility_dat,fit_E_ERadT,log_lik_E_ERadT,loo_E_ERadT,file='E_ERadTotal.RData')




