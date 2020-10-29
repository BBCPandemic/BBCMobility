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

fit_S_ImpT <- stan(file = '../../../stan/Impnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_ImpT <- extract_log_lik(fit_S_ImpT,c('log_lik'),merge_chains=FALSE)
loo_S_ImpT <- loo(log_lik_S_ImpT, r_eff=relative_eff(exp(log_lik_S_ImpT)),cores=4,save_psis = TRUE)
print(loo_S_ImpT)

save(S_total_mobility_dat,fit_S_ImpT,log_lik_S_ImpT,loo_S_ImpT,file='S_ImpTotal.RData')

fit_W_ImpT <- stan(file = '../../../stan/Impnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_ImpT <- extract_log_lik(fit_W_ImpT,c('log_lik'),merge_chains=FALSE)
loo_W_ImpT <- loo(log_lik_W_ImpT, r_eff=relative_eff(exp(log_lik_W_ImpT)),cores=4,save_psis = TRUE)
print(loo_W_ImpT)

save(W_total_mobility_dat,fit_W_ImpT,log_lik_W_ImpT,loo_W_ImpT,file='W_ImpTotal.RData')

fit_NI_ImpT <- stan(file = '../../../stan/Impnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_ImpT <- extract_log_lik(fit_NI_ImpT,c('log_lik'),merge_chains=FALSE)
loo_NI_ImpT <- loo(log_lik_NI_ImpT, r_eff=relative_eff(exp(log_lik_NI_ImpT)),cores=4,save_psis = TRUE)
print(loo_NI_ImpT)

save(NI_total_mobility_dat,fit_NI_ImpT,log_lik_NI_ImpT,loo_NI_ImpT,file='NI_ImpTotal.RData')

fit_E_ImpT <- stan(file = '../../../stan/Impnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_ImpT <- extract_log_lik(fit_E_ImpT,c('log_lik'),merge_chains=FALSE)
loo_E_ImpT <- loo(log_lik_E_ImpT, r_eff=relative_eff(exp(log_lik_E_ImpT)),cores=4,save_psis = TRUE)
print(loo_E_ImpT)

save(E_total_mobility_dat,fit_E_ImpT,log_lik_E_ImpT,loo_E_ImpT,file='E_ImpTotal.RData')




