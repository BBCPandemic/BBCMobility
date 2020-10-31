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



fit_S_CDORT <- stan(file = '../../../stan/CDORnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_CDORT <- extract_log_lik(fit_S_CDORT,c('log_lik'),merge_chains=FALSE)
loo_S_CDORT <- loo(log_lik_S_CDORT, r_eff=relative_eff(exp(log_lik_S_CDORT)),cores=4,save_psis = TRUE)
print(loo_S_CDORT)

save(S_total_mobility_dat,fit_S_CDORT,log_lik_S_CDORT,loo_S_CDORT,file='S_CDORTotal.RData')

fit_W_CDORT <- stan(file = '../../../stan/CDORnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_CDORT <- extract_log_lik(fit_W_CDORT,c('log_lik'),merge_chains=FALSE)
loo_W_CDORT <- loo(log_lik_W_CDORT, r_eff=relative_eff(exp(log_lik_W_CDORT)),cores=4,save_psis = TRUE)
print(loo_W_CDORT)

save(W_total_mobility_dat,fit_W_CDORT,log_lik_W_CDORT,loo_W_CDORT,file='W_CDORTotal.RData')

fit_NI_CDORT <- stan(file = '../../../stan/CDORnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_CDORT <- extract_log_lik(fit_NI_CDORT,c('log_lik'),merge_chains=FALSE)
loo_NI_CDORT <- loo(log_lik_NI_CDORT, r_eff=relative_eff(exp(log_lik_NI_CDORT)),cores=4,save_psis = TRUE)
print(loo_NI_CDORT)

save(NI_total_mobility_dat,fit_NI_CDORT,log_lik_NI_CDORT,loo_NI_CDORT,file='NI_CDORTotal.RData')

fit_E_CDORT <- stan(file = '../../../stan/CDORnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_CDORT <- extract_log_lik(fit_E_CDORT,c('log_lik'),merge_chains=FALSE)
loo_E_CDORT <- loo(log_lik_E_CDORT, r_eff=relative_eff(exp(log_lik_E_CDORT)),cores=4,save_psis = TRUE)
print(loo_E_CDORT)

save(E_total_mobility_dat,fit_E_CDORT,log_lik_E_CDORT,loo_E_CDORT,file='E_CDORTotal.RData')




