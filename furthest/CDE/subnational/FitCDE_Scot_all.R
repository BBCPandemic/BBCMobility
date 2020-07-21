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


fit_S_CDET_all <- stan(file = '../../../stan/CDEnegbin.stan', 
                   data = S_total_mobility_dat, 
                   iter = 4000, chains = 4)

log_lik_S_CDET_all <- extract_log_lik(fit_S_CDET_all,c('log_lik'),merge_chains=FALSE)
loo_S_CDET_all <- loo(log_lik_S_CDET_all, r_eff=relative_eff(exp(log_lik_S_CDET_all)),cores=4,save_psis = TRUE)
print(loo_S_CDET_all)

save(fit_S_CDET_all,log_lik_S_CDET_all,loo_S_CDET_all,file='S_CDETotal_all.RData')
