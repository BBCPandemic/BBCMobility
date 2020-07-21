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

# Remove Highland (9)

S_total_mobility_dat$N = S_total_mobility_dat$N[-c(9)]
S_total_mobility_dat$flux = S_total_mobility_dat$flux[-c(9)]
S_total_mobility_dat$A = S_total_mobility_dat$A[-c(9)]
S_total_mobility_dat$mv = S_total_mobility_dat$mv[-c(9),-c(9)]
S_total_mobility_dat$r = S_total_mobility_dat$r[-c(9),-c(9)]
S_total_mobility_dat$s = S_total_mobility_dat$s[-c(9),-c(9)]
S_total_mobility_dat$no_patches = length(S_total_mobility_dat$N)
S_total_mobility_dat$L = array(c(-1))
S_total_mobility_dat$Lno = 0

S_total_mobility_dat$non_zero = S_total_mobility_dat$flux>0
S_total_mobility_dat$no_non_zero = sum(S_total_mobility_dat$non_zero)

# Remove Westminster / City of London

E_total_mobility_dat$N = E_total_mobility_dat$N[-c(294,326)]
E_total_mobility_dat$flux = E_total_mobility_dat$flux[-c(294,326)]
E_total_mobility_dat$A = E_total_mobility_dat$A[-c(294,326)]
E_total_mobility_dat$mv = E_total_mobility_dat$mv[-c(294,326),-c(294,326)]
E_total_mobility_dat$r = E_total_mobility_dat$r[-c(294,326),-c(294,326)]
E_total_mobility_dat$s = E_total_mobility_dat$s[-c(294,326),-c(294,326)]
E_total_mobility_dat$no_patches = length(E_total_mobility_dat$N)
E_total_mobility_dat$L = array(c(-1))
E_total_mobility_dat$Lno = 0

E_total_mobility_dat$non_zero = E_total_mobility_dat$flux>0
E_total_mobility_dat$no_non_zero = sum(E_total_mobility_dat$non_zero)


fit_S_CDET <- stan(file = '../../../stan/CDEnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4)

log_lik_S_CDET <- extract_log_lik(fit_S_CDET,c('log_lik'),merge_chains=FALSE)
loo_S_CDET <- loo(log_lik_S_CDET, r_eff=relative_eff(exp(log_lik_S_CDET)),cores=4,save_psis = TRUE)
print(loo_S_CDET)

save(S_total_mobility_dat,fit_S_CDET,log_lik_S_CDET,loo_S_CDET,file='S_CDETotal.RData')

fit_W_CDET <- stan(file = '../../../stan/CDEnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_W_CDET <- extract_log_lik(fit_W_CDET,c('log_lik'),merge_chains=FALSE)
loo_W_CDET <- loo(log_lik_W_CDET, r_eff=relative_eff(exp(log_lik_W_CDET)),cores=4,save_psis = TRUE)
print(loo_W_CDET)

save(W_total_mobility_dat,fit_W_CDET,log_lik_W_CDET,loo_W_CDET,file='W_CDETotal.RData')

fit_NI_CDET <- stan(file = '../../../stan/CDEnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4)

log_lik_NI_CDET <- extract_log_lik(fit_NI_CDET,c('log_lik'),merge_chains=FALSE)
loo_NI_CDET <- loo(log_lik_NI_CDET, r_eff=relative_eff(exp(log_lik_NI_CDET)),cores=4,save_psis = TRUE)
print(loo_NI_CDET)

save(NI_total_mobility_dat,fit_NI_CDET,log_lik_NI_CDET,loo_NI_CDET,file='NI_CDETotal.RData')

fit_E_CDET <- stan(file = '../../../stan/CDEnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4)

log_lik_E_CDET <- extract_log_lik(fit_E_CDET,c('log_lik'),merge_chains=FALSE)
loo_E_CDET <- loo(log_lik_E_CDET, r_eff=relative_eff(exp(log_lik_E_CDET)),cores=4,save_psis = TRUE)
print(loo_E_CDET)

save(E_total_mobility_dat,fit_E_CDET,log_lik_E_CDET,loo_E_CDET,file='E_CDETotal.RData')




