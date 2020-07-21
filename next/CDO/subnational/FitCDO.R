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




