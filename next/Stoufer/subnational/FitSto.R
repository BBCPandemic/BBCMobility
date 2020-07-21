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




