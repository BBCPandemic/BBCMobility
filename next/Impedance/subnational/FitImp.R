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




