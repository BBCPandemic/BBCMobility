require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(truncdist)

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


fit_S_IOT <- stan(file = '../../../stan/IOnegbin.stan', 
               data = S_total_mobility_dat, 
               iter = 4000, chains = 4,
               init=function()
               {list(gamma=runif(1,0,1e-6),
                     phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                     eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_lik_S_IOT <- extract_log_lik(fit_S_IOT,c('log_lik'),merge_chains=FALSE)
loo_S_IOT <- loo(log_lik_S_IOT, r_eff=relative_eff(exp(log_lik_S_IOT)),cores=4,save_psis = TRUE)
print(loo_S_IOT)

save(S_total_mobility_dat,fit_S_IOT,log_lik_S_IOT,loo_S_IOT,file='S_IOTotal.RData')

fit_W_IOT <- stan(file = '../../../stan/IOnegbin.stan', 
                   data = W_total_mobility_dat, 
                   iter = 2000, chains = 4,
                  init=function()
                  {list(gamma=runif(1,0,1e-6),
                        phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                        eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_lik_W_IOT <- extract_log_lik(fit_W_IOT,c('log_lik'),merge_chains=FALSE)
loo_W_IOT <- loo(log_lik_W_IOT, r_eff=relative_eff(exp(log_lik_W_IOT)),cores=4,save_psis = TRUE)
print(loo_W_IOT)

save(W_total_mobility_dat,fit_W_IOT,log_lik_W_IOT,loo_W_IOT,file='W_IOTotal.RData')

fit_NI_IOT <- stan(file = '../../../stan/IOnegbin.stan', 
                   data = NI_total_mobility_dat, 
                   iter = 2000, chains = 4,
                   init=function()
                   {list(gamma=runif(1,0,1e-6),
                         phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                         eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_lik_NI_IOT <- extract_log_lik(fit_NI_IOT,c('log_lik'),merge_chains=FALSE)
loo_NI_IOT <- loo(log_lik_NI_IOT, r_eff=relative_eff(exp(log_lik_NI_IOT)),cores=4,save_psis = TRUE)
print(loo_NI_IOT)

save(NI_total_mobility_dat,fit_NI_IOT,log_lik_NI_IOT,loo_NI_IOT,file='NI_IOTotal.RData')

fit_E_IOT <- stan(file = '../../../stan/IOnegbin.stan', 
                    data = E_total_mobility_dat, 
                    iter = 2000, chains = 4,
                  init=function()
                  {list(gamma=runif(1,0,1e-6),
                        phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                        eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_lik_E_IOT <- extract_log_lik(fit_E_IOT,c('log_lik'),merge_chains=FALSE)
loo_E_IOT <- loo(log_lik_E_IOT, r_eff=relative_eff(exp(log_lik_E_IOT)),cores=4,save_psis = TRUE)
print(loo_E_IOT)

save(E_total_mobility_dat,fit_E_IOT,log_lik_E_IOT,loo_E_IOT,file='E_IOTotal.RData')




