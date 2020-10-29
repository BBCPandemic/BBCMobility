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

# Patch data sets to remove outlier LADS
source('../../DropLADSsubnat.R')


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




