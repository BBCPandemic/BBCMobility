require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

fitCDOT <- stan(file = '../../../stan/CDORnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDOT <- extract_log_lik(fitCDOT,c('log_lik'),merge_chains=FALSE)
loo_CDOT <- loo(log_likCDOT, r_eff=relative_eff(exp(log_likCDOT)),cores=4,save_psis = TRUE)
print(loo_CDOT)


save(total_mobility_dat,fitCDOT,log_likCDOT,loo_CDOT,file='CDOTotal.RData')

fitCDOU <- stan(file = '../../../stan/CDORnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likCDOU <- extract_log_lik(fitCDOU,c('log_lik'),merge_chains=FALSE)
loo_CDOU <- loo(log_likCDOU, r_eff=relative_eff(exp(log_likCDOU)),cores=4,save_psis = TRUE)
print(loo_CDOU)


save(under18_mobility_dat,fitCDOU,log_likCDOU,loo_CDOU,file='CDOUnder18.RData')

fitCDOEd <- stan(file = '../../../stan/CDORnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDOEd <- extract_log_lik(fitCDOEd,c('log_lik'),merge_chains=FALSE)
loo_CDOEd <- loo(log_likCDOEd, r_eff=relative_eff(exp(log_likCDOEd)),cores=4,save_psis = TRUE)
print(loo_CDOEd)


save(fitCDOEd,log_likCDOEd,loo_CDOEd,file='CDOEducation.RData')


fitCDOEm <- stan(file = '../../../stan/CDORnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDOEm <- extract_log_lik(fitCDOEm,c('log_lik'),merge_chains=FALSE)
loo_CDOEm <- loo(log_likCDOEm, r_eff=relative_eff(exp(log_likCDOEm)),cores=4,save_psis = TRUE)
print(loo_CDOEm)


save(employed_mobility_dat,fitCDOEm,log_likCDOEm,loo_CDOEm,file='CDOEmployed.RData')


fitCDON <- stan(file = '../../../stan/CDORnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDON <- extract_log_lik(fitCDON,c('log_lik'),merge_chains=FALSE)
loo_CDON <- loo(log_likCDON, r_eff=relative_eff(exp(log_likCDON)),cores=4,save_psis = TRUE)
print(loo_CDON)

save(neet_mobility_dat,fitCDON,log_likCDON,loo_CDON,file='CDONEET.RData')


