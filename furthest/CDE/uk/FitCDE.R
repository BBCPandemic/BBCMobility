require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

fitCDET <- stan(file = '../../../stan/CDEnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDET <- extract_log_lik(fitCDET,c('log_lik'),merge_chains=FALSE)
loo_CDET <- loo(log_likCDET, r_eff=relative_eff(exp(log_likCDET)),cores=4,save_psis = TRUE)
print(loo_CDET)


save(total_mobility_dat,fitCDET,log_likCDET,loo_CDET,file='CDETotal.RData')

fitCDEU <- stan(file = '../../../stan/CDEnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likCDEU <- extract_log_lik(fitCDEU,c('log_lik'),merge_chains=FALSE)
loo_CDEU <- loo(log_likCDEU, r_eff=relative_eff(exp(log_likCDEU)),cores=4,save_psis = TRUE)
print(loo_CDEU)


save(under18_mobility_dat,fitCDEU,log_likCDEU,loo_CDEU,file='CDEUnder18.RData')

fitCDEEd <- stan(file = '../../../stan/CDEnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDEEd <- extract_log_lik(fitCDEEd,c('log_lik'),merge_chains=FALSE)
loo_CDEEd <- loo(log_likCDEEd, r_eff=relative_eff(exp(log_likCDEEd)),cores=4,save_psis = TRUE)
print(loo_CDEEd)


save(fitCDEEd,log_likCDEEd,loo_CDEEd,file='CDEEducation.RData')


fitCDEEm <- stan(file = '../../../stan/CDEnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDEEm <- extract_log_lik(fitCDEEm,c('log_lik'),merge_chains=FALSE)
loo_CDEEm <- loo(log_likCDEEm, r_eff=relative_eff(exp(log_likCDEEm)),cores=4,save_psis = TRUE)
print(loo_CDEEm)


save(employed_mobility_dat,fitCDEEm,log_likCDEEm,loo_CDEEm,file='CDEEmployed.RData')


fitCDEN <- stan(file = '../../../stan/CDEnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDEN <- extract_log_lik(fitCDEN,c('log_lik'),merge_chains=FALSE)
loo_CDEN <- loo(log_likCDEN, r_eff=relative_eff(exp(log_likCDEN)),cores=4,save_psis = TRUE)
print(loo_CDEN)

save(neet_mobility_dat,fitCDEN,log_likCDEN,loo_CDEN,file='CDENEET.RData')


