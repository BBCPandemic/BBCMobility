require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

fitCDORT <- stan(file = '../../../stan/CDORnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDORT <- extract_log_lik(fitCDORT,c('log_lik'),merge_chains=FALSE)
loo_CDORT <- loo(log_likCDORT, r_eff=relative_eff(exp(log_likCDORT)),cores=4,save_psis = TRUE)
print(loo_CDORT)


save(total_mobility_dat,fitCDORT,log_likCDORT,loo_CDORT,file='CDORTotal.RData')

fitCDORU <- stan(file = '../../../stan/CDORnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likCDORU <- extract_log_lik(fitCDORU,c('log_lik'),merge_chains=FALSE)
loo_CDORU <- loo(log_likCDORU, r_eff=relative_eff(exp(log_likCDORU)),cores=4,save_psis = TRUE)
print(loo_CDORU)


save(under18_mobility_dat,fitCDORU,log_likCDORU,loo_CDORU,file='CDORUnder18.RData')

fitCDOREd <- stan(file = '../../../stan/CDORnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDOREd <- extract_log_lik(fitCDOREd,c('log_lik'),merge_chains=FALSE)
loo_CDOREd <- loo(log_likCDOREd, r_eff=relative_eff(exp(log_likCDOREd)),cores=4,save_psis = TRUE)
print(loo_CDOREd)


save(fitCDOREd,log_likCDOREd,loo_CDOREd,file='CDOREducation.RData')


fitCDOREm <- stan(file = '../../../stan/CDORnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDOREm <- extract_log_lik(fitCDOREm,c('log_lik'),merge_chains=FALSE)
loo_CDOREm <- loo(log_likCDOREm, r_eff=relative_eff(exp(log_likCDOREm)),cores=4,save_psis = TRUE)
print(loo_CDOREm)


save(employed_mobility_dat,fitCDOREm,log_likCDOREm,loo_CDOREm,file='CDOREmployed.RData')


fitCDORN <- stan(file = '../../../stan/CDORnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDORN <- extract_log_lik(fitCDORN,c('log_lik'),merge_chains=FALSE)
loo_CDORN <- loo(log_likCDORN, r_eff=relative_eff(exp(log_likCDORN)),cores=4,save_psis = TRUE)
print(loo_CDORN)

save(neet_mobility_dat,fitCDORN,log_likCDORN,loo_CDORN,file='CDORNEET.RData')


