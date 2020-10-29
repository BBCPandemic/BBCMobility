require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

fitERadT <- stan(file = '../../../stan/ERadnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likERadT <- extract_log_lik(fitERadT,c('log_lik'),merge_chains=FALSE)
loo_ERadT <- loo(log_likERadT, r_eff=relative_eff(exp(log_likERadT)),cores=4,save_psis = TRUE)
print(loo_ERadT)


save(total_mobility_dat,fitERadT,log_likERadT,loo_ERadT,file='ERadTotal.RData')

fitERadU <- stan(file = '../../../stan/ERadnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likERadU <- extract_log_lik(fitERadU,c('log_lik'),merge_chains=FALSE)
loo_ERadU <- loo(log_likERadU, r_eff=relative_eff(exp(log_likERadU)),cores=4,save_psis = TRUE)
print(loo_ERadU)


save(under18_mobility_dat,fitERadU,log_likERadU,loo_ERadU,file='ERadUnder18.RData')

fitERadEd <- stan(file = '../../../stan/ERadnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likERadEd <- extract_log_lik(fitERadEd,c('log_lik'),merge_chains=FALSE)
loo_ERadEd <- loo(log_likERadEd, r_eff=relative_eff(exp(log_likERadEd)),cores=4,save_psis = TRUE)
print(loo_ERadEd)


save(fitERadEd,log_likERadEd,loo_ERadEd,file='ERadEducation.RData')


fitERadEm <- stan(file = '../../../stan/ERadnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likERadEm <- extract_log_lik(fitERadEm,c('log_lik'),merge_chains=FALSE)
loo_ERadEm <- loo(log_likERadEm, r_eff=relative_eff(exp(log_likERadEm)),cores=4,save_psis = TRUE)
print(loo_ERadEm)


save(employed_mobility_dat,fitERadEm,log_likERadEm,loo_ERadEm,file='ERadEmployed.RData')


fitERadN <- stan(file = '../../../stan/ERadnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likERadN <- extract_log_lik(fitERadN,c('log_lik'),merge_chains=FALSE)
loo_ERadN <- loo(log_likERadN, r_eff=relative_eff(exp(log_likERadN)),cores=4,save_psis = TRUE)
print(loo_ERadN)

save(neet_mobility_dat,fitERadN,log_likERadN,loo_ERadN,file='ERadNEET.RData')


