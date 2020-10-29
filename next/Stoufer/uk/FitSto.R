require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

fitStoT <- stan(file = '../../../stan/Stoufernegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likStoT <- extract_log_lik(fitStoT,c('log_lik'),merge_chains=FALSE)
loo_StoT <- loo(log_likStoT, r_eff=relative_eff(exp(log_likStoT)),cores=4,save_psis = TRUE)
print(loo_StoT)


save(total_mobility_dat,fitStoT,log_likStoT,loo_StoT,file='StoTotal.RData')

fitStoU <- stan(file = '../../../stan/Stoufernegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likStoU <- extract_log_lik(fitStoU,c('log_lik'),merge_chains=FALSE)
loo_StoU <- loo(log_likStoU, r_eff=relative_eff(exp(log_likStoU)),cores=4,save_psis = TRUE)
print(loo_StoU)


save(under18_mobility_dat,fitStoU,log_likStoU,loo_StoU,file='StoUnder18.RData')

fitStoEd <- stan(file = '../../../stan/Stoufernegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likStoEd <- extract_log_lik(fitStoEd,c('log_lik'),merge_chains=FALSE)
loo_StoEd <- loo(log_likStoEd, r_eff=relative_eff(exp(log_likStoEd)),cores=4,save_psis = TRUE)
print(loo_StoEd)


save(fitStoEd,log_likStoEd,loo_StoEd,file='StoEducation.RData')


fitStoEm <- stan(file = '../../../stan/Stoufernegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likStoEm <- extract_log_lik(fitStoEm,c('log_lik'),merge_chains=FALSE)
loo_StoEm <- loo(log_likStoEm, r_eff=relative_eff(exp(log_likStoEm)),cores=4,save_psis = TRUE)
print(loo_StoEm)


save(employed_mobility_dat,fitStoEm,log_likStoEm,loo_StoEm,file='StoEmployed.RData')


fitStoN <- stan(file = '../../../stan/Stoufernegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likStoN <- extract_log_lik(fitStoN,c('log_lik'),merge_chains=FALSE)
loo_StoN <- loo(log_likStoN, r_eff=relative_eff(exp(log_likStoN)),cores=4,save_psis = TRUE)
print(loo_StoN)

save(neet_mobility_dat,fitStoN,log_likStoN,loo_StoN,file='StoNEET.RData')


