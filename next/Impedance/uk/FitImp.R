require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

fitImpT <- stan(file = '../../../stan/Impnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likImpT <- extract_log_lik(fitImpT,c('log_lik'),merge_chains=FALSE)
loo_ImpT <- loo(log_likImpT, r_eff=relative_eff(exp(log_likImpT)),cores=4,save_psis = TRUE)
print(loo_ImpT)


save(total_mobility_dat,fitImpT,log_likImpT,loo_ImpT,file='ImpTotal.RData')

fitImpU <- stan(file = '../../../stan/Impnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likImpU <- extract_log_lik(fitImpU,c('log_lik'),merge_chains=FALSE)
loo_ImpU <- loo(log_likImpU, r_eff=relative_eff(exp(log_likImpU)),cores=4,save_psis = TRUE)
print(loo_ImpU)


save(under18_mobility_dat,fitImpU,log_likImpU,loo_ImpU,file='ImpUnder18.RData')

fitImpEd <- stan(file = '../../../stan/Impnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likImpEd <- extract_log_lik(fitImpEd,c('log_lik'),merge_chains=FALSE)
loo_ImpEd <- loo(log_likImpEd, r_eff=relative_eff(exp(log_likImpEd)),cores=4,save_psis = TRUE)
print(loo_ImpEd)


save(fitImpEd,log_likImpEd,loo_ImpEd,file='ImpEducation.RData')


fitImpEm <- stan(file = '../../../stan/Impnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likImpEm <- extract_log_lik(fitImpEm,c('log_lik'),merge_chains=FALSE)
loo_ImpEm <- loo(log_likImpEm, r_eff=relative_eff(exp(log_likImpEm)),cores=4,save_psis = TRUE)
print(loo_ImpEm)


save(employed_mobility_dat,fitImpEm,log_likImpEm,loo_ImpEm,file='ImpEmployed.RData')


fitImpN <- stan(file = '../../../stan/Impnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likImpN <- extract_log_lik(fitImpN,c('log_lik'),merge_chains=FALSE)
loo_ImpN <- loo(log_likImpN, r_eff=relative_eff(exp(log_likImpN)),cores=4,save_psis = TRUE)
print(loo_ImpN)

save(neet_mobility_dat,fitImpN,log_likImpN,loo_ImpN,file='ImpNEET.RData')


