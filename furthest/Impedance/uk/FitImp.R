require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_furthest.RData')

total_mobility_dat$N = total_mobility_dat$N[-c(234,243,294,326,346)]
total_mobility_dat$flux = total_mobility_dat$flux[-c(234,243,294,326,346)]
total_mobility_dat$A = total_mobility_dat$A[-c(234,243,294,326,346)]
total_mobility_dat$mv = total_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
total_mobility_dat$r = total_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
total_mobility_dat$s = total_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
total_mobility_dat$no_patches = length(total_mobility_dat$N)
total_mobility_dat$L = array(c(-1))
total_mobility_dat$Lno = 0

total_mobility_dat$non_zero = total_mobility_dat$flux>0
total_mobility_dat$no_non_zero = sum(total_mobility_dat$non_zero)

under18_mobility_dat$N = under18_mobility_dat$N[-c(234,243,294,326,346)]
under18_mobility_dat$flux = under18_mobility_dat$flux[-c(234,243,294,326,346)]
under18_mobility_dat$A = under18_mobility_dat$A[-c(234,243,294,326,346)]
under18_mobility_dat$mv = under18_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
under18_mobility_dat$r = under18_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
under18_mobility_dat$s = under18_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
under18_mobility_dat$no_patches = length(under18_mobility_dat$N)
under18_mobility_dat$L = array(c(-1))
under18_mobility_dat$Lno = 0

under18_mobility_dat$non_zero = under18_mobility_dat$flux>0
under18_mobility_dat$no_non_zero = sum(under18_mobility_dat$non_zero)

education_mobility_dat$N = education_mobility_dat$N[-c(234,243,294,326,346)]
education_mobility_dat$flux = education_mobility_dat$flux[-c(234,243,294,326,346)]
education_mobility_dat$A = education_mobility_dat$A[-c(234,243,294,326,346)]
education_mobility_dat$mv = education_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
education_mobility_dat$r = education_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
education_mobility_dat$s = education_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
education_mobility_dat$no_patches = length(education_mobility_dat$N)
education_mobility_dat$L = array(c(-1))
education_mobility_dat$Lno = 0

education_mobility_dat$non_zero = education_mobility_dat$flux>0
education_mobility_dat$no_non_zero = sum(education_mobility_dat$non_zero)

employed_mobility_dat$N = employed_mobility_dat$N[-c(234,243,294,326,346)]
employed_mobility_dat$flux = employed_mobility_dat$flux[-c(234,243,294,326,346)]
employed_mobility_dat$A = employed_mobility_dat$A[-c(234,243,294,326,346)]
employed_mobility_dat$mv = employed_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
employed_mobility_dat$r = employed_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
employed_mobility_dat$s = employed_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
employed_mobility_dat$no_patches = length(employed_mobility_dat$N)
employed_mobility_dat$L = array(c(-1))
employed_mobility_dat$Lno = 0

employed_mobility_dat$non_zero = employed_mobility_dat$flux>0
employed_mobility_dat$no_non_zero = sum(employed_mobility_dat$non_zero)

neet_mobility_dat$N = neet_mobility_dat$N[-c(234,243,294,326,346)]
neet_mobility_dat$flux = neet_mobility_dat$flux[-c(234,243,294,326,346)]
neet_mobility_dat$A = neet_mobility_dat$A[-c(234,243,294,326,346)]
neet_mobility_dat$mv = neet_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
neet_mobility_dat$r = neet_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
neet_mobility_dat$s = neet_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
neet_mobility_dat$no_patches = length(neet_mobility_dat$N)
neet_mobility_dat$L = array(c(-1))
neet_mobility_dat$Lno = 0

neet_mobility_dat$non_zero = neet_mobility_dat$flux>0
neet_mobility_dat$no_non_zero = sum(neet_mobility_dat$non_zero)

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


