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


