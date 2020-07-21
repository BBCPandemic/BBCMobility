require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_next.RData')

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


