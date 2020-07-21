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

fitCDOT <- stan(file = '../../../stan/CDOnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDOT <- extract_log_lik(fitCDOT,c('log_lik'),merge_chains=FALSE)
loo_CDOT <- loo(log_likCDOT, r_eff=relative_eff(exp(log_likCDOT)),cores=4,save_psis = TRUE)
print(loo_CDOT)


save(total_mobility_dat,fitCDOT,log_likCDOT,loo_CDOT,file='CDOTotal.RData')

fitCDOU <- stan(file = '../../../stan/CDOnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likCDOU <- extract_log_lik(fitCDOU,c('log_lik'),merge_chains=FALSE)
loo_CDOU <- loo(log_likCDOU, r_eff=relative_eff(exp(log_likCDOU)),cores=4,save_psis = TRUE)
print(loo_CDOU)


save(under18_mobility_dat,fitCDOU,log_likCDOU,loo_CDOU,file='CDOUnder18.RData')

fitCDOEd <- stan(file = '../../../stan/CDOnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDOEd <- extract_log_lik(fitCDOEd,c('log_lik'),merge_chains=FALSE)
loo_CDOEd <- loo(log_likCDOEd, r_eff=relative_eff(exp(log_likCDOEd)),cores=4,save_psis = TRUE)
print(loo_CDOEd)


save(fitCDOEd,log_likCDOEd,loo_CDOEd,file='CDOEducation.RData')


fitCDOEm <- stan(file = '../../../stan/CDOnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDOEm <- extract_log_lik(fitCDOEm,c('log_lik'),merge_chains=FALSE)
loo_CDOEm <- loo(log_likCDOEm, r_eff=relative_eff(exp(log_likCDOEm)),cores=4,save_psis = TRUE)
print(loo_CDOEm)


save(employed_mobility_dat,fitCDOEm,log_likCDOEm,loo_CDOEm,file='CDOEmployed.RData')


fitCDON <- stan(file = '../../../stan/CDOnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDON <- extract_log_lik(fitCDON,c('log_lik'),merge_chains=FALSE)
loo_CDON <- loo(log_likCDON, r_eff=relative_eff(exp(log_likCDON)),cores=4,save_psis = TRUE)
print(loo_CDON)

save(neet_mobility_dat,fitCDON,log_likCDON,loo_CDON,file='CDONEET.RData')


