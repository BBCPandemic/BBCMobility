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


