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

fitCDPT <- stan(file = '../../../stan/CDPnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDPT <- extract_log_lik(fitCDPT,c('log_lik'),merge_chains=FALSE)
loo_CDPT <- loo(log_likCDPT, r_eff=relative_eff(exp(log_likCDPT)),cores=4,save_psis = TRUE)
print(loo_CDPT)


save(total_mobility_dat,fitCDPT,log_likCDPT,loo_CDPT,file='CDPTotal.RData')

fitCDPU <- stan(file = '../../../stan/CDPnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4)

log_likCDPU <- extract_log_lik(fitCDPU,c('log_lik'),merge_chains=FALSE)
loo_CDPU <- loo(log_likCDPU, r_eff=relative_eff(exp(log_likCDPU)),cores=4,save_psis = TRUE)
print(loo_CDPU)


save(under18_mobility_dat,fitCDPU,log_likCDPU,loo_CDPU,file='CDPUnder18.RData')

fitCDPEd <- stan(file = '../../../stan/CDPnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4)

log_likCDPEd <- extract_log_lik(fitCDPEd,c('log_lik'),merge_chains=FALSE)
loo_CDPEd <- loo(log_likCDPEd, r_eff=relative_eff(exp(log_likCDPEd)),cores=4,save_psis = TRUE)
print(loo_CDPEd)


save(fitCDPEd,log_likCDPEd,loo_CDPEd,file='CDPEducation.RData')


fitCDPEm <- stan(file = '../../../stan/CDPnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 4000, chains = 4)

log_likCDPEm <- extract_log_lik(fitCDPEm,c('log_lik'),merge_chains=FALSE)
loo_CDPEm <- loo(log_likCDPEm, r_eff=relative_eff(exp(log_likCDPEm)),cores=4,save_psis = TRUE)
print(loo_CDPEm)


save(employed_mobility_dat,fitCDPEm,log_likCDPEm,loo_CDPEm,file='CDPEmployed.RData')


fitCDPN <- stan(file = '../../../stan/CDPnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDPN <- extract_log_lik(fitCDPN,c('log_lik'),merge_chains=FALSE)
loo_CDPN <- loo(log_likCDPN, r_eff=relative_eff(exp(log_likCDPN)),cores=4,save_psis = TRUE)
print(loo_CDPN)

save(neet_mobility_dat,fitCDPN,log_likCDPN,loo_CDPN,file='CDPNEET.RData')


