require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_furthest.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSuk.R')

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


