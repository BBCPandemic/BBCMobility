require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_next.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSage.R')

fitCDEU <- stan(file = '../../../stan/CDEnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4)

log_likCDEU <- extract_log_lik(fitCDEU,c('log_lik'),merge_chains=FALSE)
loo_CDEU <- loo(log_likCDEU, r_eff=relative_eff(exp(log_likCDEU)),cores=4,save_psis = TRUE)
print(loo_CDEU)

save(under18_mobility_dat,fitCDEU,log_likCDEU,loo_CDEU,file='CDEUnder18.RData')

fitCDE18_30 <- stan(file = '../../../stan/CDEnegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4)

log_likCDE18_30 <- extract_log_lik(fitCDE18_30,c('log_lik'),merge_chains=FALSE)
loo_CDE18_30 <- loo(log_likCDE18_30, r_eff=relative_eff(exp(log_likCDE18_30)),cores=4,save_psis = TRUE)
print(loo_CDE18_30)

save(a18_30_mobility_dat,fitCDE18_30,log_likCDE18_30,loo_CDE18_30,file='CDE18_30.RData')

fitCDE30_60 <- stan(file = '../../../stan/CDEnegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDE30_60 <- extract_log_lik(fitCDE30_60,c('log_lik'),merge_chains=FALSE)
loo_CDE30_60 <- loo(log_likCDE30_60, r_eff=relative_eff(exp(log_likCDE30_60)),cores=4,save_psis = TRUE)
print(loo_CDE30_60)

save(a30_60_mobility_dat,fitCDE30_60,log_likCDE30_60,loo_CDE30_60,file='CDE30_60.RData')

fitCDE60_100 <- stan(file = '../../../stan/CDEnegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4)

log_likCDE60_100 <- extract_log_lik(fitCDE60_100,c('log_lik'),merge_chains=FALSE)
loo_CDE60_100 <- loo(log_likCDE60_100, r_eff=relative_eff(exp(log_likCDE60_100)),cores=4,save_psis = TRUE)
print(loo_CDE60_100)

save(a60_100_mobility_dat,fitCDE60_100,log_likCDE60_100,loo_CDE60_100,file='CDE60_100.RData')

