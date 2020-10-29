require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../DropLADSCensus.R')

fitCDE_CS <- stan(file = '../../stan/CDEnegbin.stan',
                    data = scotland_census_mobility_dat,
                    iter = 4000, chains = 4)
stan_hist(fitCDE_CS,pars=c('tau','rho','delta','phi'))
log_likCDE_CS   <- extract_log_lik(fitCDE_CS  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CS   <- loo(log_likCDE_CS  , r_eff=relative_eff(exp(log_likCDE_CS)),cores=4,save_psis = TRUE)
print(loo_CDE_CS)

save(fitCDE_CS,log_likCDE_CS,loo_CDE_CS,file='CDE_SCensus.RData')

fitCDE_CNI <- stan(file = '../../stan/CDEnegbin.stan',
                         data = NIcensus_mobility_dat,
                         iter = 2000, chains = 4)

log_likCDE_CNI   <- extract_log_lik(fitCDE_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CNI   <- loo(log_likCDE_CNI  , r_eff=relative_eff(exp(log_likCDE_CNI)),cores=4,save_psis = TRUE)
print(loo_CDE_CNI)
save(fitCDE_CNI,log_likCDE_CNI,loo_CDE_CNI,file='CDE_NICensus.RData')

fitCDE_CW <- stan(file = '../../stan/CDEnegbin.stan',
                   data = Wcensus_mobility_dat,
                   iter = 2000, chains = 4)

log_likCDE_CW   <- extract_log_lik(fitCDE_CW  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CW   <- loo(log_likCDE_CW  , r_eff=relative_eff(exp(log_likCDE_CW)),cores=4,save_psis = TRUE)
print(loo_CDE_CW)

save(fitCDE_CW,log_likCDE_CW,loo_CDE_CW,file='CDE_WCensus.RData')

fitCDE_CE <- stan(file = '../../stan/CDEnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likCDE_CE   <- extract_log_lik(fitCDE_CE  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CE   <- loo(log_likCDE_CE  , r_eff=relative_eff(exp(log_likCDE_CE)),cores=4,save_psis = TRUE)
print(loo_CDE_CE)
stan_hist(fitCDE_CE,pars=c('tau','rho','delta','phi'))

save(fitCDE_CE,log_likCDE_CE,loo_CDE_CE,file='CDE_ECensus.RData')
