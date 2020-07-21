require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
load('../../flux/census_flux.RData')

scotland_census_mobility_dat$N = scotland_census_mobility_dat$N[-c(9)]
scotland_census_mobility_dat$flux = scotland_census_mobility_dat$flux[-c(9)]
scotland_census_mobility_dat$A = scotland_census_mobility_dat$A[-c(9)]
scotland_census_mobility_dat$mv = scotland_census_mobility_dat$mv[-c(9),-c(9)]
scotland_census_mobility_dat$r = scotland_census_mobility_dat$r[-c(9),-c(9)]
scotland_census_mobility_dat$no_patches = length(scotland_census_mobility_dat$N)
scotland_census_mobility_dat$L = array(c(-1))
scotland_census_mobility_dat$Lno = 0

scotland_census_mobility_dat$non_zero = scotland_census_mobility_dat$flux>0
scotland_census_mobility_dat$no_non_zero = sum(scotland_census_mobility_dat$non_zero)

Ecensus_mobility_dat$N = Ecensus_mobility_dat$N[-c(294,326)]
Ecensus_mobility_dat$flux = Ecensus_mobility_dat$flux[-c(294,326)]
Ecensus_mobility_dat$A = Ecensus_mobility_dat$A[-c(294,326)]
Ecensus_mobility_dat$mv = Ecensus_mobility_dat$mv[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$r = Ecensus_mobility_dat$r[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$no_patches = length(Ecensus_mobility_dat$N)
Ecensus_mobility_dat$L = array(c(-1))
Ecensus_mobility_dat$Lno = 0

Ecensus_mobility_dat$non_zero = Ecensus_mobility_dat$flux>0
Ecensus_mobility_dat$no_non_zero = sum(Ecensus_mobility_dat$non_zero)

fitGE_CS <- stan(file = '../../stan/GEnegbin.stan',
                    data = scotland_census_mobility_dat,
                    iter = 4000, chains = 4)
log_likGE_CS   <- extract_log_lik(fitGE_CS  ,c('log_lik'),merge_chains=FALSE)
loo_GE_CS   <- loo(log_likGE_CS  , r_eff=relative_eff(exp(log_likGE_CS)),cores=4,save_psis = TRUE)
print(loo_GE_CS)

save(fitGE_CS,log_likGE_CS,loo_GE_CS,file='GE_SCensus.RData')

fitGE_CNI <- stan(file = '../../stan/GEnegbin.stan',
                         data = NIcensus_mobility_dat,
                         iter = 2000, chains = 4)

log_likGE_CNI   <- extract_log_lik(fitGE_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_GE_CNI   <- loo(log_likGE_CNI  , r_eff=relative_eff(exp(log_likGE_CNI)),cores=4,save_psis = TRUE)
print(loo_GE_CNI)
save(fitGE_CNI,log_likGE_CNI,loo_GE_CNI,file='GE_NICensus.RData')

fitGE_CW <- stan(file = '../../stan/GEnegbin.stan',
                   data = Wcensus_mobility_dat,
                   iter = 2000, chains = 4)

log_likGE_CW   <- extract_log_lik(fitGE_CW  ,c('log_lik'),merge_chains=FALSE)
loo_GE_CW   <- loo(log_likGE_CW  , r_eff=relative_eff(exp(log_likGE_CW)),cores=4,save_psis = TRUE)
print(loo_GE_CW)

save(fitGE_CW,log_likGE_CW,loo_GE_CW,file='GE_WCensus.RData')

fitGE_CE <- stan(file = '../../stan/GEnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likGE_CE   <- extract_log_lik(fitGE_CE  ,c('log_lik'),merge_chains=FALSE)
loo_GE_CE   <- loo(log_likGE_CE  , r_eff=relative_eff(exp(log_likGE_CE)),cores=4,save_psis = TRUE)
print(loo_GE_CE)

save(fitGE_CE,log_likGE_CE,loo_GE_CE,file='GE_ECensus.RData')
