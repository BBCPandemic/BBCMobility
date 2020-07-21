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


fitGO_CS <- stan(file = '../../stan/GOnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4,control=list(adapt_delta=0.9))

log_likGO_CS   <- extract_log_lik(fitGO_CS  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CS   <- loo(log_likGO_CS  , r_eff=relative_eff(exp(log_likGO_CS)),cores=4,save_psis = TRUE)
print(loo_GO_CS )

save(fitGO_CS,log_likGO_CS,loo_GO_CS,file='GO_SCensus.RData')

fitGO_CNI <- stan(file = '../../stan/GOnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likGO_CNI   <- extract_log_lik(fitGO_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CNI   <- loo(log_likGO_CNI  , r_eff=relative_eff(exp(log_likGO_CNI)),cores=4,save_psis = TRUE)
print(loo_GO_CNI)

save(fitGO_CNI,log_likGO_CNI,loo_GO_CNI,file='GO_NICensus.RData')


fitGO_CW <- stan(file = '../../stan/GOnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likGO_CW   <- extract_log_lik(fitGO_CW  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CW   <- loo(log_likGO_CW  , r_eff=relative_eff(exp(log_likGO_CW)),cores=4,save_psis = TRUE)
print(loo_GO_CW)

save(fitGO_CW,log_likGO_CW,loo_GO_CW,file='GO_WCensus.RData')


fitGO_CE <- stan(file = '../../stan/GOnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likGO_CE   <- extract_log_lik(fitGO_CE  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CE   <- loo(log_likGO_CE  , r_eff=relative_eff(exp(log_likGO_CE)),cores=4,save_psis = TRUE)
print(loo_GO_CE)

save(fitGO_CE,log_likGO_CE,loo_GO_CE,file='GO_ECensus.RData')
