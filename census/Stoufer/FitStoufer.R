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
scotland_census_mobility_dat$s = scotland_census_mobility_dat$s[-c(9),-c(9)]
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
Ecensus_mobility_dat$s = Ecensus_mobility_dat$s[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$no_patches = length(Ecensus_mobility_dat$N)
Ecensus_mobility_dat$L = array(c(-1))
Ecensus_mobility_dat$Lno = 0

Ecensus_mobility_dat$non_zero = Ecensus_mobility_dat$flux>0
Ecensus_mobility_dat$no_non_zero = sum(Ecensus_mobility_dat$non_zero)


fitStoufer_CS <- stan(file = '../../stan/Stoufernegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4)

log_likStoufer_CS   <- extract_log_lik(fitStoufer_CS  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CS   <- loo(log_likStoufer_CS  , r_eff=relative_eff(exp(log_likStoufer_CS)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CS )

save(fitStoufer_CS,log_likStoufer_CS,loo_Stoufer_CS,file='Sto_CSCensus.RData')

fitStoufer_CNI <- stan(file = '../../stan/Stoufernegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likStoufer_CNI   <- extract_log_lik(fitStoufer_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CNI   <- loo(log_likStoufer_CNI  , r_eff=relative_eff(exp(log_likStoufer_CNI)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CNI)

save(fitStoufer_CNI,log_likStoufer_CNI,loo_Stoufer_CNI,file='Sto_NICensus.RData')


fitStoufer_CW <- stan(file = '../../stan/Stoufernegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likStoufer_CW   <- extract_log_lik(fitStoufer_CW  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CW   <- loo(log_likStoufer_CW  , r_eff=relative_eff(exp(log_likStoufer_CW)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CW)

save(fitStoufer_CW,log_likStoufer_CW,loo_Stoufer_CW,file='Sto_WCensus.RData')

fitStoufer_CE <- stan(file = '../../stan/Stoufernegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likStoufer_CE   <- extract_log_lik(fitStoufer_CE  ,c('log_lik'),merge_chains=FALSE)
loo_Stoufer_CE   <- loo(log_likStoufer_CE  , r_eff=relative_eff(exp(log_likStoufer_CE)),cores=4,save_psis = TRUE)
print(loo_Stoufer_CE)

save(fitStoufer_CE,log_likStoufer_CE,loo_Stoufer_CE,file='Sto_ECensus.RData')