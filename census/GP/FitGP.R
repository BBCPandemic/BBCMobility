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


fitGP_CS <- stan(file = '../../stan/GPnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4)

log_likGP_CS   <- extract_log_lik(fitGP_CS  ,c('log_lik'),merge_chains=FALSE)
loo_GP_CS   <- loo(log_likGP_CS  , r_eff=relative_eff(exp(log_likGP_CS)),cores=4,save_psis = TRUE)
print(loo_GP_CS )

save(fitGP_CS,log_likGP_CS,loo_GP_CS,file='GP_SCensus.RData')

fitGP_CNI <- stan(file = '../../stan/GPnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likGP_CNI   <- extract_log_lik(fitGP_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_GP_CNI   <- loo(log_likGP_CNI  , r_eff=relative_eff(exp(log_likGP_CNI)),cores=4,save_psis = TRUE)
print(loo_GP_CNI)

save(fitGP_CNI,log_likGP_CNI,loo_GP_CNI,file='GP_NICensus.RData')


fitGP_CW <- stan(file = '../../stan/GPnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likGP_CW   <- extract_log_lik(fitGP_CW  ,c('log_lik'),merge_chains=FALSE)
loo_GP_CW   <- loo(log_likGP_CW  , r_eff=relative_eff(exp(log_likGP_CW)),cores=4,save_psis = TRUE)
print(loo_GP_CW)

save(fitGP_CW,log_likGP_CW,loo_GP_CW,file='GP_WCensus.RData')

fitGP_CE <- stan(file = '../../stan/GPnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likGP_CE   <- extract_log_lik(fitGP_CE  ,c('log_lik'),merge_chains=FALSE)
loo_GP_CE   <- loo(log_likGP_CE  , r_eff=relative_eff(exp(log_likGP_CE)),cores=4,save_psis = TRUE)
print(loo_GP_CE)

save(fitGP_CE,log_likGP_CE,loo_GP_CE,file='GP_ECensus.RData')
