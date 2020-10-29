require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(truncdist)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSCensus.R')

fitIO_CS <- stan(file = '../../stan/IOnegbin.stan',
                         data = scotland_census_mobility_dat,
                         iter = 4000, chains = 4)

log_likIO_CS   <- extract_log_lik(fitIO_CS  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CS   <- loo(log_likIO_CS  , r_eff=relative_eff(exp(log_likIO_CS)),cores=4,save_psis = TRUE)
print(loo_IO_CS )

save(fitIO_CS,log_likIO_CS,loo_IO_CS,file='IO_CSCensus.RData')

fitIO_CNI <- stan(file = '../../stan/IOnegbin.stan',
                   data = NIcensus_mobility_dat,
                   iter = 4000, chains = 4)

log_likIO_CNI   <- extract_log_lik(fitIO_CNI  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CNI   <- loo(log_likIO_CNI  , r_eff=relative_eff(exp(log_likIO_CNI)),cores=4,save_psis = TRUE)
print(loo_IO_CNI)

save(fitIO_CNI,log_likIO_CNI,loo_IO_CNI,file='IO_NICensus.RData')


fitIO_CW <- stan(file = '../../stan/IOnegbin.stan',
                  data = Wcensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likIO_CW   <- extract_log_lik(fitIO_CW  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CW   <- loo(log_likIO_CW  , r_eff=relative_eff(exp(log_likIO_CW)),cores=4,save_psis = TRUE)
print(loo_IO_CW)

save(fitIO_CW,log_likIO_CW,loo_IO_CW,file='IO_WCensus.RData')

fitIO_CE <- stan(file = '../../stan/IOnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4,
                 init=function()
                 {list(gamma=runif(1,0,1e-6),
                       phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                       eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})


log_likIO_CE   <- extract_log_lik(fitIO_CE  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CE   <- loo(log_likIO_CE  , r_eff=relative_eff(exp(log_likIO_CE)),cores=4,save_psis = TRUE)
print(loo_IO_CE)

save(fitIO_CE,log_likIO_CE,loo_IO_CE,file='IO_ECensus.RData')
