require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(truncdist)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../DropLADSCensus.R')

Ecensus_mobility_dat$phi = 0.1
Wcensus_mobility_dat$phi = 0.1
NIcensus_mobility_dat$phi = 0.1
scotland_census_mobility_dat$phi = 0.1

fitIO_CSphi1 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CSphi1   <- extract_log_lik(fitIO_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CSphi1   <- loo(log_likIO_CSphi1  , r_eff=relative_eff(exp(log_likIO_CSphi1)),cores=4,save_psis = TRUE)
print(loo_IO_CSphi1 )

save(fitIO_CSphi1,log_likIO_CSphi1,loo_IO_CSphi1,file='IO_SCensusphi1.RData')

fitIO_CNIphi1 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likIO_CNIphi1   <- extract_log_lik(fitIO_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CNIphi1   <- loo(log_likIO_CNIphi1  , r_eff=relative_eff(exp(log_likIO_CNIphi1)),cores=4,save_psis = TRUE)
print(loo_IO_CNIphi1)

save(fitIO_CNIphi1,log_likIO_CNIphi1,loo_IO_CNIphi1,file='IO_NICensusphi1.RData')


fitIO_CWphi1 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CWphi1   <- extract_log_lik(fitIO_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CWphi1   <- loo(log_likIO_CWphi1  , r_eff=relative_eff(exp(log_likIO_CWphi1)),cores=4,save_psis = TRUE)
print(loo_IO_CWphi1)

save(fitIO_CWphi1,log_likIO_CWphi1,loo_IO_CWphi1,file='IO_WCensusphi1.RData')

fitIO_CEphi1 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,
                     init=function()
                     {list(gamma=runif(1,0,1e-6),
                           phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                           eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO_CEphi1   <- extract_log_lik(fitIO_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CEphi1   <- loo(log_likIO_CEphi1  , r_eff=relative_eff(exp(log_likIO_CEphi1)),cores=4,save_psis = TRUE)
print(loo_IO_CEphi1)

save(fitIO_CEphi1,log_likIO_CEphi1,loo_IO_CEphi1,file='IO_ECensusphi1.RData')

Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitIO_CSphi2 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CSphi2   <- extract_log_lik(fitIO_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CSphi2   <- loo(log_likIO_CSphi2  , r_eff=relative_eff(exp(log_likIO_CSphi2)),cores=4,save_psis = TRUE)
print(loo_IO_CSphi2 )

save(fitIO_CSphi2,log_likIO_CSphi2,loo_IO_CSphi2,file='IO_SCensusphi2.RData')

fitIO_CNIphi2 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likIO_CNIphi2   <- extract_log_lik(fitIO_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CNIphi2   <- loo(log_likIO_CNIphi2  , r_eff=relative_eff(exp(log_likIO_CNIphi2)),cores=4,save_psis = TRUE)
print(loo_IO_CNIphi2)

save(fitIO_CNIphi2,log_likIO_CNIphi2,loo_IO_CNIphi2,file='IO_NICensusphi2.RData')


fitIO_CWphi2 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CWphi2   <- extract_log_lik(fitIO_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CWphi2   <- loo(log_likIO_CWphi2  , r_eff=relative_eff(exp(log_likIO_CWphi2)),cores=4,save_psis = TRUE)
print(loo_IO_CWphi2)

save(fitIO_CWphi2,log_likIO_CWphi2,loo_IO_CWphi2,file='IO_WCensusphi2.RData')

fitIO_CEphi2 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,
                     init=function()
                     {list(gamma=runif(1,0,1e-6),
                           phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                           eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO_CEphi2   <- extract_log_lik(fitIO_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CEphi2   <- loo(log_likIO_CEphi2  , r_eff=relative_eff(exp(log_likIO_CEphi2)),cores=4,save_psis = TRUE)
print(loo_IO_CEphi2)

save(fitIO_CEphi2,log_likIO_CEphi2,loo_IO_CEphi2,file='IO_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitIO_CSphi3 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CSphi3   <- extract_log_lik(fitIO_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CSphi3   <- loo(log_likIO_CSphi3  , r_eff=relative_eff(exp(log_likIO_CSphi3)),cores=4,save_psis = TRUE)
print(loo_IO_CSphi3 )

save(fitIO_CSphi3,log_likIO_CSphi3,loo_IO_CSphi3,file='IO_SCensusphi3.RData')

fitIO_CNIphi3 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likIO_CNIphi3   <- extract_log_lik(fitIO_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CNIphi3   <- loo(log_likIO_CNIphi3  , r_eff=relative_eff(exp(log_likIO_CNIphi3)),cores=4,save_psis = TRUE)
print(loo_IO_CNIphi3)

save(fitIO_CNIphi3,log_likIO_CNIphi3,loo_IO_CNIphi3,file='IO_NICensusphi3.RData')


fitIO_CWphi3 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CWphi3   <- extract_log_lik(fitIO_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CWphi3   <- loo(log_likIO_CWphi3  , r_eff=relative_eff(exp(log_likIO_CWphi3)),cores=4,save_psis = TRUE)
print(loo_IO_CWphi3)

save(fitIO_CWphi3,log_likIO_CWphi3,loo_IO_CWphi3,file='IO_WCensusphi3.RData')

fitIO_CEphi3 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,
                     init=function()
                     {list(gamma=runif(1,0,1e-6),
                           phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                           eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO_CEphi3   <- extract_log_lik(fitIO_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CEphi3   <- loo(log_likIO_CEphi3  , r_eff=relative_eff(exp(log_likIO_CEphi3)),cores=4,save_psis = TRUE)
print(loo_IO_CEphi3)

save(fitIO_CEphi3,log_likIO_CEphi3,loo_IO_CEphi3,file='IO_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitIO_CSphi4 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CSphi4   <- extract_log_lik(fitIO_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CSphi4   <- loo(log_likIO_CSphi4  , r_eff=relative_eff(exp(log_likIO_CSphi4)),cores=4,save_psis = TRUE)
print(loo_IO_CSphi4 )

save(fitIO_CSphi4,log_likIO_CSphi4,loo_IO_CSphi4,file='IO_SCensusphi4.RData')

fitIO_CNIphi4 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likIO_CNIphi4   <- extract_log_lik(fitIO_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CNIphi4   <- loo(log_likIO_CNIphi4  , r_eff=relative_eff(exp(log_likIO_CNIphi4)),cores=4,save_psis = TRUE)
print(loo_IO_CNIphi4)

save(fitIO_CNIphi4,log_likIO_CNIphi4,loo_IO_CNIphi4,file='IO_NICensusphi4.RData')


fitIO_CWphi4 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CWphi4   <- extract_log_lik(fitIO_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CWphi4   <- loo(log_likIO_CWphi4  , r_eff=relative_eff(exp(log_likIO_CWphi4)),cores=4,save_psis = TRUE)
print(loo_IO_CWphi4)

save(fitIO_CWphi4,log_likIO_CWphi4,loo_IO_CWphi4,file='IO_WCensusphi4.RData')

fitIO_CEphi4 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,
                     init=function()
                     {list(gamma=runif(1,0,1e-6),
                           phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                           eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO_CEphi4   <- extract_log_lik(fitIO_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CEphi4   <- loo(log_likIO_CEphi4  , r_eff=relative_eff(exp(log_likIO_CEphi4)),cores=4,save_psis = TRUE)
print(loo_IO_CEphi4)

save(fitIO_CEphi4,log_likIO_CEphi4,loo_IO_CEphi4,file='IO_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitIO_CSphi5 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CSphi5   <- extract_log_lik(fitIO_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CSphi5   <- loo(log_likIO_CSphi5  , r_eff=relative_eff(exp(log_likIO_CSphi5)),cores=4,save_psis = TRUE)
print(loo_IO_CSphi5 )

save(fitIO_CSphi5,log_likIO_CSphi5,loo_IO_CSphi5,file='IO_SCensusphi5.RData')

fitIO_CNIphi5 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likIO_CNIphi5   <- extract_log_lik(fitIO_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CNIphi5   <- loo(log_likIO_CNIphi5  , r_eff=relative_eff(exp(log_likIO_CNIphi5)),cores=4,save_psis = TRUE)
print(loo_IO_CNIphi5)

save(fitIO_CNIphi5,log_likIO_CNIphi5,loo_IO_CNIphi5,file='IO_NICensusphi5.RData')


fitIO_CWphi5 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likIO_CWphi5   <- extract_log_lik(fitIO_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CWphi5   <- loo(log_likIO_CWphi5  , r_eff=relative_eff(exp(log_likIO_CWphi5)),cores=4,save_psis = TRUE)
print(loo_IO_CWphi5)

save(fitIO_CWphi5,log_likIO_CWphi5,loo_IO_CWphi5,file='IO_WCensusphi5.RData')

fitIO_CEphi5 <- stan(file = '../../stan/IOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,
                     init=function()
                     {list(gamma=runif(1,0,1e-6),
                           phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                           eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO_CEphi5   <- extract_log_lik(fitIO_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_IO_CEphi5   <- loo(log_likIO_CEphi5  , r_eff=relative_eff(exp(log_likIO_CEphi5)),cores=4,save_psis = TRUE)
print(loo_IO_CEphi5)

save(fitIO_CEphi5,log_likIO_CEphi5,loo_IO_CEphi5,file='IO_ECensusphi5.RData')



