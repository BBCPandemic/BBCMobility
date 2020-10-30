require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../DropLADSCensus.R')

Ecensus_mobility_dat$phi = 0.1
Wcensus_mobility_dat$phi = 0.1
NIcensus_mobility_dat$phi = 0.1
scotland_census_mobility_dat$phi = 0.1

fitCDO_CSphi1 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = scotland_census_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CSphi1   <- extract_log_lik(fitCDO_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CSphi1   <- loo(log_likCDO_CSphi1  , r_eff=relative_eff(exp(log_likCDO_CSphi1)),cores=4,save_psis = TRUE)
print(loo_CDO_CSphi1 )

save(fitCDO_CSphi1,log_likCDO_CSphi1,loo_CDO_CSphi1,file='CDO_SCensusphi1.RData')

fitCDO_CNIphi1 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                       data = NIcensus_mobility_dat,
                       iter = 2000, chains = 4)

log_likCDO_CNIphi1   <- extract_log_lik(fitCDO_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CNIphi1   <- loo(log_likCDO_CNIphi1  , r_eff=relative_eff(exp(log_likCDO_CNIphi1)),cores=4,save_psis = TRUE)
print(loo_CDO_CNIphi1)

save(fitCDO_CNIphi1,log_likCDO_CNIphi1,loo_CDO_CNIphi1,file='CDO_NICensusphi1.RData')


fitCDO_CWphi1 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = Wcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CWphi1   <- extract_log_lik(fitCDO_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CWphi1   <- loo(log_likCDO_CWphi1  , r_eff=relative_eff(exp(log_likCDO_CWphi1)),cores=4,save_psis = TRUE)
print(loo_CDO_CWphi1)

save(fitCDO_CWphi1,log_likCDO_CWphi1,loo_CDO_CWphi1,file='CDO_WCensusphi1.RData')

fitCDO_CEphi1 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = Ecensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CEphi1   <- extract_log_lik(fitCDO_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CEphi1   <- loo(log_likCDO_CEphi1  , r_eff=relative_eff(exp(log_likCDO_CEphi1)),cores=4,save_psis = TRUE)
print(loo_CDO_CEphi1)

save(fitCDO_CEphi1,log_likCDO_CEphi1,loo_CDO_CEphi1,file='CDO_ECensusphi1.RData')


Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitCDO_CSphi2 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CSphi2   <- extract_log_lik(fitCDO_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CSphi2   <- loo(log_likCDO_CSphi2  , r_eff=relative_eff(exp(log_likCDO_CSphi2)),cores=4,save_psis = TRUE)
print(loo_CDO_CSphi2 )

save(fitCDO_CSphi2,log_likCDO_CSphi2,loo_CDO_CSphi2,file='CDO_SCensusphi2.RData')

fitCDO_CNIphi2 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CNIphi2   <- extract_log_lik(fitCDO_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CNIphi2   <- loo(log_likCDO_CNIphi2  , r_eff=relative_eff(exp(log_likCDO_CNIphi2)),cores=4,save_psis = TRUE)
print(loo_CDO_CNIphi2)

save(fitCDO_CNIphi2,log_likCDO_CNIphi2,loo_CDO_CNIphi2,file='CDO_NICensusphi2.RData')


fitCDO_CWphi2 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CWphi2   <- extract_log_lik(fitCDO_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CWphi2   <- loo(log_likCDO_CWphi2  , r_eff=relative_eff(exp(log_likCDO_CWphi2)),cores=4,save_psis = TRUE)
print(loo_CDO_CWphi2)

save(fitCDO_CWphi2,log_likCDO_CWphi2,loo_CDO_CWphi2,file='CDO_WCensusphi2.RData')

fitCDO_CEphi2 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CEphi2   <- extract_log_lik(fitCDO_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CEphi2   <- loo(log_likCDO_CEphi2  , r_eff=relative_eff(exp(log_likCDO_CEphi2)),cores=4,save_psis = TRUE)
print(loo_CDO_CEphi2)

save(fitCDO_CEphi2,log_likCDO_CEphi2,loo_CDO_CEphi2,file='CDO_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitCDO_CSphi3 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CSphi3   <- extract_log_lik(fitCDO_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CSphi3   <- loo(log_likCDO_CSphi3  , r_eff=relative_eff(exp(log_likCDO_CSphi3)),cores=4,save_psis = TRUE)
print(loo_CDO_CSphi3 )

save(fitCDO_CSphi3,log_likCDO_CSphi3,loo_CDO_CSphi3,file='CDO_SCensusphi3.RData')

fitCDO_CNIphi3 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CNIphi3   <- extract_log_lik(fitCDO_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CNIphi3   <- loo(log_likCDO_CNIphi3  , r_eff=relative_eff(exp(log_likCDO_CNIphi3)),cores=4,save_psis = TRUE)
print(loo_CDO_CNIphi3)

save(fitCDO_CNIphi3,log_likCDO_CNIphi3,loo_CDO_CNIphi3,file='CDO_NICensusphi3.RData')


fitCDO_CWphi3 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CWphi3   <- extract_log_lik(fitCDO_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CWphi3   <- loo(log_likCDO_CWphi3  , r_eff=relative_eff(exp(log_likCDO_CWphi3)),cores=4,save_psis = TRUE)
print(loo_CDO_CWphi3)

save(fitCDO_CWphi3,log_likCDO_CWphi3,loo_CDO_CWphi3,file='CDO_WCensusphi3.RData')

fitCDO_CEphi3 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CEphi3   <- extract_log_lik(fitCDO_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CEphi3   <- loo(log_likCDO_CEphi3  , r_eff=relative_eff(exp(log_likCDO_CEphi3)),cores=4,save_psis = TRUE)
print(loo_CDO_CEphi3)

save(fitCDO_CEphi3,log_likCDO_CEphi3,loo_CDO_CEphi3,file='CDO_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitCDO_CSphi4 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CSphi4   <- extract_log_lik(fitCDO_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CSphi4   <- loo(log_likCDO_CSphi4  , r_eff=relative_eff(exp(log_likCDO_CSphi4)),cores=4,save_psis = TRUE)
print(loo_CDO_CSphi4 )

save(fitCDO_CSphi4,log_likCDO_CSphi4,loo_CDO_CSphi4,file='CDO_SCensusphi4.RData')

fitCDO_CNIphi4 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CNIphi4   <- extract_log_lik(fitCDO_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CNIphi4   <- loo(log_likCDO_CNIphi4  , r_eff=relative_eff(exp(log_likCDO_CNIphi4)),cores=4,save_psis = TRUE)
print(loo_CDO_CNIphi4)

save(fitCDO_CNIphi4,log_likCDO_CNIphi4,loo_CDO_CNIphi4,file='CDO_NICensusphi4.RData')


fitCDO_CWphi4 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CWphi4   <- extract_log_lik(fitCDO_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CWphi4   <- loo(log_likCDO_CWphi4  , r_eff=relative_eff(exp(log_likCDO_CWphi4)),cores=4,save_psis = TRUE)
print(loo_CDO_CWphi4)

save(fitCDO_CWphi4,log_likCDO_CWphi4,loo_CDO_CWphi4,file='CDO_WCensusphi4.RData')

fitCDO_CEphi4 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CEphi4   <- extract_log_lik(fitCDO_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CEphi4   <- loo(log_likCDO_CEphi4  , r_eff=relative_eff(exp(log_likCDO_CEphi4)),cores=4,save_psis = TRUE)
print(loo_CDO_CEphi4)

save(fitCDO_CEphi4,log_likCDO_CEphi4,loo_CDO_CEphi4,file='CDO_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitCDO_CSphi5 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CSphi5   <- extract_log_lik(fitCDO_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CSphi5   <- loo(log_likCDO_CSphi5  , r_eff=relative_eff(exp(log_likCDO_CSphi5)),cores=4,save_psis = TRUE)
print(loo_CDO_CSphi5 )

save(fitCDO_CSphi5,log_likCDO_CSphi5,loo_CDO_CSphi5,file='CDO_SCensusphi5.RData')

fitCDO_CNIphi5 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDO_CNIphi5   <- extract_log_lik(fitCDO_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CNIphi5   <- loo(log_likCDO_CNIphi5  , r_eff=relative_eff(exp(log_likCDO_CNIphi5)),cores=4,save_psis = TRUE)
print(loo_CDO_CNIphi5)

save(fitCDO_CNIphi5,log_likCDO_CNIphi5,loo_CDO_CNIphi5,file='CDO_NICensusphi5.RData')


fitCDO_CWphi5 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CWphi5   <- extract_log_lik(fitCDO_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CWphi5   <- loo(log_likCDO_CWphi5  , r_eff=relative_eff(exp(log_likCDO_CWphi5)),cores=4,save_psis = TRUE)
print(loo_CDO_CWphi5)

save(fitCDO_CWphi5,log_likCDO_CWphi5,loo_CDO_CWphi5,file='CDO_WCensusphi5.RData')

fitCDO_CEphi5 <- stan(file = '../../stan/CDOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDO_CEphi5   <- extract_log_lik(fitCDO_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CEphi5   <- loo(log_likCDO_CEphi5  , r_eff=relative_eff(exp(log_likCDO_CEphi5)),cores=4,save_psis = TRUE)
print(loo_CDO_CEphi5)

save(fitCDO_CEphi5,log_likCDO_CEphi5,loo_CDO_CEphi5,file='CDO_ECensusphi5.RData')



