require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

# Patch data sets to remove outlier LADS
source('../../DropLADSCensus.R')

Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitCDP_CSphi2 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CSphi2   <- extract_log_lik(fitCDP_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CSphi2   <- loo(log_likCDP_CSphi2  , r_eff=relative_eff(exp(log_likCDP_CSphi2)),cores=4,save_psis = TRUE)
print(loo_CDP_CSphi2 )

save(fitCDP_CSphi2,log_likCDP_CSphi2,loo_CDP_CSphi2,file='CDP_SCensusphi2.RData')

fitCDP_CNIphi2 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CNIphi2   <- extract_log_lik(fitCDP_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CNIphi2   <- loo(log_likCDP_CNIphi2  , r_eff=relative_eff(exp(log_likCDP_CNIphi2)),cores=4,save_psis = TRUE)
print(loo_CDP_CNIphi2)

save(fitCDP_CNIphi2,log_likCDP_CNIphi2,loo_CDP_CNIphi2,file='CDP_NICensusphi2.RData')


fitCDP_CWphi2 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CWphi2   <- extract_log_lik(fitCDP_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CWphi2   <- loo(log_likCDP_CWphi2  , r_eff=relative_eff(exp(log_likCDP_CWphi2)),cores=4,save_psis = TRUE)
print(loo_CDP_CWphi2)

save(fitCDP_CWphi2,log_likCDP_CWphi2,loo_CDP_CWphi2,file='CDP_WCensusphi2.RData')

fitCDP_CEphi2 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CEphi2   <- extract_log_lik(fitCDP_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CEphi2   <- loo(log_likCDP_CEphi2  , r_eff=relative_eff(exp(log_likCDP_CEphi2)),cores=4,save_psis = TRUE)
print(loo_CDP_CEphi2)

save(fitCDP_CEphi2,log_likCDP_CEphi2,loo_CDP_CEphi2,file='CDP_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitCDP_CSphi3 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CSphi3   <- extract_log_lik(fitCDP_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CSphi3   <- loo(log_likCDP_CSphi3  , r_eff=relative_eff(exp(log_likCDP_CSphi3)),cores=4,save_psis = TRUE)
print(loo_CDP_CSphi3 )

save(fitCDP_CSphi3,log_likCDP_CSphi3,loo_CDP_CSphi3,file='CDP_SCensusphi3.RData')

fitCDP_CNIphi3 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CNIphi3   <- extract_log_lik(fitCDP_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CNIphi3   <- loo(log_likCDP_CNIphi3  , r_eff=relative_eff(exp(log_likCDP_CNIphi3)),cores=4,save_psis = TRUE)
print(loo_CDP_CNIphi3)

save(fitCDP_CNIphi3,log_likCDP_CNIphi3,loo_CDP_CNIphi3,file='CDP_NICensusphi3.RData')


fitCDP_CWphi3 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CWphi3   <- extract_log_lik(fitCDP_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CWphi3   <- loo(log_likCDP_CWphi3  , r_eff=relative_eff(exp(log_likCDP_CWphi3)),cores=4,save_psis = TRUE)
print(loo_CDP_CWphi3)

save(fitCDP_CWphi3,log_likCDP_CWphi3,loo_CDP_CWphi3,file='CDP_WCensusphi3.RData')

fitCDP_CEphi3 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CEphi3   <- extract_log_lik(fitCDP_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CEphi3   <- loo(log_likCDP_CEphi3  , r_eff=relative_eff(exp(log_likCDP_CEphi3)),cores=4,save_psis = TRUE)
print(loo_CDP_CEphi3)

save(fitCDP_CEphi3,log_likCDP_CEphi3,loo_CDP_CEphi3,file='CDP_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitCDP_CSphi4 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CSphi4   <- extract_log_lik(fitCDP_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CSphi4   <- loo(log_likCDP_CSphi4  , r_eff=relative_eff(exp(log_likCDP_CSphi4)),cores=4,save_psis = TRUE)
print(loo_CDP_CSphi4 )

save(fitCDP_CSphi4,log_likCDP_CSphi4,loo_CDP_CSphi4,file='CDP_SCensusphi4.RData')

fitCDP_CNIphi4 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CNIphi4   <- extract_log_lik(fitCDP_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CNIphi4   <- loo(log_likCDP_CNIphi4  , r_eff=relative_eff(exp(log_likCDP_CNIphi4)),cores=4,save_psis = TRUE)
print(loo_CDP_CNIphi4)

save(fitCDP_CNIphi4,log_likCDP_CNIphi4,loo_CDP_CNIphi4,file='CDP_NICensusphi4.RData')


fitCDP_CWphi4 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CWphi4   <- extract_log_lik(fitCDP_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CWphi4   <- loo(log_likCDP_CWphi4  , r_eff=relative_eff(exp(log_likCDP_CWphi4)),cores=4,save_psis = TRUE)
print(loo_CDP_CWphi4)

save(fitCDP_CWphi4,log_likCDP_CWphi4,loo_CDP_CWphi4,file='CDP_WCensusphi4.RData')

fitCDP_CEphi4 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CEphi4   <- extract_log_lik(fitCDP_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CEphi4   <- loo(log_likCDP_CEphi4  , r_eff=relative_eff(exp(log_likCDP_CEphi4)),cores=4,save_psis = TRUE)
print(loo_CDP_CEphi4)

save(fitCDP_CEphi4,log_likCDP_CEphi4,loo_CDP_CEphi4,file='CDP_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitCDP_CSphi5 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CSphi5   <- extract_log_lik(fitCDP_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CSphi5   <- loo(log_likCDP_CSphi5  , r_eff=relative_eff(exp(log_likCDP_CSphi5)),cores=4,save_psis = TRUE)
print(loo_CDP_CSphi5 )

save(fitCDP_CSphi5,log_likCDP_CSphi5,loo_CDP_CSphi5,file='CDP_SCensusphi5.RData')

fitCDP_CNIphi5 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CNIphi5   <- extract_log_lik(fitCDP_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CNIphi5   <- loo(log_likCDP_CNIphi5  , r_eff=relative_eff(exp(log_likCDP_CNIphi5)),cores=4,save_psis = TRUE)
print(loo_CDP_CNIphi5)

save(fitCDP_CNIphi5,log_likCDP_CNIphi5,loo_CDP_CNIphi5,file='CDP_NICensusphi5.RData')


fitCDP_CWphi5 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CWphi5   <- extract_log_lik(fitCDP_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CWphi5   <- loo(log_likCDP_CWphi5  , r_eff=relative_eff(exp(log_likCDP_CWphi5)),cores=4,save_psis = TRUE)
print(loo_CDP_CWphi5)

save(fitCDP_CWphi5,log_likCDP_CWphi5,loo_CDP_CWphi5,file='CDP_WCensusphi5.RData')

fitCDP_CEphi5 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likCDP_CEphi5   <- extract_log_lik(fitCDP_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CEphi5   <- loo(log_likCDP_CEphi5  , r_eff=relative_eff(exp(log_likCDP_CEphi5)),cores=4,save_psis = TRUE)
print(loo_CDP_CEphi5)

save(fitCDP_CEphi5,log_likCDP_CEphi5,loo_CDP_CEphi5,file='CDP_ECensusphi5.RData')


Ecensus_mobility_dat$phi = 0.1
Wcensus_mobility_dat$phi = 0.1
NIcensus_mobility_dat$phi = 0.1
scotland_census_mobility_dat$phi = 0.1

fitCDP_CSphi1 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = scotland_census_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CSphi1   <- extract_log_lik(fitCDP_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CSphi1   <- loo(log_likCDP_CSphi1  , r_eff=relative_eff(exp(log_likCDP_CSphi1)),cores=4,save_psis = TRUE)
print(loo_CDP_CSphi1 )

save(fitCDP_CSphi1,log_likCDP_CSphi1,loo_CDP_CSphi1,file='CDP_SCensusphi1.RData')

fitCDP_CNIphi1 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                       data = NIcensus_mobility_dat,
                       iter = 2000, chains = 4)

log_likCDP_CNIphi1   <- extract_log_lik(fitCDP_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CNIphi1   <- loo(log_likCDP_CNIphi1  , r_eff=relative_eff(exp(log_likCDP_CNIphi1)),cores=4,save_psis = TRUE)
print(loo_CDP_CNIphi1)

save(fitCDP_CNIphi1,log_likCDP_CNIphi1,loo_CDP_CNIphi1,file='CDP_NICensusphi1.RData')


fitCDP_CWphi1 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = Wcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CWphi1   <- extract_log_lik(fitCDP_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CWphi1   <- loo(log_likCDP_CWphi1  , r_eff=relative_eff(exp(log_likCDP_CWphi1)),cores=4,save_psis = TRUE)
print(loo_CDP_CWphi1)

save(fitCDP_CWphi1,log_likCDP_CWphi1,loo_CDP_CWphi1,file='CDP_WCensusphi1.RData')

fitCDP_CEphi1 <- stan(file = '../../stan/CDPnegbin_fixphi.stan',
                      data = Ecensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likCDP_CEphi1   <- extract_log_lik(fitCDP_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_CDP_CEphi1   <- loo(log_likCDP_CEphi1  , r_eff=relative_eff(exp(log_likCDP_CEphi1)),cores=4,save_psis = TRUE)
print(loo_CDP_CEphi1)

save(fitCDP_CEphi1,log_likCDP_CEphi1,loo_CDP_CEphi1,file='CDP_ECensusphi1.RData')

