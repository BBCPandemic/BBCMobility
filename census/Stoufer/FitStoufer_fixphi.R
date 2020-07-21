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

Ecensus_mobility_dat$phi = 0.1
Wcensus_mobility_dat$phi = 0.1
NIcensus_mobility_dat$phi = 0.1
scotland_census_mobility_dat$phi = 0.1

fitSto_CSphi1 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = scotland_census_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CSphi1   <- extract_log_lik(fitSto_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CSphi1   <- loo(log_likSto_CSphi1  , r_eff=relative_eff(exp(log_likSto_CSphi1)),cores=4,save_psis = TRUE)
print(loo_Sto_CSphi1 )

save(fitSto_CSphi1,log_likSto_CSphi1,loo_Sto_CSphi1,file='Sto_SCensusphi1.RData')

fitSto_CNIphi1 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                       data = NIcensus_mobility_dat,
                       iter = 2000, chains = 4)

log_likSto_CNIphi1   <- extract_log_lik(fitSto_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CNIphi1   <- loo(log_likSto_CNIphi1  , r_eff=relative_eff(exp(log_likSto_CNIphi1)),cores=4,save_psis = TRUE)
print(loo_Sto_CNIphi1)

save(fitSto_CNIphi1,log_likSto_CNIphi1,loo_Sto_CNIphi1,file='Sto_NICensusphi1.RData')


fitSto_CWphi1 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = Wcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CWphi1   <- extract_log_lik(fitSto_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CWphi1   <- loo(log_likSto_CWphi1  , r_eff=relative_eff(exp(log_likSto_CWphi1)),cores=4,save_psis = TRUE)
print(loo_Sto_CWphi1)

save(fitSto_CWphi1,log_likSto_CWphi1,loo_Sto_CWphi1,file='Sto_WCensusphi1.RData')

fitSto_CEphi1 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = Ecensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CEphi1   <- extract_log_lik(fitSto_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CEphi1   <- loo(log_likSto_CEphi1  , r_eff=relative_eff(exp(log_likSto_CEphi1)),cores=4,save_psis = TRUE)
print(loo_Sto_CEphi1)

save(fitSto_CEphi1,log_likSto_CEphi1,loo_Sto_CEphi1,file='Sto_ECensusphi1.RData')

Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitSto_CSphi2 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CSphi2   <- extract_log_lik(fitSto_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CSphi2   <- loo(log_likSto_CSphi2  , r_eff=relative_eff(exp(log_likSto_CSphi2)),cores=4,save_psis = TRUE)
print(loo_Sto_CSphi2 )

save(fitSto_CSphi2,log_likSto_CSphi2,loo_Sto_CSphi2,file='Sto_SCensusphi2.RData')

fitSto_CNIphi2 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CNIphi2   <- extract_log_lik(fitSto_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CNIphi2   <- loo(log_likSto_CNIphi2  , r_eff=relative_eff(exp(log_likSto_CNIphi2)),cores=4,save_psis = TRUE)
print(loo_Sto_CNIphi2)

save(fitSto_CNIphi2,log_likSto_CNIphi2,loo_Sto_CNIphi2,file='Sto_NICensusphi2.RData')


fitSto_CWphi2 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CWphi2   <- extract_log_lik(fitSto_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CWphi2   <- loo(log_likSto_CWphi2  , r_eff=relative_eff(exp(log_likSto_CWphi2)),cores=4,save_psis = TRUE)
print(loo_Sto_CWphi2)

save(fitSto_CWphi2,log_likSto_CWphi2,loo_Sto_CWphi2,file='Sto_WCensusphi2.RData')

fitSto_CEphi2 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CEphi2   <- extract_log_lik(fitSto_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CEphi2   <- loo(log_likSto_CEphi2  , r_eff=relative_eff(exp(log_likSto_CEphi2)),cores=4,save_psis = TRUE)
print(loo_Sto_CEphi2)

save(fitSto_CEphi2,log_likSto_CEphi2,loo_Sto_CEphi2,file='Sto_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitSto_CSphi3 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CSphi3   <- extract_log_lik(fitSto_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CSphi3   <- loo(log_likSto_CSphi3  , r_eff=relative_eff(exp(log_likSto_CSphi3)),cores=4,save_psis = TRUE)
print(loo_Sto_CSphi3 )

save(fitSto_CSphi3,log_likSto_CSphi3,loo_Sto_CSphi3,file='Sto_SCensusphi3.RData')

fitSto_CNIphi3 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CNIphi3   <- extract_log_lik(fitSto_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CNIphi3   <- loo(log_likSto_CNIphi3  , r_eff=relative_eff(exp(log_likSto_CNIphi3)),cores=4,save_psis = TRUE)
print(loo_Sto_CNIphi3)

save(fitSto_CNIphi3,log_likSto_CNIphi3,loo_Sto_CNIphi3,file='Sto_NICensusphi3.RData')


fitSto_CWphi3 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CWphi3   <- extract_log_lik(fitSto_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CWphi3   <- loo(log_likSto_CWphi3  , r_eff=relative_eff(exp(log_likSto_CWphi3)),cores=4,save_psis = TRUE)
print(loo_Sto_CWphi3)

save(fitSto_CWphi3,log_likSto_CWphi3,loo_Sto_CWphi3,file='Sto_WCensusphi3.RData')

fitSto_CEphi3 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CEphi3   <- extract_log_lik(fitSto_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CEphi3   <- loo(log_likSto_CEphi3  , r_eff=relative_eff(exp(log_likSto_CEphi3)),cores=4,save_psis = TRUE)
print(loo_Sto_CEphi3)

save(fitSto_CEphi3,log_likSto_CEphi3,loo_Sto_CEphi3,file='Sto_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitSto_CSphi4 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CSphi4   <- extract_log_lik(fitSto_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CSphi4   <- loo(log_likSto_CSphi4  , r_eff=relative_eff(exp(log_likSto_CSphi4)),cores=4,save_psis = TRUE)
print(loo_Sto_CSphi4 )

save(fitSto_CSphi4,log_likSto_CSphi4,loo_Sto_CSphi4,file='Sto_SCensusphi4.RData')

fitSto_CNIphi4 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CNIphi4   <- extract_log_lik(fitSto_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CNIphi4   <- loo(log_likSto_CNIphi4  , r_eff=relative_eff(exp(log_likSto_CNIphi4)),cores=4,save_psis = TRUE)
print(loo_Sto_CNIphi4)

save(fitSto_CNIphi4,log_likSto_CNIphi4,loo_Sto_CNIphi4,file='Sto_NICensusphi4.RData')


fitSto_CWphi4 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CWphi4   <- extract_log_lik(fitSto_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CWphi4   <- loo(log_likSto_CWphi4  , r_eff=relative_eff(exp(log_likSto_CWphi4)),cores=4,save_psis = TRUE)
print(loo_Sto_CWphi4)

save(fitSto_CWphi4,log_likSto_CWphi4,loo_Sto_CWphi4,file='Sto_WCensusphi4.RData')

fitSto_CEphi4 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CEphi4   <- extract_log_lik(fitSto_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CEphi4   <- loo(log_likSto_CEphi4  , r_eff=relative_eff(exp(log_likSto_CEphi4)),cores=4,save_psis = TRUE)
print(loo_Sto_CEphi4)

save(fitSto_CEphi4,log_likSto_CEphi4,loo_Sto_CEphi4,file='Sto_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitSto_CSphi5 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CSphi5   <- extract_log_lik(fitSto_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CSphi5   <- loo(log_likSto_CSphi5  , r_eff=relative_eff(exp(log_likSto_CSphi5)),cores=4,save_psis = TRUE)
print(loo_Sto_CSphi5 )

save(fitSto_CSphi5,log_likSto_CSphi5,loo_Sto_CSphi5,file='Sto_SCensusphi5.RData')

fitSto_CNIphi5 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likSto_CNIphi5   <- extract_log_lik(fitSto_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CNIphi5   <- loo(log_likSto_CNIphi5  , r_eff=relative_eff(exp(log_likSto_CNIphi5)),cores=4,save_psis = TRUE)
print(loo_Sto_CNIphi5)

save(fitSto_CNIphi5,log_likSto_CNIphi5,loo_Sto_CNIphi5,file='Sto_NICensusphi5.RData')


fitSto_CWphi5 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CWphi5   <- extract_log_lik(fitSto_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CWphi5   <- loo(log_likSto_CWphi5  , r_eff=relative_eff(exp(log_likSto_CWphi5)),cores=4,save_psis = TRUE)
print(loo_Sto_CWphi5)

save(fitSto_CWphi5,log_likSto_CWphi5,loo_Sto_CWphi5,file='Sto_WCensusphi5.RData')

fitSto_CEphi5 <- stan(file = '../../stan/Stoufernegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likSto_CEphi5   <- extract_log_lik(fitSto_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Sto_CEphi5   <- loo(log_likSto_CEphi5  , r_eff=relative_eff(exp(log_likSto_CEphi5)),cores=4,save_psis = TRUE)
print(loo_Sto_CEphi5)

save(fitSto_CEphi5,log_likSto_CEphi5,loo_Sto_CEphi5,file='Sto_ECensusphi5.RData')



