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

Ecensus_mobility_dat$phi = 0.1
Wcensus_mobility_dat$phi = 0.1
NIcensus_mobility_dat$phi = 0.1
scotland_census_mobility_dat$phi = 0.1

fitGO_CSphi1 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CSphi1   <- extract_log_lik(fitGO_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CSphi1   <- loo(log_likGO_CSphi1  , r_eff=relative_eff(exp(log_likGO_CSphi1)),cores=4,save_psis = TRUE)
print(loo_GO_CSphi1 )

save(fitGO_CSphi1,log_likGO_CSphi1,loo_GO_CSphi1,file='GO_SCensusphi1.RData')

fitGO_CNIphi1 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likGO_CNIphi1   <- extract_log_lik(fitGO_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CNIphi1   <- loo(log_likGO_CNIphi1  , r_eff=relative_eff(exp(log_likGO_CNIphi1)),cores=4,save_psis = TRUE)
print(loo_GO_CNIphi1)

save(fitGO_CNIphi1,log_likGO_CNIphi1,loo_GO_CNIphi1,file='GO_NICensusphi1.RData')


fitGO_CWphi1 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CWphi1   <- extract_log_lik(fitGO_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CWphi1   <- loo(log_likGO_CWphi1  , r_eff=relative_eff(exp(log_likGO_CWphi1)),cores=4,save_psis = TRUE)
print(loo_GO_CWphi1)

save(fitGO_CWphi1,log_likGO_CWphi1,loo_GO_CWphi1,file='GO_WCensusphi1.RData')

fitGO_CEphi1 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CEphi1   <- extract_log_lik(fitGO_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CEphi1   <- loo(log_likGO_CEphi1  , r_eff=relative_eff(exp(log_likGO_CEphi1)),cores=4,save_psis = TRUE)
print(loo_GO_CEphi1)

save(fitGO_CEphi1,log_likGO_CEphi1,loo_GO_CEphi1,file='GO_ECensusphi1.RData')


Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitGO_CSphi2 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CSphi2   <- extract_log_lik(fitGO_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CSphi2   <- loo(log_likGO_CSphi2  , r_eff=relative_eff(exp(log_likGO_CSphi2)),cores=4,save_psis = TRUE)
print(loo_GO_CSphi2 )

save(fitGO_CSphi2,log_likGO_CSphi2,loo_GO_CSphi2,file='GO_SCensusphi2.RData')

fitGO_CNIphi2 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likGO_CNIphi2   <- extract_log_lik(fitGO_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CNIphi2   <- loo(log_likGO_CNIphi2  , r_eff=relative_eff(exp(log_likGO_CNIphi2)),cores=4,save_psis = TRUE)
print(loo_GO_CNIphi2)

save(fitGO_CNIphi2,log_likGO_CNIphi2,loo_GO_CNIphi2,file='GO_NICensusphi2.RData')


fitGO_CWphi2 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CWphi2   <- extract_log_lik(fitGO_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CWphi2   <- loo(log_likGO_CWphi2  , r_eff=relative_eff(exp(log_likGO_CWphi2)),cores=4,save_psis = TRUE)
print(loo_GO_CWphi2)

save(fitGO_CWphi2,log_likGO_CWphi2,loo_GO_CWphi2,file='GO_WCensusphi2.RData')

fitGO_CEphi2 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CEphi2   <- extract_log_lik(fitGO_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CEphi2   <- loo(log_likGO_CEphi2  , r_eff=relative_eff(exp(log_likGO_CEphi2)),cores=4,save_psis = TRUE)
print(loo_GO_CEphi2)

save(fitGO_CEphi2,log_likGO_CEphi2,loo_GO_CEphi2,file='GO_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitGO_CSphi3 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CSphi3   <- extract_log_lik(fitGO_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CSphi3   <- loo(log_likGO_CSphi3  , r_eff=relative_eff(exp(log_likGO_CSphi3)),cores=4,save_psis = TRUE)
print(loo_GO_CSphi3 )

save(fitGO_CSphi3,log_likGO_CSphi3,loo_GO_CSphi3,file='GO_SCensusphi3.RData')

fitGO_CNIphi3 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likGO_CNIphi3   <- extract_log_lik(fitGO_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CNIphi3   <- loo(log_likGO_CNIphi3  , r_eff=relative_eff(exp(log_likGO_CNIphi3)),cores=4,save_psis = TRUE)
print(loo_GO_CNIphi3)

save(fitGO_CNIphi3,log_likGO_CNIphi3,loo_GO_CNIphi3,file='GO_NICensusphi3.RData')


fitGO_CWphi3 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CWphi3   <- extract_log_lik(fitGO_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CWphi3   <- loo(log_likGO_CWphi3  , r_eff=relative_eff(exp(log_likGO_CWphi3)),cores=4,save_psis = TRUE)
print(loo_GO_CWphi3)

save(fitGO_CWphi3,log_likGO_CWphi3,loo_GO_CWphi3,file='GO_WCensusphi3.RData')

fitGO_CEphi3 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CEphi3   <- extract_log_lik(fitGO_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CEphi3   <- loo(log_likGO_CEphi3  , r_eff=relative_eff(exp(log_likGO_CEphi3)),cores=4,save_psis = TRUE)
print(loo_GO_CEphi3)

save(fitGO_CEphi3,log_likGO_CEphi3,loo_GO_CEphi3,file='GO_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitGO_CSphi4 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CSphi4   <- extract_log_lik(fitGO_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CSphi4   <- loo(log_likGO_CSphi4  , r_eff=relative_eff(exp(log_likGO_CSphi4)),cores=4,save_psis = TRUE)
print(loo_GO_CSphi4 )

save(fitGO_CSphi4,log_likGO_CSphi4,loo_GO_CSphi4,file='GO_SCensusphi4.RData')

fitGO_CNIphi4 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likGO_CNIphi4   <- extract_log_lik(fitGO_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CNIphi4   <- loo(log_likGO_CNIphi4  , r_eff=relative_eff(exp(log_likGO_CNIphi4)),cores=4,save_psis = TRUE)
print(loo_GO_CNIphi4)

save(fitGO_CNIphi4,log_likGO_CNIphi4,loo_GO_CNIphi4,file='GO_NICensusphi4.RData')


fitGO_CWphi4 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CWphi4   <- extract_log_lik(fitGO_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CWphi4   <- loo(log_likGO_CWphi4  , r_eff=relative_eff(exp(log_likGO_CWphi4)),cores=4,save_psis = TRUE)
print(loo_GO_CWphi4)

save(fitGO_CWphi4,log_likGO_CWphi4,loo_GO_CWphi4,file='GO_WCensusphi4.RData')

fitGO_CEphi4 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CEphi4   <- extract_log_lik(fitGO_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CEphi4   <- loo(log_likGO_CEphi4  , r_eff=relative_eff(exp(log_likGO_CEphi4)),cores=4,save_psis = TRUE)
print(loo_GO_CEphi4)

save(fitGO_CEphi4,log_likGO_CEphi4,loo_GO_CEphi4,file='GO_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitGO_CSphi5 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CSphi5   <- extract_log_lik(fitGO_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CSphi5   <- loo(log_likGO_CSphi5  , r_eff=relative_eff(exp(log_likGO_CSphi5)),cores=4,save_psis = TRUE)
print(loo_GO_CSphi5 )

save(fitGO_CSphi5,log_likGO_CSphi5,loo_GO_CSphi5,file='GO_SCensusphi5.RData')

fitGO_CNIphi5 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likGO_CNIphi5   <- extract_log_lik(fitGO_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CNIphi5   <- loo(log_likGO_CNIphi5  , r_eff=relative_eff(exp(log_likGO_CNIphi5)),cores=4,save_psis = TRUE)
print(loo_GO_CNIphi5)

save(fitGO_CNIphi5,log_likGO_CNIphi5,loo_GO_CNIphi5,file='GO_NICensusphi5.RData')


fitGO_CWphi5 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CWphi5   <- extract_log_lik(fitGO_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CWphi5   <- loo(log_likGO_CWphi5  , r_eff=relative_eff(exp(log_likGO_CWphi5)),cores=4,save_psis = TRUE)
print(loo_GO_CWphi5)

save(fitGO_CWphi5,log_likGO_CWphi5,loo_GO_CWphi5,file='GO_WCensusphi5.RData')

fitGO_CEphi5 <- stan(file = '../../stan/GOnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likGO_CEphi5   <- extract_log_lik(fitGO_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_GO_CEphi5   <- loo(log_likGO_CEphi5  , r_eff=relative_eff(exp(log_likGO_CEphi5)),cores=4,save_psis = TRUE)
print(loo_GO_CEphi5)

save(fitGO_CEphi5,log_likGO_CEphi5,loo_GO_CEphi5,file='GO_ECensusphi5.RData')



