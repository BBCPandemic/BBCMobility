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

fitERad_CSphi1 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                       data = scotland_census_mobility_dat,
                       iter = 2000, chains = 4)

log_likERad_CSphi1   <- extract_log_lik(fitERad_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CSphi1   <- loo(log_likERad_CSphi1  , r_eff=relative_eff(exp(log_likERad_CSphi1)),cores=4,save_psis = TRUE)
print(loo_ERad_CSphi1 )

save(fitERad_CSphi1,log_likERad_CSphi1,loo_ERad_CSphi1,file='ERad_SCensusphi1.RData')

fitERad_CNIphi1 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                        data = NIcensus_mobility_dat,
                        iter = 2000, chains = 4)

log_likERad_CNIphi1   <- extract_log_lik(fitERad_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CNIphi1   <- loo(log_likERad_CNIphi1  , r_eff=relative_eff(exp(log_likERad_CNIphi1)),cores=4,save_psis = TRUE)
print(loo_ERad_CNIphi1)

save(fitERad_CNIphi1,log_likERad_CNIphi1,loo_ERad_CNIphi1,file='ERad_NICensusphi1.RData')


fitERad_CWphi1 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                       data = Wcensus_mobility_dat,
                       iter = 2000, chains = 4)

log_likERad_CWphi1   <- extract_log_lik(fitERad_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CWphi1   <- loo(log_likERad_CWphi1  , r_eff=relative_eff(exp(log_likERad_CWphi1)),cores=4,save_psis = TRUE)
print(loo_ERad_CWphi1)

save(fitERad_CWphi1,log_likERad_CWphi1,loo_ERad_CWphi1,file='ERad_WCensusphi1.RData')

fitERad_CEphi1 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                       data = Ecensus_mobility_dat,
                       iter = 2000, chains = 4)

log_likERad_CEphi1   <- extract_log_lik(fitERad_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CEphi1   <- loo(log_likERad_CEphi1  , r_eff=relative_eff(exp(log_likERad_CEphi1)),cores=4,save_psis = TRUE)
print(loo_ERad_CEphi1)

save(fitERad_CEphi1,log_likERad_CEphi1,loo_ERad_CEphi1,file='ERad_ECensusphi1.RData')

Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitERad_CSphi2 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CSphi2   <- extract_log_lik(fitERad_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CSphi2   <- loo(log_likERad_CSphi2  , r_eff=relative_eff(exp(log_likERad_CSphi2)),cores=4,save_psis = TRUE)
print(loo_ERad_CSphi2 )

save(fitERad_CSphi2,log_likERad_CSphi2,loo_ERad_CSphi2,file='ERad_SCensusphi2.RData')

fitERad_CNIphi2 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likERad_CNIphi2   <- extract_log_lik(fitERad_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CNIphi2   <- loo(log_likERad_CNIphi2  , r_eff=relative_eff(exp(log_likERad_CNIphi2)),cores=4,save_psis = TRUE)
print(loo_ERad_CNIphi2)

save(fitERad_CNIphi2,log_likERad_CNIphi2,loo_ERad_CNIphi2,file='ERad_NICensusphi2.RData')


fitERad_CWphi2 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CWphi2   <- extract_log_lik(fitERad_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CWphi2   <- loo(log_likERad_CWphi2  , r_eff=relative_eff(exp(log_likERad_CWphi2)),cores=4,save_psis = TRUE)
print(loo_ERad_CWphi2)

save(fitERad_CWphi2,log_likERad_CWphi2,loo_ERad_CWphi2,file='ERad_WCensusphi2.RData')

fitERad_CEphi2 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CEphi2   <- extract_log_lik(fitERad_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CEphi2   <- loo(log_likERad_CEphi2  , r_eff=relative_eff(exp(log_likERad_CEphi2)),cores=4,save_psis = TRUE)
print(loo_ERad_CEphi2)

save(fitERad_CEphi2,log_likERad_CEphi2,loo_ERad_CEphi2,file='ERad_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitERad_CSphi3 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CSphi3   <- extract_log_lik(fitERad_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CSphi3   <- loo(log_likERad_CSphi3  , r_eff=relative_eff(exp(log_likERad_CSphi3)),cores=4,save_psis = TRUE)
print(loo_ERad_CSphi3 )

save(fitERad_CSphi3,log_likERad_CSphi3,loo_ERad_CSphi3,file='ERad_SCensusphi3.RData')

fitERad_CNIphi3 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likERad_CNIphi3   <- extract_log_lik(fitERad_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CNIphi3   <- loo(log_likERad_CNIphi3  , r_eff=relative_eff(exp(log_likERad_CNIphi3)),cores=4,save_psis = TRUE)
print(loo_ERad_CNIphi3)

save(fitERad_CNIphi3,log_likERad_CNIphi3,loo_ERad_CNIphi3,file='ERad_NICensusphi3.RData')


fitERad_CWphi3 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CWphi3   <- extract_log_lik(fitERad_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CWphi3   <- loo(log_likERad_CWphi3  , r_eff=relative_eff(exp(log_likERad_CWphi3)),cores=4,save_psis = TRUE)
print(loo_ERad_CWphi3)

save(fitERad_CWphi3,log_likERad_CWphi3,loo_ERad_CWphi3,file='ERad_WCensusphi3.RData')

fitERad_CEphi3 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CEphi3   <- extract_log_lik(fitERad_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CEphi3   <- loo(log_likERad_CEphi3  , r_eff=relative_eff(exp(log_likERad_CEphi3)),cores=4,save_psis = TRUE)
print(loo_ERad_CEphi3)

save(fitERad_CEphi3,log_likERad_CEphi3,loo_ERad_CEphi3,file='ERad_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitERad_CSphi4 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CSphi4   <- extract_log_lik(fitERad_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CSphi4   <- loo(log_likERad_CSphi4  , r_eff=relative_eff(exp(log_likERad_CSphi4)),cores=4,save_psis = TRUE)
print(loo_ERad_CSphi4 )

save(fitERad_CSphi4,log_likERad_CSphi4,loo_ERad_CSphi4,file='ERad_SCensusphi4.RData')

fitERad_CNIphi4 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likERad_CNIphi4   <- extract_log_lik(fitERad_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CNIphi4   <- loo(log_likERad_CNIphi4  , r_eff=relative_eff(exp(log_likERad_CNIphi4)),cores=4,save_psis = TRUE)
print(loo_ERad_CNIphi4)

save(fitERad_CNIphi4,log_likERad_CNIphi4,loo_ERad_CNIphi4,file='ERad_NICensusphi4.RData')


fitERad_CWphi4 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CWphi4   <- extract_log_lik(fitERad_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CWphi4   <- loo(log_likERad_CWphi4  , r_eff=relative_eff(exp(log_likERad_CWphi4)),cores=4,save_psis = TRUE)
print(loo_ERad_CWphi4)

save(fitERad_CWphi4,log_likERad_CWphi4,loo_ERad_CWphi4,file='ERad_WCensusphi4.RData')

fitERad_CEphi4 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CEphi4   <- extract_log_lik(fitERad_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CEphi4   <- loo(log_likERad_CEphi4  , r_eff=relative_eff(exp(log_likERad_CEphi4)),cores=4,save_psis = TRUE)
print(loo_ERad_CEphi4)

save(fitERad_CEphi4,log_likERad_CEphi4,loo_ERad_CEphi4,file='ERad_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitERad_CSphi5 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CSphi5   <- extract_log_lik(fitERad_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CSphi5   <- loo(log_likERad_CSphi5  , r_eff=relative_eff(exp(log_likERad_CSphi5)),cores=4,save_psis = TRUE)
print(loo_ERad_CSphi5 )

save(fitERad_CSphi5,log_likERad_CSphi5,loo_ERad_CSphi5,file='ERad_SCensusphi5.RData')

fitERad_CNIphi5 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4)

log_likERad_CNIphi5   <- extract_log_lik(fitERad_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CNIphi5   <- loo(log_likERad_CNIphi5  , r_eff=relative_eff(exp(log_likERad_CNIphi5)),cores=4,save_psis = TRUE)
print(loo_ERad_CNIphi5)

save(fitERad_CNIphi5,log_likERad_CNIphi5,loo_ERad_CNIphi5,file='ERad_NICensusphi5.RData')


fitERad_CWphi5 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CWphi5   <- extract_log_lik(fitERad_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CWphi5   <- loo(log_likERad_CWphi5  , r_eff=relative_eff(exp(log_likERad_CWphi5)),cores=4,save_psis = TRUE)
print(loo_ERad_CWphi5)

save(fitERad_CWphi5,log_likERad_CWphi5,loo_ERad_CWphi5,file='ERad_WCensusphi5.RData')

fitERad_CEphi5 <- stan(file = '../../stan/ERadnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4)

log_likERad_CEphi5   <- extract_log_lik(fitERad_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CEphi5   <- loo(log_likERad_CEphi5  , r_eff=relative_eff(exp(log_likERad_CEphi5)),cores=4,save_psis = TRUE)
print(loo_ERad_CEphi5)

save(fitERad_CEphi5,log_likERad_CEphi5,loo_ERad_CEphi5,file='ERad_ECensusphi5.RData')



