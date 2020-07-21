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

fitImp_CSphi1 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                       data = scotland_census_mobility_dat,
                       iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CSphi1   <- extract_log_lik(fitImp_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CSphi1   <- loo(log_likImp_CSphi1  , r_eff=relative_eff(exp(log_likImp_CSphi1)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CSphi1 )

save(fitImp_CSphi1,log_likImp_CSphi1,loo_Imp_CSphi1,file='Imp_SCensusphi1.RData')

fitImp_CNIphi1 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                        data = NIcensus_mobility_dat,
                        iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CNIphi1   <- extract_log_lik(fitImp_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CNIphi1   <- loo(log_likImp_CNIphi1  , r_eff=relative_eff(exp(log_likImp_CNIphi1)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CNIphi1)

save(fitImp_CNIphi1,log_likImp_CNIphi1,loo_Imp_CNIphi1,file='Imp_NICensusphi1.RData')


fitImp_CWphi1 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                       data = Wcensus_mobility_dat,
                       iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CWphi1   <- extract_log_lik(fitImp_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CWphi1   <- loo(log_likImp_CWphi1  , r_eff=relative_eff(exp(log_likImp_CWphi1)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CWphi1)

save(fitImp_CWphi1,log_likImp_CWphi1,loo_Imp_CWphi1,file='Imp_WCensusphi1.RData')

fitImp_CEphi1 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                       data = Ecensus_mobility_dat,
                       iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CEphi1   <- extract_log_lik(fitImp_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CEphi1   <- loo(log_likImp_CEphi1  , r_eff=relative_eff(exp(log_likImp_CEphi1)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CEphi1)

save(fitImp_CEphi1,log_likImp_CEphi1,loo_Imp_CEphi1,file='Imp_ECensusphi1.RData')

Ecensus_mobility_dat$phi = 0.2
Wcensus_mobility_dat$phi = 0.2
NIcensus_mobility_dat$phi = 0.2
scotland_census_mobility_dat$phi = 0.2

fitImp_CSphi2 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CSphi2   <- extract_log_lik(fitImp_CSphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CSphi2   <- loo(log_likImp_CSphi2  , r_eff=relative_eff(exp(log_likImp_CSphi2)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CSphi2 )

save(fitImp_CSphi2,log_likImp_CSphi2,loo_Imp_CSphi2,file='Imp_SCensusphi2.RData')

fitImp_CNIphi2 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CNIphi2   <- extract_log_lik(fitImp_CNIphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CNIphi2   <- loo(log_likImp_CNIphi2  , r_eff=relative_eff(exp(log_likImp_CNIphi2)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CNIphi2)

save(fitImp_CNIphi2,log_likImp_CNIphi2,loo_Imp_CNIphi2,file='Imp_NICensusphi2.RData')


fitImp_CWphi2 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CWphi2   <- extract_log_lik(fitImp_CWphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CWphi2   <- loo(log_likImp_CWphi2  , r_eff=relative_eff(exp(log_likImp_CWphi2)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CWphi2)

save(fitImp_CWphi2,log_likImp_CWphi2,loo_Imp_CWphi2,file='Imp_WCensusphi2.RData')

fitImp_CEphi2 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CEphi2   <- extract_log_lik(fitImp_CEphi2  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CEphi2   <- loo(log_likImp_CEphi2  , r_eff=relative_eff(exp(log_likImp_CEphi2)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CEphi2)

save(fitImp_CEphi2,log_likImp_CEphi2,loo_Imp_CEphi2,file='Imp_ECensusphi2.RData')

Ecensus_mobility_dat$phi = 0.3
Wcensus_mobility_dat$phi = 0.3
NIcensus_mobility_dat$phi = 0.3
scotland_census_mobility_dat$phi = 0.3

fitImp_CSphi3 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CSphi3   <- extract_log_lik(fitImp_CSphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CSphi3   <- loo(log_likImp_CSphi3  , r_eff=relative_eff(exp(log_likImp_CSphi3)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CSphi3 )

save(fitImp_CSphi3,log_likImp_CSphi3,loo_Imp_CSphi3,file='Imp_SCensusphi3.RData')

fitImp_CNIphi3 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CNIphi3   <- extract_log_lik(fitImp_CNIphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CNIphi3   <- loo(log_likImp_CNIphi3  , r_eff=relative_eff(exp(log_likImp_CNIphi3)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CNIphi3)

save(fitImp_CNIphi3,log_likImp_CNIphi3,loo_Imp_CNIphi3,file='Imp_NICensusphi3.RData')


fitImp_CWphi3 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CWphi3   <- extract_log_lik(fitImp_CWphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CWphi3   <- loo(log_likImp_CWphi3  , r_eff=relative_eff(exp(log_likImp_CWphi3)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CWphi3)

save(fitImp_CWphi3,log_likImp_CWphi3,loo_Imp_CWphi3,file='Imp_WCensusphi3.RData')

fitImp_CEphi3 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CEphi3   <- extract_log_lik(fitImp_CEphi3  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CEphi3   <- loo(log_likImp_CEphi3  , r_eff=relative_eff(exp(log_likImp_CEphi3)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CEphi3)

save(fitImp_CEphi3,log_likImp_CEphi3,loo_Imp_CEphi3,file='Imp_ECensusphi3.RData')

Ecensus_mobility_dat$phi = 0.4
Wcensus_mobility_dat$phi = 0.4
NIcensus_mobility_dat$phi = 0.4
scotland_census_mobility_dat$phi = 0.4

fitImp_CSphi4 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CSphi4   <- extract_log_lik(fitImp_CSphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CSphi4   <- loo(log_likImp_CSphi4  , r_eff=relative_eff(exp(log_likImp_CSphi4)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CSphi4 )

save(fitImp_CSphi4,log_likImp_CSphi4,loo_Imp_CSphi4,file='Imp_SCensusphi4.RData')

fitImp_CNIphi4 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CNIphi4   <- extract_log_lik(fitImp_CNIphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CNIphi4   <- loo(log_likImp_CNIphi4  , r_eff=relative_eff(exp(log_likImp_CNIphi4)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CNIphi4)

save(fitImp_CNIphi4,log_likImp_CNIphi4,loo_Imp_CNIphi4,file='Imp_NICensusphi4.RData')


fitImp_CWphi4 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CWphi4   <- extract_log_lik(fitImp_CWphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CWphi4   <- loo(log_likImp_CWphi4  , r_eff=relative_eff(exp(log_likImp_CWphi4)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CWphi4)

save(fitImp_CWphi4,log_likImp_CWphi4,loo_Imp_CWphi4,file='Imp_WCensusphi4.RData')

fitImp_CEphi4 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CEphi4   <- extract_log_lik(fitImp_CEphi4  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CEphi4   <- loo(log_likImp_CEphi4  , r_eff=relative_eff(exp(log_likImp_CEphi4)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CEphi4)

save(fitImp_CEphi4,log_likImp_CEphi4,loo_Imp_CEphi4,file='Imp_ECensusphi4.RData')

Ecensus_mobility_dat$phi = 0.5
Wcensus_mobility_dat$phi = 0.5
NIcensus_mobility_dat$phi = 0.5
scotland_census_mobility_dat$phi = 0.5

fitImp_CSphi5 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = scotland_census_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CSphi5   <- extract_log_lik(fitImp_CSphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CSphi5   <- loo(log_likImp_CSphi5  , r_eff=relative_eff(exp(log_likImp_CSphi5)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CSphi5 )

save(fitImp_CSphi5,log_likImp_CSphi5,loo_Imp_CSphi5,file='Imp_SCensusphi5.RData')

fitImp_CNIphi5 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                      data = NIcensus_mobility_dat,
                      iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CNIphi5   <- extract_log_lik(fitImp_CNIphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CNIphi5   <- loo(log_likImp_CNIphi5  , r_eff=relative_eff(exp(log_likImp_CNIphi5)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CNIphi5)

save(fitImp_CNIphi5,log_likImp_CNIphi5,loo_Imp_CNIphi5,file='Imp_NICensusphi5.RData')


fitImp_CWphi5 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Wcensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CWphi5   <- extract_log_lik(fitImp_CWphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CWphi5   <- loo(log_likImp_CWphi5  , r_eff=relative_eff(exp(log_likImp_CWphi5)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CWphi5)

save(fitImp_CWphi5,log_likImp_CWphi5,loo_Imp_CWphi5,file='Imp_WCensusphi5.RData')

fitImp_CEphi5 <- stan(file = '../../stan/Impnegbin_fixphi.stan',
                     data = Ecensus_mobility_dat,
                     iter = 2000, chains = 4,algorithm='Fixed_param')

log_likImp_CEphi5   <- extract_log_lik(fitImp_CEphi5  ,c('log_lik'),merge_chains=FALSE)
loo_Imp_CEphi5   <- loo(log_likImp_CEphi5  , r_eff=relative_eff(exp(log_likImp_CEphi5)),cores=4,save_psis = TRUE,is_method='sis')
print(loo_Imp_CEphi5)

save(fitImp_CEphi5,log_likImp_CEphi5,loo_Imp_CEphi5,file='Imp_ECensusphi5.RData')



