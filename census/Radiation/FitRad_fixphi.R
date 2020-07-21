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

fitRad_CSphi1 <- stan(file = '../../stan/Radnegbin_fixphi.stan',
                       data = scotland_census_mobility_dat,
                       iter = 2000, chains = 4,algorithm='Fixed_param')

log_likRad_CSphi1   <- extract_log_lik(fitRad_CSphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CSphi1   <- loo(log_likRad_CSphi1  , r_eff=relative_eff(exp(log_likRad_CSphi1)),cores=4,save_psis = TRUE, is_method='sis')
print(loo_Rad_CSphi1 )

save(fitRad_CSphi1,log_likRad_CSphi1,loo_Rad_CSphi1,file='Rad_SCensusphi1.RData')

fitRad_CNIphi1 <- stan(file = '../../stan/Radnegbin_fixphi.stan',
                        data = NIcensus_mobility_dat,
                        iter = 2000, chains = 4,algorithm='Fixed_param')

log_likRad_CNIphi1   <- extract_log_lik(fitRad_CNIphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CNIphi1   <- loo(log_likRad_CNIphi1  , r_eff=relative_eff(exp(log_likRad_CNIphi1)),cores=4,save_psis = TRUE, is_method='sis')
print(loo_Rad_CNIphi1)

save(fitRad_CNIphi1,log_likRad_CNIphi1,loo_Rad_CNIphi1,file='Rad_NICensusphi1.RData')


fitRad_CWphi1 <- stan(file = '../../stan/Radnegbin_fixphi.stan',
                       data = Wcensus_mobility_dat,
                       iter = 2000, chains = 4,algorithm='Fixed_param')

log_likRad_CWphi1   <- extract_log_lik(fitRad_CWphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CWphi1   <- loo(log_likRad_CWphi1  , r_eff=relative_eff(exp(log_likRad_CWphi1)),cores=4,save_psis = TRUE, is_method='sis')
print(loo_Rad_CWphi1)

save(fitRad_CWphi1,log_likRad_CWphi1,loo_Rad_CWphi1,file='Rad_WCensusphi1.RData')

fitRad_CEphi1 <- stan(file = '../../stan/Radnegbin_fixphi.stan',
                       data = Ecensus_mobility_dat,
                       iter = 2000, chains = 4,algorithm='Fixed_param')

log_likRad_CEphi1   <- extract_log_lik(fitRad_CEphi1  ,c('log_lik'),merge_chains=FALSE)
loo_Rad_CEphi1   <- loo(log_likRad_CEphi1  , r_eff=relative_eff(exp(log_likRad_CEphi1)),cores=4,save_psis = TRUE, is_method='sis')
print(loo_Rad_CEphi1)

save(fitRad_CEphi1,log_likRad_CEphi1,loo_Rad_CEphi1,file='Rad_ECensusphi1.RData')

