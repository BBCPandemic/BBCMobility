require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

fitCDOR_CE_all <- stan(file = '../../stan/CDORnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDOR_CE_all   <- extract_log_lik(fitCDOR_CE_all  ,c('log_lik'),merge_chains=FALSE)
loo_CDOR_CE_all   <- loo(log_likCDOR_CE_all  , r_eff=relative_eff(exp(log_likCDOR_CE_all)),cores=4,save_psis = TRUE)
print(loo_CDOR_CE_all)

save(fitCDOR_CE_all,log_likCDOR_CE_all,loo_CDOR_CE_all,file='CDOR_ECensus_all.RData')
