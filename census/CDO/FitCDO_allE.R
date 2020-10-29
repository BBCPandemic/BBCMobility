require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

fitCDO_CE_all <- stan(file = '../../stan/CDOnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 4000, chains = 4)

log_likCDO_CE_all   <- extract_log_lik(fitCDO_CE_all  ,c('log_lik'),merge_chains=FALSE)
loo_CDO_CE_all   <- loo(log_likCDO_CE_all  , r_eff=relative_eff(exp(log_likCDO_CE_all)),cores=4,save_psis = TRUE)
print(loo_CDO_CE_all)

save(fitCDO_CE_all,log_likCDO_CE_all,loo_CDO_CE_all,file='CDO_ECensus_all.RData')