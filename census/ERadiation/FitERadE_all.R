require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../flux/census_flux.RData')

fitERad_CE_all <- stan(file = '../../stan/ERadnegbin.stan',
                  data = Ecensus_mobility_dat,
                  iter = 2000, chains = 4)

log_likERad_CE_all   <- extract_log_lik(fitERad_CE_all  ,c('log_lik'),merge_chains=FALSE)
loo_ERad_CE_all   <- loo(log_likERad_CE_all  , r_eff=relative_eff(exp(log_likERad_CE_all)),cores=4,save_psis = TRUE)
print(loo_ERad_CE_all)

save(fitERad_CE_all,log_likERad_CE_all,loo_ERad_CE_all,file='ERad_ECensus_all.RData')
