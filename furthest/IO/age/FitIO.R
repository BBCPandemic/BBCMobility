require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(truncdist)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/age/age_flux_furthest.RData')

# Remove Westminster / City of London / Highlands / Waverley / Chichester

under18_mobility_dat$N = under18_mobility_dat$N[-c(234,243,294,326,346)]
under18_mobility_dat$flux = under18_mobility_dat$flux[-c(234,243,294,326,346)]
under18_mobility_dat$A = under18_mobility_dat$A[-c(234,243,294,326,346)]
under18_mobility_dat$mv = under18_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
under18_mobility_dat$r = under18_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
under18_mobility_dat$s = under18_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
under18_mobility_dat$no_patches = length(under18_mobility_dat$N)
under18_mobility_dat$L = array(c(-1))
under18_mobility_dat$Lno = 0

under18_mobility_dat$non_zero = under18_mobility_dat$flux>0
under18_mobility_dat$no_non_zero = sum(under18_mobility_dat$non_zero)

a18_30_mobility_dat$N = a18_30_mobility_dat$N[-c(234,243,294,326,346)]
a18_30_mobility_dat$flux = a18_30_mobility_dat$flux[-c(234,243,294,326,346)]
a18_30_mobility_dat$A = a18_30_mobility_dat$A[-c(234,243,294,326,346)]
a18_30_mobility_dat$mv = a18_30_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a18_30_mobility_dat$r = a18_30_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a18_30_mobility_dat$s = a18_30_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a18_30_mobility_dat$no_patches = length(a18_30_mobility_dat$N)
a18_30_mobility_dat$L = array(c(-1))
a18_30_mobility_dat$Lno = 0

a18_30_mobility_dat$non_zero = a18_30_mobility_dat$flux>0
a18_30_mobility_dat$no_non_zero = sum(a18_30_mobility_dat$non_zero)

a30_60_mobility_dat$N = a30_60_mobility_dat$N[-c(234,243,294,326,346)]
a30_60_mobility_dat$flux = a30_60_mobility_dat$flux[-c(234,243,294,326,346)]
a30_60_mobility_dat$A = a30_60_mobility_dat$A[-c(234,243,294,326,346)]
a30_60_mobility_dat$mv = a30_60_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a30_60_mobility_dat$r = a30_60_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a30_60_mobility_dat$s = a30_60_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a30_60_mobility_dat$no_patches = length(a30_60_mobility_dat$N)
a30_60_mobility_dat$L = array(c(-1))
a30_60_mobility_dat$Lno = 0

a30_60_mobility_dat$non_zero = a30_60_mobility_dat$flux>0
a30_60_mobility_dat$no_non_zero = sum(a30_60_mobility_dat$non_zero)

a60_100_mobility_dat$N = a60_100_mobility_dat$N[-c(234,243,294,326,346)]
a60_100_mobility_dat$flux = a60_100_mobility_dat$flux[-c(234,243,294,326,346)]
a60_100_mobility_dat$A = a60_100_mobility_dat$A[-c(234,243,294,326,346)]
a60_100_mobility_dat$mv = a60_100_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a60_100_mobility_dat$r = a60_100_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a60_100_mobility_dat$s = a60_100_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
a60_100_mobility_dat$no_patches = length(a60_100_mobility_dat$N)
a60_100_mobility_dat$L = array(c(-1))
a60_100_mobility_dat$Lno = 0

a60_100_mobility_dat$non_zero = a60_100_mobility_dat$flux>0
a60_100_mobility_dat$no_non_zero = sum(a60_100_mobility_dat$non_zero)

fitIOU <- stan(file = '../../../stan/IOnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 4000, chains = 4,
              init=function()
              {list(gamma=runif(1,0,1e-6),
                    phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                    eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIOU <- extract_log_lik(fitIOU,c('log_lik'),merge_chains=FALSE)
loo_IOU <- loo(log_likIOU, r_eff=relative_eff(exp(log_likIOU)),cores=4,save_psis = TRUE)
print(loo_IOU)

save(under18_mobility_dat,fitIOU,log_likIOU,loo_IOU,file='IOUnder18.RData')

fitIO18_30 <- stan(file = '../../../stan/IOnegbin.stan', 
                data = a18_30_mobility_dat, 
                iter = 2000, chains = 4,
                init=function()
                {list(gamma=runif(1,0,1e-6),
                      phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                      eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO18_30 <- extract_log_lik(fitIO18_30,c('log_lik'),merge_chains=FALSE)
loo_IO18_30 <- loo(log_likIO18_30, r_eff=relative_eff(exp(log_likIO18_30)),cores=4,save_psis = TRUE)
print(loo_IO18_30)

save(a18_30_mobility_dat,fitIO18_30,log_likIO18_30,loo_IO18_30,file='IO18_30.RData')

fitIO30_60 <- stan(file = '../../../stan/IOnegbin.stan', 
                    data = a30_60_mobility_dat, 
                    iter = 2000, chains = 4,
                   init=function()
                   {list(gamma=runif(1,0,1e-6),
                         phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                         eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO30_60 <- extract_log_lik(fitIO30_60,c('log_lik'),merge_chains=FALSE)
loo_IO30_60 <- loo(log_likIO30_60, r_eff=relative_eff(exp(log_likIO30_60)),cores=4,save_psis = TRUE)
print(loo_IO30_60)

save(a30_60_mobility_dat,fitIO30_60,log_likIO30_60,loo_IO30_60,file='IO30_60.RData')

fitIO60_100 <- stan(file = '../../../stan/IOnegbin.stan', 
                    data = a60_100_mobility_dat, 
                    iter = 2000, chains = 4,
                    init=function()
                    {list(gamma=runif(1,0,1e-6),
                          phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                          eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIO60_100 <- extract_log_lik(fitIO60_100,c('log_lik'),merge_chains=FALSE)
loo_IO60_100 <- loo(log_likIO60_100, r_eff=relative_eff(exp(log_likIO60_100)),cores=4,save_psis = TRUE)
print(loo_IO60_100)

save(a60_100_mobility_dat,fitIO60_100,log_likIO60_100,loo_IO60_100,file='IO60_100.RData')

