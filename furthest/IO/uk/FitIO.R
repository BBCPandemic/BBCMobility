require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(truncdist)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

load('../../../flux/mobility_flux_furthest.RData')

total_mobility_dat$N = total_mobility_dat$N[-c(234,243,294,326,346)]
total_mobility_dat$flux = total_mobility_dat$flux[-c(234,243,294,326,346)]
total_mobility_dat$A = total_mobility_dat$A[-c(234,243,294,326,346)]
total_mobility_dat$mv = total_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
total_mobility_dat$r = total_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
total_mobility_dat$s = total_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
total_mobility_dat$no_patches = length(total_mobility_dat$N)
total_mobility_dat$L = array(c(-1))
total_mobility_dat$Lno = 0

total_mobility_dat$non_zero = total_mobility_dat$flux>0
total_mobility_dat$no_non_zero = sum(total_mobility_dat$non_zero)

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

education_mobility_dat$N = education_mobility_dat$N[-c(234,243,294,326,346)]
education_mobility_dat$flux = education_mobility_dat$flux[-c(234,243,294,326,346)]
education_mobility_dat$A = education_mobility_dat$A[-c(234,243,294,326,346)]
education_mobility_dat$mv = education_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
education_mobility_dat$r = education_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
education_mobility_dat$s = education_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
education_mobility_dat$no_patches = length(education_mobility_dat$N)
education_mobility_dat$L = array(c(-1))
education_mobility_dat$Lno = 0

education_mobility_dat$non_zero = education_mobility_dat$flux>0
education_mobility_dat$no_non_zero = sum(education_mobility_dat$non_zero)

employed_mobility_dat$N = employed_mobility_dat$N[-c(234,243,294,326,346)]
employed_mobility_dat$flux = employed_mobility_dat$flux[-c(234,243,294,326,346)]
employed_mobility_dat$A = employed_mobility_dat$A[-c(234,243,294,326,346)]
employed_mobility_dat$mv = employed_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
employed_mobility_dat$r = employed_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
employed_mobility_dat$s = employed_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
employed_mobility_dat$no_patches = length(employed_mobility_dat$N)
employed_mobility_dat$L = array(c(-1))
employed_mobility_dat$Lno = 0

employed_mobility_dat$non_zero = employed_mobility_dat$flux>0
employed_mobility_dat$no_non_zero = sum(employed_mobility_dat$non_zero)

neet_mobility_dat$N = neet_mobility_dat$N[-c(234,243,294,326,346)]
neet_mobility_dat$flux = neet_mobility_dat$flux[-c(234,243,294,326,346)]
neet_mobility_dat$A = neet_mobility_dat$A[-c(234,243,294,326,346)]
neet_mobility_dat$mv = neet_mobility_dat$mv[-c(234,243,294,326,346),-c(234,243,294,326,346)]
neet_mobility_dat$r = neet_mobility_dat$r[-c(234,243,294,326,346),-c(234,243,294,326,346)]
neet_mobility_dat$s = neet_mobility_dat$s[-c(234,243,294,326,346),-c(234,243,294,326,346)]
neet_mobility_dat$no_patches = length(neet_mobility_dat$N)
neet_mobility_dat$L = array(c(-1))
neet_mobility_dat$Lno = 0

neet_mobility_dat$non_zero = neet_mobility_dat$flux>0
neet_mobility_dat$no_non_zero = sum(neet_mobility_dat$non_zero)

fitIOT <- stan(file = '../../../stan/IOnegbin.stan', 
               data = total_mobility_dat, 
               iter = 2000, chains = 4,
               init=function()
               {list(gamma=runif(1,0,1e-6),
                     phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                     eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIOT <- extract_log_lik(fitIOT,c('log_lik'),merge_chains=FALSE)
loo_IOT <- loo(log_likIOT, r_eff=relative_eff(exp(log_likIOT)),cores=4,save_psis = TRUE)
print(loo_IOT)


save(total_mobility_dat,fitIOT,log_likIOT,loo_IOT,file='IOTotal.RData')

fitIOU <- stan(file = '../../../stan/IOnegbin.stan', 
              data = under18_mobility_dat, 
              iter = 2000, chains = 4,
              init=function()
              {list(gamma=runif(1,0,1e-6),
                    phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                    eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIOU <- extract_log_lik(fitIOU,c('log_lik'),merge_chains=FALSE)
loo_IOU <- loo(log_likIOU, r_eff=relative_eff(exp(log_likIOU)),cores=4,save_psis = TRUE)
print(loo_IOU)


save(under18_mobility_dat,fitIOU,log_likIOU,loo_IOU,file='IOUnder18.RData')

fitIOEd <- stan(file = '../../../stan/IOnegbin.stan', 
               data = education_mobility_dat, 
               iter = 2000, chains = 4,
               init=function()
               {list(gamma=runif(1,0,1e-6),
                     phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                     eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIOEd <- extract_log_lik(fitIOEd,c('log_lik'),merge_chains=FALSE)
loo_IOEd <- loo(log_likIOEd, r_eff=relative_eff(exp(log_likIOEd)),cores=4,save_psis = TRUE)
print(loo_IOEd)


save(fitIOEd,log_likIOEd,loo_IOEd,file='IOEducation.RData')


fitIOEm <- stan(file = '../../../stan/IOnegbin.stan', 
                data = employed_mobility_dat, 
                iter = 2000, chains = 4,
                init=function()
                {list(gamma=runif(1,0,1e-6),
                      phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                      eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likIOEm <- extract_log_lik(fitIOEm,c('log_lik'),merge_chains=FALSE)
loo_IOEm <- loo(log_likIOEm, r_eff=relative_eff(exp(log_likIOEm)),cores=4,save_psis = TRUE)
print(loo_IOEm)


save(employed_mobility_dat,fitIOEm,log_likIOEm,loo_IOEm,file='IOEmployed.RData')


fitION <- stan(file = '../../../stan/IOnegbin.stan', 
                data = neet_mobility_dat, 
                iter = 2000, chains = 4,
               init=function()
               {list(gamma=runif(1,0,1e-6),
                     phi=rtrunc(1,'cauchy',a=0,b=5,location=0,scale=5),
                     eta=array(rtrunc(2,'cauchy',a=0,b=5,location=0,scale=1),dim=c(2)))})

log_likION <- extract_log_lik(fitION,c('log_lik'),merge_chains=FALSE)
loo_ION <- loo(log_likION, r_eff=relative_eff(exp(log_likION)),cores=4,save_psis = TRUE)
print(loo_ION)

save(neet_mobility_dat,fitION,log_likION,loo_ION,file='IONEET.RData')


