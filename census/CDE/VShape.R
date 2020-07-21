require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(tidybayes)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
load('../../flux/census_flux.RData')

scotland_census_mobility_dat$phi = 0.5

fitCDE_CS1 <- stan(file = '../../stan/CDEnegbin.stan',
                  data = scotland_census_mobility_dat,
                  iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 0.25

fitCDE_CS2 <- stan(file = '../../stan/CDEnegbin.stan',
                   data = scotland_census_mobility_dat,
                   iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 0.1

fitCDE_CS3 <- stan(file = '../../stan/CDEnegbin.stan',
                   data = scotland_census_mobility_dat,
                   iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 0.05

fitCDE_CS4 <- stan(file = '../../stan/CDEnegbin.stan',
                   data = scotland_census_mobility_dat,
                   iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 1.0

fitCDE_CSP <- stan(file = '../../stan/CDEnegbin.stan',
                   data = scotland_census_mobility_dat,
                   iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 5.0

fitCDE_CSP2 <- stan(file = '../../stan/CDEnegbin.stan',
                   data = scotland_census_mobility_dat,
                   iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 10.0

fitCDE_CSP3 <- stan(file = '../../stan/CDEnegbin.stan',
                    data = scotland_census_mobility_dat,
                    iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 50.0

fitCDE_CSP4 <- stan(file = '../../stan/CDEnegbin.stan',
                    data = scotland_census_mobility_dat,
                    iter = 2000, chains = 4)

scotland_census_mobility_dat$phi = 100.0

fitCDE_CSP5 <- stan(file = '../../stan/CDEnegbin.stan',
                    data = scotland_census_mobility_dat,
                    iter = 2000, chains = 4)

log_likCDE_CS1   <- extract_log_lik(fitCDE_CS1  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CS1   <- loo(log_likCDE_CS1  , r_eff=relative_eff(exp(log_likCDE_CS1)),cores=4,save_psis = TRUE)
print(loo_CDE_CS1)

log_likCDE_CS2   <- extract_log_lik(fitCDE_CS2  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CS2   <- loo(log_likCDE_CS2  , r_eff=relative_eff(exp(log_likCDE_CS2)),cores=4,save_psis = TRUE)
print(loo_CDE_CS2)

log_likCDE_CS3   <- extract_log_lik(fitCDE_CS3  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CS3   <- loo(log_likCDE_CS3  , r_eff=relative_eff(exp(log_likCDE_CS3)),cores=4,save_psis = TRUE)
print(loo_CDE_CS3)

log_likCDE_CS4   <- extract_log_lik(fitCDE_CS4  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CS4   <- loo(log_likCDE_CS4  , r_eff=relative_eff(exp(log_likCDE_CS4)),cores=4,save_psis = TRUE)
print(loo_CDE_CS4)

log_likCDE_CSP   <- extract_log_lik(fitCDE_CSP  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CSP   <- loo(log_likCDE_CSP  , r_eff=relative_eff(exp(log_likCDE_CSP)),cores=4,save_psis = TRUE)
print(loo_CDE_CSP)

log_likCDE_CSP2   <- extract_log_lik(fitCDE_CSP2  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CSP2   <- loo(log_likCDE_CSP2  , r_eff=relative_eff(exp(log_likCDE_CSP2)),cores=4,save_psis = TRUE)
print(loo_CDE_CSP2)

log_likCDE_CSP3   <- extract_log_lik(fitCDE_CSP3  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CSP3   <- loo(log_likCDE_CSP3  , r_eff=relative_eff(exp(log_likCDE_CSP3)),cores=4,save_psis = TRUE)
print(loo_CDE_CSP3)

log_likCDE_CSP4   <- extract_log_lik(fitCDE_CSP4  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CSP4   <- loo(log_likCDE_CSP4  , r_eff=relative_eff(exp(log_likCDE_CSP4)),cores=4,save_psis = TRUE)
print(loo_CDE_CSP4)

log_likCDE_CSP5   <- extract_log_lik(fitCDE_CSP5  ,c('log_lik'),merge_chains=FALSE)
loo_CDE_CSP5   <- loo(log_likCDE_CSP5  , r_eff=relative_eff(exp(log_likCDE_CSP5)),cores=4,save_psis = TRUE)
print(loo_CDE_CSP5)

CDEmatflux <- function(mobility_data,fit,median=FALSE, eta_adjust=FALSE,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho,delta)
  y = numeric(length(N))
  
  if(eta_adjust)
  {
    eta<-extract(fit,pars='eta')$eta
    if(median){y[mobility_data$L[1:(length(mobility_data$L)-1)]]=apply(eta,2,median)}else{
      y[mobility_data$L[1:(length(mobility_data$L)-1)]] = eta[sample(1:dim(eta)[1])[1]];}
  }
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta

  no_patches = length(N)
  theta = matrix(NA,length(N),length(N))
  kappa = numeric(length(N))
  normalise = numeric(length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(i in 1:no_patches)
  {
    kappa[i] = .Machine$double.xmin;
    for(k in 1:no_patches)
    {
      if(i!=k){kappa[i] = kappa[i] + (N[k]^tau)*exp(-r[i,k]/(1000*rho));}
    }
  }
  
  for(j in 1:no_patches)
  {
    normalise[j] = 0;
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin;
      if(i!=j){
        d1=(N[i]^tau)*exp(-r[j,i]/(1000*rho));
        d2=(N[j]^tau)*exp(-r[j,i]/(1000*rho));
        theta[j,i] = exp(y[i])*((kappa[i]-d2)^delta)*d1;}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

CPC <- function(L,Lp)
{

  res = 0;
  
  if(!prod(dim(L)==dim(Lp))){return(NA)}
  
  for(i in 1:dim(L)[1])
  {
    for(j in 1:dim(L)[2])
    {
      
      res = res +  min(L[i,j],Lp[i,j])
    }
  }
  
  return(2*res/(sum(L)+sum(Lp)))
}

canberra <- function(L,Lp)
{
  
  res = 0
  nz=0
  if(!prod(dim(L)==dim(Lp))){return(NA)}
  
  for(i in 1:dim(L)[1])
  {
    for(j in 1:dim(L)[2])
    {
      d = (abs(L[i,j])+abs(Lp[i,j]))
      if(d!=0){
      res = res + abs(L[i,j]-Lp[i,j])/d
      nz=nz+1
      }
      }
  }
  
  return((1/nz)*res)
}

CPCpp <- function(mobility_data,fit,model,data, flux_function, adjust_eta,phi)
{
  
  x<-(t(sapply(1:100,function(i){
    oot <- flux_function(mobility_data,fit,FALSE,adjust_eta,phi)
    cbind(CPC(oot$mean,mobility_data$mv),CPC(oot$sample,mobility_data$mv))
  })))
  
  
  return(tibble(model=model,data=data,sample=1:dim(x)[1],CPCm=as.numeric(x[,1]),CPCpp=as.numeric(x[,2])))
}


ppF1<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CS1,median=T,eta_adjust=FALSE,0.5)
ppF2<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CS2,median=T,eta_adjust=FALSE,0.25)
ppF3<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CS3,median=T,eta_adjust=FALSE,0.1)
ppF4<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CS4,median=T,eta_adjust=FALSE,0.05)
ppFP<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CSP,median=T,eta_adjust=FALSE,1)
ppFP2<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CSP2,median=T,eta_adjust=FALSE,5)
ppFP3<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CSP3,median=T,eta_adjust=FALSE,10)
ppFP4<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CSP4,median=T,eta_adjust=FALSE,50)
ppFP5<-CDEmatflux(scotland_census_mobility_dat,fitCDE_CSP5,median=T,eta_adjust=FALSE,100)

x<-CPC(ppF1$mean,scotland_census_mobility_dat$mv)
x2<-CPC(ppFP5$mean,scotland_census_mobility_dat$mv)


plot(colSums(scotland_census_mobility_dat$mv),colSums(ppF1$mean),pch=19)
abline(a=0,b=1)
points(colSums(scotland_census_mobility_dat$mv),colSums(ppF2$mean),pch=19,col='red')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppF3$mean),pch=19,col='blue')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppF4$mean),pch=19,col='green')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppFP$mean),pch=19,col='pink')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppFP2$mean),pch=19,col='yellow')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppFP3$mean),pch=19,col='red')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppFP4$mean),pch=19,col='blue')
points(colSums(scotland_census_mobility_dat$mv),colSums(ppFP5$mean),pch=19,col='green')


CPC1 <- CPCpp(scotland_census_mobility_dat,fitCDE_CS1,'CDE1','Scotland Census',CDEmatflux,FALSE,0.5)
CPC2 <- CPCpp(scotland_census_mobility_dat,fitCDE_CS2,'CDE2','Scotland Census',CDEmatflux,FALSE,0.25)
CPC3 <- CPCpp(scotland_census_mobility_dat,fitCDE_CS3,'CDE3','Scotland Census',CDEmatflux,FALSE,0.15)
CPC4 <- CPCpp(scotland_census_mobility_dat,fitCDE_CS4,'CDE4','Scotland Census',CDEmatflux,FALSE,0.05)
CPCP <- CPCpp(scotland_census_mobility_dat,fitCDE_CSP,'CDEP','Scotland Census',CDEmatflux,FALSE,1.0)
CPCP2 <- CPCpp(scotland_census_mobility_dat,fitCDE_CSP2,'CDEP','Scotland Census',CDEmatflux,FALSE,5.0)
CPCP3 <- CPCpp(scotland_census_mobility_dat,fitCDE_CSP3,'CDEP','Scotland Census',CDEmatflux,FALSE,10.0)
CPCP4 <- CPCpp(scotland_census_mobility_dat,fitCDE_CSP4,'CDEP','Scotland Census',CDEmatflux,FALSE,50.0)
CPCP5 <- CPCpp(scotland_census_mobility_dat,fitCDE_CSP5,'CDEP','Scotland Census',CDEmatflux,FALSE,100.0)

