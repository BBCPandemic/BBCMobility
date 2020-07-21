CDPmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho,delta)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
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
      if(i!=k){kappa[i] = kappa[i] + (N[k]^tau)/(r[i,k]^rho);}
    }
  }
  
  for(j in 1:no_patches)
  {
    normalise[j] = 0;
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin;
      if(i!=j){
        d1=(N[i]^tau)/(r[j,i]^rho);
        d2=(N[j]^tau)/(r[j,i]^rho);
        theta[j,i] = ((kappa[i]-d2)^delta)*d1;}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

CDOmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho,delta,alpha)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),alpha=median(alpha));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  alpha  = post$alpha
  
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
      if(i!=k){kappa[i] = kappa[i] + (N[k]^tau)/((1+r[i,k]/(1000*rho*alpha))^alpha);}
    }
  }
  
  for(j in 1:no_patches)
  {
    normalise[j] = 0;
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin;
      if(i!=j){
        d1=(N[i]^tau)/((1+r[j,i]/(1000*rho*alpha))^alpha);
        d2=(N[j]^tau)/((1+r[j,i]/(1000*rho*alpha))^alpha);
        theta[j,i] = ((kappa[i]-d2)^delta)*d1;}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

CDEmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho,delta)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),phi=median(phi));
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
        theta[j,i] = ((kappa[i]-d2)^delta)*d1;}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

GPmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  
  no_patches = length(N)
  theta = matrix(NA,length(N),length(N))
  normalise = numeric(length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(j in 1:no_patches)
  {
    normalise[j] = 0;
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin;
      if(i!=j){
        theta[j,i] = (N[i]^tau)/(r[j,i]^rho);}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

CDOmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho,delta,alpha)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),alpha=median(alpha));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  alpha  = post$alpha
  
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
      if(i!=k){kappa[i] = kappa[i] + (N[k]^tau)/((1+r[i,k]/(1000*rho*alpha))^alpha);}
    }
  }
  
  for(j in 1:no_patches)
  {
    normalise[j] = 0;
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin;
      if(i!=j){
        d1=(N[i]^tau)/((1+r[j,i]/(1000*rho*alpha))^alpha);
        d2=(N[j]^tau)/((1+r[j,i]/(1000*rho*alpha))^alpha);
        theta[j,i] = ((kappa[i]-d2)^delta)*d1;}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

CDEmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  post <- fit %>% spread_draws(tau,rho,delta)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
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
        theta[j,i] = ((kappa[i]-d2)^delta)*d1;}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}


Impmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux

  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}

  no_patches = length(N)
  theta = matrix(NA,length(N),length(N))
  kappa = numeric(length(N))
  normalise = numeric(length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(j in 1:no_patches)
  {
    normalise[j] = 0;
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin;
      if(i!=j){
        theta[j,i] = (N[i]+N[j])/(r[j,i]);}
      normalise[j] = normalise[j] + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}


ERadmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  s = mobility_data$s
  post <- fit %>% spread_draws(alpha)
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post <- post %>% summarise(alpha=median(alpha));
  }else
  {post <- sample_n(post,1)}
  
  alpha  = post$alpha
  no_patches = length(N)
  
  theta = matrix(NA,length(N),length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(j in 1:no_patches)
  {
    
    normalise = 0;
    
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin
      if(i!=j){
        theta[j,i] = (((s[j,i]+N[j]+N[i])^alpha - (s[j,i] + N[j])^alpha)*(N[j]^alpha + 1));  
        theta[j,i] = theta[j,i]/(((s[j,i]+N[j])^alpha + 1)*((s[j,i] + N[j] + N[i])^alpha + 1));
      }
      normalise = normalise+theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise
    
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

Radmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = as.numeric(mobility_data$N)
  r = mobility_data$r
  flux = mobility_data$flux
  s = mobility_data$s
  y = numeric(length(N))
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  

  no_patches = length(N)
  
  theta = matrix(NA,length(N),length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(j in 1:no_patches)
  {
    
    normalise = 0;
    
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin
      if(i!=j){
        theta[j,i] = N[j]*N[i]/((N[j]+s[j,i])*(N[j]+N[i]+s[j,i])); 
      }
      normalise = normalise+theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise
    
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

Stomatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  s = mobility_data$s
  y = numeric(length(N))
  post <- fit %>% spread_draws(tau)
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post %>% summarise(tau=median(tau));
  }else
    {post <- sample_n(post,1)}
  
  tau  = post$tau
  no_patches = length(N)
  
  theta = matrix(NA,length(N),length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(j in 1:no_patches)
  {
    
    normalise = 0;
    
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin
      if(i!=j){theta[j,i] = exp(tau*(log(N[i])-log(s[j,i])));}
      normalise = normalise + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

IOmatflux <- function(mobility_data,fit,median=FALSE,post_flux=TRUE,pmove=NA,phi)
{
  N = mobility_data$N
  r = mobility_data$r
  flux = mobility_data$flux
  s = mobility_data$s
  y = numeric(length(N))
  post <- fit %>% spread_draws(gamma)
  
  if(!post_flux){if(is.na(pmove)){flux=rep(1,length(flux))}else{flux=pmove*N}}
  
  if(median){ post %>% summarise(gamma=median(gamma));
  }else
    {post <- sample_n(post,1)}
  
  gamma  = post$gamma
  no_patches = length(N)
  
  theta = matrix(NA,length(N),length(N))
  mv = matrix(NA,length(N),length(N))
  
  for(j in 1:no_patches)
  {
    
    normalise = 0;
    
    for(i in 1:no_patches)
    {
      theta[j,i] = .Machine$double.xmin
      if(i!=j){theta[j,i] = exp(-(gamma)*s[j,i]) - exp(-(gamma)*(s[j,i]+N[i]));}
      normalise = normalise + theta[j,i];
    }
    
    theta[j,] = flux[j]*theta[j,]/normalise
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
      res = res + min(L[i,j],Lp[i,j])
    }
  }
  
  return(2*res/(sum(L)+sum(Lp)))
}


CPCpp <- function(mobility_data,fit,model,data, flux_function,phi)
{
  
  x<-(t(sapply(1:100,function(i){
    oot <- flux_function(mobility_data,fit,FALSE,TRUE,NA,phi)
    cbind(CPC(oot$mean,mobility_data$mv),CPC(oot$sample,mobility_data$mv))
  })))
  

  oot = tibble(model=model,data=data,sample=1:dim(x)[1],
               CPCm=as.numeric(x[,1]),CPCpp=as.numeric(x[,2]))
  
  return(oot)
}