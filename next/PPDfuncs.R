# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
CDPmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
  
  post <- fit %>% spread_draws(tau,rho,delta,phi)
  y = numeric(length(N))
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
CDOmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r

  post <- fit %>% spread_draws(tau,rho,delta,alpha,phi)
  y = numeric(length(N))
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),alpha=median(alpha),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  alpha  = post$alpha
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
CDEmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
  
  post <- fit %>% spread_draws(tau,rho,delta,phi)
  y = numeric(length(N))
  
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
GPmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
  
  post <- fit %>% spread_draws(tau,rho,phi)
  y = numeric(length(N))
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}
# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
CDOmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
  
  post <- fit %>% spread_draws(tau,rho,delta,alpha,phi)
  y = numeric(length(N))
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),alpha=median(alpha),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  alpha  = post$alpha
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}
# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
CDEmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r

  post <- fit %>% spread_draws(tau,rho,delta,phi)
  y = numeric(length(N))
  
  if(median){ post <- post %>% summarise(tau=median(tau),rho=median(rho),delta=median(delta),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  tau    = post$tau
  rho    = post$rho
  delta  = post$delta
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
Impmovemat <- function(mobility_data,fit,median=FALSE)
{
  
  N = mobility_data$N
  r = mobility_data$r
  post <- fit %>% spread_draws(phi)
  y = numeric(length(N))

  if(median){ post <- post %>% summarise(phi=median(phi)) ;
  }else
  {post <- sample_n(post,1)}
  
  phi    = post$phi
  
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
    
    theta[j,] = theta[j,]/normalise[j]
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}

# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
ERadmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
  s = mobility_data$s
  post <- fit %>% spread_draws(alpha,phi)
  y = numeric(length(N))
  
  if(median){ post <- post %>% summarise(alpha=median(alpha),phi=median(phi));
  }else
  {post <- sample_n(post,1)}
  
  phi    = post$phi
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
    
    theta[j,] = theta[j,]/normalise
    
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}
# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
Stomatflux <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
  s = mobility_data$s
  y = numeric(length(N))
  post <- fit %>% spread_draws(tau,phi)
  
  if(median){ post %>% summarise(tau=median(tau),phi=median(phi));
  }else
    {post <- sample_n(post,1)}
  
  phi    = post$phi
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
    
    theta[j,] = theta[j,]/normalise
    mv[j,] = rnbinom(length(N),mu=theta[j,],size = phi)
  }
  
  return(list(mean=theta,sample=mv))
}
# Returns conditional movement matrix with rows indexing "from"
# columns indexing "to"
IOmovemat <- function(mobility_data,fit,median=FALSE)
{
  N = mobility_data$N
  r = mobility_data$r
 
  s = mobility_data$s
  y = numeric(length(N))
  post <- fit %>% spread_draws(gamma,phi)
  
  if(median){ post %>% summarise(gamma=median(gamma),phi=median(phi));
  }else
    {post <- sample_n(post,1)}
  
  phi    = post$phi
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
    
    theta[j,] = theta[j,]/normalise
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

CPCpp <- function(mobility_data,fit,model,data, move_function)
{
  
  x<-(t(sapply(1:100,function(i){
    oot <- move_function(mobility_data,fit,FALSE)
    cbind(CPC(oot$mean,mobility_data$mv),CPC(oot$sample,mobility_data$mv))
  })))
  
  
  return(tibble(model=model,data=data,sample=1:dim(x)[1],CPCm=as.numeric(x[,1]),CPCpp=as.numeric(x[,2])))
}