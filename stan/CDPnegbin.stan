data {
  int no_patches;
  int no_non_zero;
  vector[no_patches] N;
  vector[no_patches] A;
  int<lower=0,upper=1> non_zero[no_patches];
  real<lower=0> r[no_patches,no_patches];
  int<lower=0>  mv[no_patches,no_patches];
  int<lower=0>  flux[no_patches];
}

parameters{
  real<lower=0,upper=5> tau;
  real<lower=0,upper=5> rho;
  real<lower=-5,upper=5> delta;
  real<lower=0,upper=5> phi;
}

model {
  vector[no_patches] kappa;

  tau   ~ cauchy(0,5);
  rho   ~ cauchy(0,5);
  delta ~ cauchy(0,5);
  
  phi ~ cauchy(0,5);

  for(i in 1:no_patches)
  {
    kappa[i]=machine_precision();
    for(k in 1:no_patches)
    {
      if(i!=k){kappa[i] += (N[k]^tau)/(r[i,k]^rho);}
    }
  }

  for(j in 1:no_patches)
  {
    vector[no_patches] theta;
    real normalise = 0;
    
    if(non_zero[j]==1)
    {
    for(i in 1:no_patches)
    {
      theta[i] = machine_precision();
      if(i!=j){
          real d1=(N[i]^tau)/(r[j,i]^rho);
          real d2=(N[j]^tau)/(r[j,i]^rho);
        theta[i] = ((kappa[i]-d2)^delta)*d1;}
      normalise += theta[i];
    }

    theta = log(theta) - log(normalise) + log(flux[j]);

    mv[j] ~ neg_binomial_2_log(theta,phi);
    
    }
  }
  
}

generated quantities {
  vector[no_non_zero] log_lik;
  {
  int l=1;
  vector[no_patches] kappa;

  for(i in 1:no_patches)
  {
  kappa[i]=machine_precision();
  for(k in 1:no_patches)
  {
  if(i!=k){kappa[i] += (N[k]^tau)/(r[i,k]^rho);}
  }
  }

  for(j in 1:no_patches){
    vector[no_patches] theta;
    real normalise = 0;
    
  if(non_zero[j]==1)
  {
  for(i in 1:no_patches)
  {
  theta[i] = machine_precision();
  if(i!=j){
  real d1=(N[i]^tau)/(r[j,i]^rho);
  real d2=(N[j]^tau)/(r[j,i]^rho);
  theta[i] = ((kappa[i]-d2)^delta)*d1;}
  normalise += theta[i];
  }

  theta = log(theta) - log(normalise) + log(flux[j]);
    
  log_lik[l]  =  neg_binomial_2_log_lpmf(mv[j] | theta,phi);
  l += 1;
  }
}
}

}
