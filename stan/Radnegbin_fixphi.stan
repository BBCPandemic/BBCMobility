data {
  int no_patches;
  int no_non_zero;
  vector[no_patches] N;
  vector[no_patches] A;
  int<lower=0,upper=1> non_zero[no_patches];
  real<lower=0> r[no_patches,no_patches];
  int<lower=0>  mv[no_patches,no_patches];
  int<lower=0>  flux[no_patches];
  real s[no_patches,no_patches];
  real phi;
}

parameters{
 
}

model {
 
  phi ~ cauchy(0,5);

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
        theta[i] = N[j]*N[i]/((N[j]+s[j,i])*(N[j]+N[i]+s[j,i]));
      }
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
  
  for(j in 1:no_patches)
  {
    real normalise = 0;
    vector[no_patches] theta;
    
    if(non_zero[j]==1)
    {
    
    for(i in 1:no_patches)
    {
      theta[i] = machine_precision();
      if(i!=j){
       theta[i] = N[j]*N[i]/((N[j]+s[j,i])*(N[j]+N[i]+s[j,i]));
      }
      normalise += theta[i];
    }
    
    theta = log(theta) - log(normalise) + log(flux[j]);
    
    log_lik[l]  =  neg_binomial_2_log_lpmf(mv[j] | theta,phi);
    l += 1;
    }
  }
 }
}


