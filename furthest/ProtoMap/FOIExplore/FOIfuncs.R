mkflux_mats <-function(theta, move_m, N)
{
  Cg = length(theta)
  if(length(move_m)!=Cg){return(NA)}
  P  = dim(move_m[[1]])[1]
  effectiveN = numeric(P)
  
  move_sums = lapply(move_m,rowSums)
  
  flux_term = lapply(1:Cg,function(g){1.0/((1+theta[[g]]*move_sums[[g]]))})
  
  fluxmat = lapply(1:Cg,function(x){matrix(0,P,P)})
  for(g in 1:Cg)
  {
    for(i in 1:P)
    {
      effectiveN[i] = effectiveN[i] + N[[g]][i]/(1+theta[g]*move_sums[[g]][i])
      for(j in 1:P)
      {
        effectiveN[i] = effectiveN[i] + theta[g]*move_m[[g]][j,i]*N[[g]][j]/(1+theta[g]*move_sums[[g]][j])
        fluxmat[[g]][i,j] = theta[g]*move_m[[g]][i,j]*flux_term[[g]][j]*flux_term[[g]][i]
      }
    }
    flux_term[[g]] = flux_term[[g]]^2
  }
  return(list(effectiveN=effectiveN,flux_term=flux_term,fluxmat=fluxmat))
}

calculate_FOI <- function(theta,move_m,N,I)
{
  Cg = length(theta)
  if(length(move_m)!=Cg){return(NA)}
  P  = dim(move_m[[1]])[1]
  
  flux_mats <- mkflux_mats(theta,move_m,N)
  
  fluxmat = flux_mats$fluxmat
  flux_term = flux_mats$flux_term
  effectiveN = flux_mats$effectiveN
  
  term1 = matrix(0,P,Cg)
  term2 = matrix(0,P,Cg)
  term3 = matrix(0,P,Cg)
  
  for(j in 1:P)
  {
    for(g in 1:Cg)
    {
      
      for(k in 1:Cg)
      {
        term1[j,g] = term1[j,g] + (I[[k]][j]*flux_term[[k]][j]) / effectiveN[j]
      }
      
      for(i in 1:P)
      {
        Itot = 0
        for(k in 1:Cg)
        {
          term2[j,g] = term2[j,g] + I[[k]][i] * fluxmat[[k]][i,j]/effectiveN[j]
          Itot = Itot + I[[k]][i]
        }
        
        term3[j,g] = term3[j,g] + (fluxmat[[g]][j,i]*Itot)/effectiveN[i]
        
      }
      
    }
    
  }
  
  avg_inf_rate = rowSums(sapply(1:Cg,function(g){(term1[,g] + term2[,g] + term3[,g])*N[[g]]}))
  
  return(list(avg_inf_rate=avg_inf_rate,term1=term1,term2=term2,term3=term3,effectiveN=effectiveN))
  
}