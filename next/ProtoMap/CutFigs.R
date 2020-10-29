## Risk difference between imputed Census and BBC commuter flows

```{r meta_FOI,echo=FALSE,message=FALSE,warning=FALSE}
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


lads_map_crunch <- st_simplify(lads_map,preserveTopology = TRUE,dTolerance=1000)
ukoutline_crunch <- st_simplify(ukoutline,preserveTopology = TRUE,dTolerance=1000)
save(lads_map_crunch,ukoutline_crunch,
     ppCDOU_move,ppCDO18_30_move,ppCDO30_60_move,ppCDO60_100_move,
     calculate_FOI,mkflux_mats,file='./ProtoMap/FOIExplore/ShinyDat.RData')


```

```{r risk_map_Haslemere, echo=FALSE,message=FALSE, fig.path='Figs/',fig.width=5, fig.height=5}

I0=numeric(391)
I0[234]=1

foi_gC<-calculate_FOI(theta=c(1.0),
                      move_m=list(ppCDOCE$mean/lads_map$all),
                      N=list(lads_map$all),
                      I=list(I0)) 

# I0a=numeric(391)
# I0a[234] = sum(lads_map$under18)/sum(lads_map$all)
# 
# I0b=numeric(391)
# I0b[234] = sum(lads_map$a18_30)/sum(lads_map$all)
# 
# I0c=numeric(391)
# I0c[234] = sum(lads_map$a30_60)/sum(lads_map$all)
# 
# I0d=numeric(391)
# I0d[234] = sum(lads_map$a60_100)/sum(lads_map$all)


foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                      move_m=list(ppCDOU$mean / lads_map$under18,
                                  ppCDO18_30$mean / lads_map$a18_30,
                                  ppCDO30_60$mean / lads_map$a30_60,
                                  ppCDO60_100$mean / lads_map$a60_100),
                      N=list(lads_map$under18,lads_map$a18_30,
                             lads_map$a30_60,lads_map$a60_100),
                      I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 

foi_gCR<-calculate_FOI(theta=c(1.0),
                       move_m=list(census_flux/lads_map$all),
                       N=list(lads_map$all),
                       I=list(I0)) 

foi_gT<-calculate_FOI(theta=c(1.0),
                      move_m=list(ppCDOT$mean/lads_map$all),
                      N=list(lads_map$all),
                      I=list(I0)) 

foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                      move_m=list(ppCDOU$mean / lads_map$under18,
                                  ppCDO18_30$mean / lads_map$a18_30,
                                  ppCDO30_60$mean / lads_map$a30_60,
                                  ppCDO60_100$mean / lads_map$a60_100),
                      N=list(lads_map$under18,lads_map$a18_30,
                             lads_map$a30_60,lads_map$a60_100),
                      I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 

foi_g18_30<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                          move_m=list(ppCDOU$mean / lads_map$under18,
                                      ppCDO18_30$mean / lads_map$a18_30,
                                      ppCDO30_60$mean / lads_map$a30_60,
                                      ppCDO60_100$mean / lads_map$a60_100),
                          N=list(lads_map$under18,lads_map$a18_30,
                                 lads_map$a30_60,lads_map$a60_100),
                          I=list(rep(0,391),I0,rep(0,391),rep(0,391)) ) 

foi_g30_60<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                          move_m=list(ppCDOU$mean / lads_map$under18,
                                      ppCDO18_30$mean / lads_map$a18_30,
                                      ppCDO30_60$mean / lads_map$a30_60,
                                      ppCDO60_100$mean / lads_map$a60_100),
                          N=list(lads_map$under18,lads_map$a18_30,
                                 lads_map$a30_60,lads_map$a60_100),
                          I=list(rep(0,391),rep(0,391),I0,rep(0,391)) ) 

foi_g60_100<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                           move_m=list(ppCDOU$mean / lads_map$under18,
                                       ppCDO18_30$mean / lads_map$a18_30,
                                       ppCDO30_60$mean / lads_map$a30_60,
                                       ppCDO60_100$mean / lads_map$a60_100),
                           N=list(lads_map$under18,lads_map$a18_30,
                                  lads_map$a30_60,lads_map$a60_100),
                           I=list(rep(0,391),rep(0,391),rep(0,391),I0) ) 

z = lads_map %>% mutate(lC = (foi_gC$avg_inf_rate))
z = z %>% mutate(lT = (foi_gT$avg_inf_rate),
                 lU = ((foi_gU$avg_inf_rate)-lT),
                 l18_30 = ((foi_g18_30$avg_inf_rate)-lT),
                 l30_60 = ((foi_g30_60$avg_inf_rate)-lT),
                 l60_100 = ((foi_g60_100$avg_inf_rate)-lT))

max_val = max(c(z$lU[-234],z$l18_30[-234],z$l30_60[-234],z$l60_100[-234]),na.rm=T)
min_val = min(c(z$lU[-234],z$l18_30[-234],z$l30_60[-234],z$l60_100[-234]),na.rm=T)


#scale_fill_gradient2(name ='Relative Risk',trans='sqrt',midpoint=1,low=muted('blue'),high=muted('red'),limits=c(min_val,max_val)) + 
p1=ggplot(st_sf(z[-234,]),aes(fill=lU)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC Under 18')+ theme(text = element_text(size=8))+ 
  geom_sf(data=ukoutline,fill=NA,size=0.01) +
  xlim(350000,550000) + ylim(50000,250000)+
  geom_sf(data=st_sf(z[234,]),fill=NA,size=0.25)

#c(343,351,354)

p2=ggplot(st_sf(z[-234,]),aes(fill=l18_30)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 18-30')+ theme(text = element_text(size=8))+ 
  geom_sf(data=ukoutline,fill=NA,size=0.01) +
  xlim(350000,550000) + ylim(50000,250000)+
  geom_sf(data=st_sf(z[234,]),fill=NA,size=0.25)


p3=ggplot(st_sf(z[-234,]),aes(fill=l30_60)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 30-60')+ theme(text = element_text(size=8))+ 
  geom_sf(data=ukoutline,fill=NA,size=0.01) +
  xlim(350000,550000) + ylim(50000,250000)+
  geom_sf(data=st_sf(z[234,]),fill=NA,size=0.25)


p4=ggplot(st_sf(z[-234,]),aes(fill=l60_100)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 60-100')+ theme(text = element_text(size=8))+ 
  geom_sf(data=ukoutline,fill=NA,size=0.01) +
  xlim(350000,550000) + ylim(50000,250000)+
  geom_sf(data=st_sf(z[234,]),fill=NA,size=0.25)

p1+p2+p3+p4 + plot_layout(guides='collect')

```

```{r risk_map_Falkirk, echo=FALSE,message=FALSE, fig.path='Figs/',fig.width=5, fig.height=5}

I0=numeric(391)
I0[344]=1

foi_gC<-calculate_FOI(theta=c(1.0),
                      move_m=list(ppCDOCE$mean/lads_map$all),
                      N=list(lads_map$all),
                      I=list(I0)) 


foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                      move_m=list(ppCDOU$mean / lads_map$under18,
                                  ppCDO18_30$mean / lads_map$a18_30,
                                  ppCDO30_60$mean / lads_map$a30_60,
                                  ppCDO60_100$mean / lads_map$a60_100),
                      N=list(lads_map$under18,lads_map$a18_30,
                             lads_map$a30_60,lads_map$a60_100),
                      I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 

foi_gCR<-calculate_FOI(theta=c(1.0),
                       move_m=list(census_flux/lads_map$all),
                       N=list(lads_map$all),
                       I=list(I0)) 

foi_gT<-calculate_FOI(theta=c(1.0),
                      move_m=list(ppCDOT$mean/lads_map$all),
                      N=list(lads_map$all),
                      I=list(I0)) 

foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                      move_m=list(ppCDOU$mean / lads_map$under18,
                                  ppCDO18_30$mean / lads_map$a18_30,
                                  ppCDO30_60$mean / lads_map$a30_60,
                                  ppCDO60_100$mean / lads_map$a60_100),
                      N=list(lads_map$under18,lads_map$a18_30,
                             lads_map$a30_60,lads_map$a60_100),
                      I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 

foi_g18_30<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                          move_m=list(ppCDOU$mean / lads_map$under18,
                                      ppCDO18_30$mean / lads_map$a18_30,
                                      ppCDO30_60$mean / lads_map$a30_60,
                                      ppCDO60_100$mean / lads_map$a60_100),
                          N=list(lads_map$under18,lads_map$a18_30,
                                 lads_map$a30_60,lads_map$a60_100),
                          I=list(rep(0,391),I0,rep(0,391),rep(0,391)) ) 

foi_g30_60<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                          move_m=list(ppCDOU$mean / lads_map$under18,
                                      ppCDO18_30$mean / lads_map$a18_30,
                                      ppCDO30_60$mean / lads_map$a30_60,
                                      ppCDO60_100$mean / lads_map$a60_100),
                          N=list(lads_map$under18,lads_map$a18_30,
                                 lads_map$a30_60,lads_map$a60_100),
                          I=list(rep(0,391),rep(0,391),I0,rep(0,391)) ) 

foi_g60_100<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                           move_m=list(ppCDOU$mean / lads_map$under18,
                                       ppCDO18_30$mean / lads_map$a18_30,
                                       ppCDO30_60$mean / lads_map$a30_60,
                                       ppCDO60_100$mean / lads_map$a60_100),
                           N=list(lads_map$under18,lads_map$a18_30,
                                  lads_map$a30_60,lads_map$a60_100),
                           I=list(rep(0,391),rep(0,391),rep(0,391),I0) ) 

z = lads_map %>% mutate(lC = (foi_gC$avg_inf_rate))
z = z %>% mutate(lT = (foi_gT$avg_inf_rate),
                 lU = ((foi_gU$avg_inf_rate)-lT),
                 l18_30 = ((foi_g18_30$avg_inf_rate)-lT),
                 l30_60 = ((foi_g30_60$avg_inf_rate)-lT),
                 l60_100 = ((foi_g60_100$avg_inf_rate)-lT))

max_val = max(c(z$lU[-344],z$l18_30[-344],z$l30_60[-344],z$l60_100[-344]),na.rm=T)
min_val = min(c(z$lU[-344],z$l18_30[-344],z$l30_60[-344],z$l60_100[-344]),na.rm=T)


#scale_fill_gradient2(name ='Relative Risk',trans='sqrt',midpoint=1,low=muted('blue'),high=muted('red'),limits=c(min_val,max_val)) + 
p1=ggplot(st_sf(z[-344,] %>% filter(ctry11NM=='Scotland')),aes(fill=lU)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC Under 18')+ theme(text = element_text(size=8))+
  geom_sf(data=st_sf(z[344,]),fill=NA,size=0.25)+ 
  ylim(550000,750000) + xlim(100000,400000)

#c(343,351,354)

p2=ggplot(st_sf(z[-344,]%>% filter(ctry11NM=='Scotland')),aes(fill=l18_30)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 18-30')+ theme(text = element_text(size=8))+
  geom_sf(data=st_sf(z[344,]),fill=NA,size=0.25)+
  ylim(550000,750000) + xlim(100000,400000)


p3=ggplot(st_sf(z[-344,]%>% filter(ctry11NM=='Scotland')),aes(fill=l30_60)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 30-60')+ theme(text = element_text(size=8))+
  geom_sf(data=st_sf(z[344,]),fill=NA,size=0.25)+
  ylim(550000,750000) + xlim(100000,400000)


p4=ggplot(st_sf(z[-344,]%>% filter(ctry11NM=='Scotland')),aes(fill=l60_100)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 60-100')+ theme(text = element_text(size=8))+
  geom_sf(data=st_sf(z[344,]),fill=NA,size=0.25)+
  ylim(550000,750000) + xlim(100000,400000)

p1+p2+p3+p4 + plot_layout(guides='collect')

```


```{r risk_map_barnet, echo=FALSE,message=FALSE, fig.path='Figs/',fig.width=5, fig.height=5}

I0=numeric(391)
I0[296]=1

foi_gC<-calculate_FOI(theta=c(1.0),
                      move_m=list(ppCDOCE$mean/lads_map$all),
                      N=list(lads_map$all),
                      I=list(I0)) 


foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                      move_m=list(ppCDOU$mean / lads_map$under18,
                                  ppCDO18_30$mean / lads_map$a18_30,
                                  ppCDO30_60$mean / lads_map$a30_60,
                                  ppCDO60_100$mean / lads_map$a60_100),
                      N=list(lads_map$under18,lads_map$a18_30,
                             lads_map$a30_60,lads_map$a60_100),
                      I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 

foi_gCR<-calculate_FOI(theta=c(1.0),
                       move_m=list(census_flux/lads_map$all),
                       N=list(lads_map$all),
                       I=list(I0)) 

foi_gT<-calculate_FOI(theta=c(1.0),
                      move_m=list(ppCDOT$mean/lads_map$all),
                      N=list(lads_map$all),
                      I=list(I0)) 

foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                      move_m=list(ppCDOU$mean / lads_map$under18,
                                  ppCDO18_30$mean / lads_map$a18_30,
                                  ppCDO30_60$mean / lads_map$a30_60,
                                  ppCDO60_100$mean / lads_map$a60_100),
                      N=list(lads_map$under18,lads_map$a18_30,
                             lads_map$a30_60,lads_map$a60_100),
                      I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 

foi_g18_30<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                          move_m=list(ppCDOU$mean / lads_map$under18,
                                      ppCDO18_30$mean / lads_map$a18_30,
                                      ppCDO30_60$mean / lads_map$a30_60,
                                      ppCDO60_100$mean / lads_map$a60_100),
                          N=list(lads_map$under18,lads_map$a18_30,
                                 lads_map$a30_60,lads_map$a60_100),
                          I=list(rep(0,391),I0,rep(0,391),rep(0,391)) ) 

foi_g30_60<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                          move_m=list(ppCDOU$mean / lads_map$under18,
                                      ppCDO18_30$mean / lads_map$a18_30,
                                      ppCDO30_60$mean / lads_map$a30_60,
                                      ppCDO60_100$mean / lads_map$a60_100),
                          N=list(lads_map$under18,lads_map$a18_30,
                                 lads_map$a30_60,lads_map$a60_100),
                          I=list(rep(0,391),rep(0,391),I0,rep(0,391)) ) 

foi_g60_100<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                           move_m=list(ppCDOU$mean / lads_map$under18,
                                       ppCDO18_30$mean / lads_map$a18_30,
                                       ppCDO30_60$mean / lads_map$a30_60,
                                       ppCDO60_100$mean / lads_map$a60_100),
                           N=list(lads_map$under18,lads_map$a18_30,
                                  lads_map$a30_60,lads_map$a60_100),
                           I=list(rep(0,391),rep(0,391),rep(0,391),I0) ) 

z = lads_map %>% mutate(lC = (foi_gC$avg_inf_rate))
z = z %>% mutate(lT = (foi_gT$avg_inf_rate),
                 lU = ((foi_gU$avg_inf_rate)-lT),
                 l18_30 = ((foi_g18_30$avg_inf_rate)-lT),
                 l30_60 = ((foi_g30_60$avg_inf_rate)-lT),
                 l60_100 = ((foi_g60_100$avg_inf_rate)-lT))

max_val = max(c(z$lU[-296],z$l18_30[-296],z$l30_60[-296],z$l60_100[-296]),na.rm=T)
min_val = min(c(z$lU[-296],z$l18_30[-296],z$l30_60[-296],z$l60_100[-296]),na.rm=T)


#scale_fill_gradient2(name ='Relative Risk',trans='sqrt',midpoint=1,low=muted('blue'),high=muted('red'),limits=c(min_val,max_val)) + 
p1=ggplot(st_sf(z[-296,] %>% filter(ctry11NM=='England')),aes(fill=lU)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC Under 18')+ theme(text = element_text(size=8))+
  xlim(450000,600000) + ylim(150000,250000)+
  geom_sf(data=st_sf(z[296,]),fill=NA,size=0.25)

#c(343,351,354)

p2=ggplot(st_sf(z[-296,]%>% filter(ctry11NM=='England')),aes(fill=l18_30)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 18-30')+ theme(text = element_text(size=8))+
  xlim(450000,600000) + ylim(150000,250000)+
  geom_sf(data=st_sf(z[296,]),fill=NA,size=0.25)

p3=ggplot(st_sf(z[-296,]%>% filter(ctry11NM=='England')),aes(fill=l30_60)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 30-60')+ theme(text = element_text(size=8))+
  xlim(450000,600000) + ylim(150000,250000)+
  geom_sf(data=st_sf(z[296,]),fill=NA,size=0.25)


p4=ggplot(st_sf(z[-296,]%>% filter(ctry11NM=='England')),aes(fill=l60_100)) + geom_sf(size=0.0) + 
  scale_fill_gradient2(midpoint=0,
                       low=muted('blue'),high=muted('red'),
                       limits=c(min_val,max_val),name ='Risk Difference') + 
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
  theme(legend.position = "right") + 
  ggtitle('BBC 60-100')+ theme(text = element_text(size=8))+
  xlim(450000,600000) + ylim(150000,250000)+
  geom_sf(data=st_sf(z[296,]),fill=NA,size=0.25)

p1+p2+p3+p4 + plot_layout(guides='collect')

```