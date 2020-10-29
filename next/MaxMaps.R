I0=numeric(391)
I0[234]=1

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


foi_net <- tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_gU$avg_inf_rate,data='Under 18') 
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_g18_30$avg_inf_rate,data='18-30')) 
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_g30_60$avg_inf_rate,data='30-60')) 
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_g60_100$avg_inf_rate,data='60-100'))

foi_net <- foi_net %>% filter(to!='Waverley')


foi_net <- foi_net %>% group_by(to) %>% mutate(weight=weight/(max(weight))) %>% ungroup()

foi_net <- foi_net %>% filter(weight==1)

#foi_net <- foi_net %>% mutate(weight=weight/max(weight))

foi_net <- foi_net %>% mutate(data = factor(data,levels=c('Under 18','18-30','30-60','60-100')))

pdf('./ProtoMap/UKMap_affine_FOI_max.pdf')
ggplot(lads_map %>% filter(lad17nm!='Waverley') %>% inner_join(foi_net %>% mutate(lad17nm=to)))  + 
  geom_sf(col='black',size=0.1,aes(fill=data)) + 
  scale_fill_brewer(palette ='Spectral',direction=-1) +
  labs(fill='Age Group')
dev.off()

flux_net <- imputed_flux %>% filter(data=='BBC Under 18' & Var1=='234') 
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 18-30' & Var1=='234') 
)
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 30-60' & Var1=='234') 
)
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 60-100' & Var1=='234') 
)


flux_net <- flux_net %>% mutate(from = lads_map$lad17nm[Var1],to=lads_map$lad17nm[Var2],weight=value)

flux_net <- flux_net %>% filter(to!='Waverley')

flux_net <- flux_net %>% group_by(to) %>% mutate(weight=weight/(max(weight))) %>% ungroup()

flux_net <- flux_net %>% filter(weight==1)

pdf('./ProtoMap/UKMap_affine_flux_max.pdf')
ggplot(lads_map %>% filter(lad17nm!='Waverley') %>% inner_join(flux_net %>% mutate(lad17nm=to)))  + 
  geom_sf(col='black',size=0.1,aes(fill=data)) + 
  scale_fill_brewer(palette ='Spectral',direction=-1) +
  labs(fill='Age Group')
dev.off()



  