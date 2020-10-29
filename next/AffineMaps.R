#require(cartogram)
#lads_cartogram<-cartogram(lads_map,'TotPop')
#equal_cartogram<-cartogram_cont(lads_map %>% mutate(scale=1),'scale')
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


foi_net <- foi_net %>% group_by(to) %>% mutate(weight=weight/(max(weight)+0.1*max(weight))) %>% ungroup()

#foi_net <- foi_net %>% mutate(weight=weight/max(weight))

nodes <- data.frame(id=unique(c(foi_net$from[1],foi_net$to)),
                    X=NA,
                    Y=NA) 

locations = t(sapply(nodes$id,function(x){st_coordinates(st_centroid(lads_map %>% filter(lad17nm==x)))}))

nodes$X = locations[,1]
nodes$Y = locations[,2]




edges <- data.frame(from=foi_net$from,
                    to=foi_net$to,
                    weight=foi_net$weight,
                    data=foi_net$data)

gr <- tbl_graph(edges=edges,directed=TRUE)

gr2 <- create_layout(gr,'manual',x=nodes$X,y=nodes$Y)

require(sf)
require(ggspatial)

lads_map_d1 = lads_map %>% filter(lad17nm!='Waverley')

lads_map_scale_1 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='Under 18') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_2 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='18-30') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_3 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='30-60') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_4 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='60-100') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))

lads_map_scale_1 = st_set_geometry(lads_map_d1,lads_map_scale_1)
lads_map_scale_2 = st_set_geometry(lads_map_d1,lads_map_scale_2)
lads_map_scale_3 = st_set_geometry(lads_map_d1,lads_map_scale_3)
lads_map_scale_4 = st_set_geometry(lads_map_d1,lads_map_scale_4)

st_crs(lads_map_scale_1) <- st_crs(lads_map)
st_crs(lads_map_scale_2) <- st_crs(lads_map)
st_crs(lads_map_scale_3) <- st_crs(lads_map)
st_crs(lads_map_scale_4) <- st_crs(lads_map)


pdf('./ProtoMap/UKMap_affine_FOI.pdf')
ggplot()  + 
  geom_sf(col='black',fill=NA,data=lads_map,size=0.1) + 
  geom_sf(aes(col='Under 18'),
          fill=NA,data=lads_map_scale_1,size=0.1) +
  geom_sf(aes(col='18-30'),
          fill=NA,data=lads_map_scale_2,size=0.1) +
  geom_sf(aes(col='30-60'),
          fill=NA,data=lads_map_scale_3,size=0.1) +
  geom_sf(aes(col='60-100'),
          fill=NA,data=lads_map_scale_4,size=0.1) +
  scale_color_manual(name   = 'Age Group',
                     breaks = c('Under 18','18-30','30-60','60-100'),
                     values = c('Under 18'=brewer.pal(4,'Spectral')[4],
                                '18-30'=brewer.pal(4,'Spectral')[3],
                                '30-60'=brewer.pal(4,'Spectral')[2],
                                '60-100'=brewer.pal(4,'Spectral')[1])) + 
  theme(legend.position='bottom',legend.direction=c('horizontal')) 
dev.off()

I0=numeric(391)
I0[281]=1

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


foi_net <- tibble(from=lads_map$lad17nm[281],to=lads_map$lad17nm,weight=foi_gU$avg_inf_rate,data='Under 18') 
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[281],to=lads_map$lad17nm,weight=foi_g18_30$avg_inf_rate,data='18-30')) 
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[281],to=lads_map$lad17nm,weight=foi_g30_60$avg_inf_rate,data='30-60')) 
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[281],to=lads_map$lad17nm,weight=foi_g60_100$avg_inf_rate,data='60-100'))

foi_net <- foi_net %>% filter(to!='Birmingham')


foi_net <- foi_net %>% group_by(to) %>% mutate(weight=weight/(max(weight)+0.1*max(weight))) %>% ungroup()

#foi_net <- foi_net %>% mutate(weight=weight/max(weight))

nodes <- data.frame(id=unique(c(foi_net$from[1],foi_net$to)),
                    X=NA,
                    Y=NA) 

locations = t(sapply(nodes$id,function(x){st_coordinates(st_centroid(lads_map %>% filter(lad17nm==x)))}))

nodes$X = locations[,1]
nodes$Y = locations[,2]




edges <- data.frame(from=foi_net$from,
                    to=foi_net$to,
                    weight=foi_net$weight,
                    data=foi_net$data)

gr <- tbl_graph(edges=edges,directed=TRUE)

gr2 <- create_layout(gr,'manual',x=nodes$X,y=nodes$Y)

require(sf)
require(ggspatial)

lads_map_d1 = lads_map %>% filter(lad17nm!='Birmingham')

lads_map_scale_1 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='Under 18') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_2 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='18-30') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_3 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='30-60') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_4 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(foi_net %>% filter(data=='60-100') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))

lads_map_scale_1 = st_set_geometry(lads_map_d1,lads_map_scale_1)
lads_map_scale_2 = st_set_geometry(lads_map_d1,lads_map_scale_2)
lads_map_scale_3 = st_set_geometry(lads_map_d1,lads_map_scale_3)
lads_map_scale_4 = st_set_geometry(lads_map_d1,lads_map_scale_4)

st_crs(lads_map_scale_1) <- st_crs(lads_map)
st_crs(lads_map_scale_2) <- st_crs(lads_map)
st_crs(lads_map_scale_3) <- st_crs(lads_map)
st_crs(lads_map_scale_4) <- st_crs(lads_map)
+ geom_sf(data=ukoutline,col='black',fill=NA)

pdf('./ProtoMap/UKMap_affine_FOI_Birmingham.pdf')
ggplot()  + 
  geom_sf(col='black',fill=NA,data=lads_map,size=0.1) + 
  geom_sf(aes(col='Under 18'),
          fill=NA,data=lads_map_scale_1,size=0.1) +
  geom_sf(aes(col='18-30'),
          fill=NA,data=lads_map_scale_2,size=0.1) +
  geom_sf(aes(col='30-60'),
          fill=NA,data=lads_map_scale_3,size=0.1) +
  geom_sf(aes(col='60-100'),
          fill=NA,data=lads_map_scale_4,size=0.1) +
  scale_color_manual(name   = 'Age Group',
                     breaks = c('Under 18','18-30','30-60','60-100'),
                     values = c('Under 18'=brewer.pal(4,'Spectral')[4],
                                '18-30'=brewer.pal(4,'Spectral')[3],
                                '30-60'=brewer.pal(4,'Spectral')[2],
                                '60-100'=brewer.pal(4,'Spectral')[1])) + 
  theme(legend.position='bottom',legend.direction=c('horizontal')) 
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


flux_net <- flux_net %>% group_by(to) %>% mutate(weight=weight/(max(weight)+0.1*max(weight))) %>% ungroup()


#foi_net <- foi_net %>% mutate(weight=weight/max(weight))

nodes <- data.frame(id=unique(c(flux_net$from[1],flux_net$to)),
                    X=NA,
                    Y=NA) 

locations = t(sapply(nodes$id,function(x){st_coordinates(st_centroid(lads_map %>% filter(lad17nm==x)))}))

nodes$X = locations[,1]
nodes$Y = locations[,2]




edges <- data.frame(from=flux_net$from,
                    to=flux_net$to,
                    weight=flux_net$weight,
                    data=flux_net$data)

gr <- tbl_graph(edges=edges,directed=TRUE)

gr2 <- create_layout(gr,'manual',x=nodes$X,y=nodes$Y)

require(sf)
require(ggspatial)

lads_map_d1 = lads_map %>% filter(lad17nm!='Waverley')

lads_map_scale_1 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC Under 18') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_2 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC 18-30') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_3 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC 30-60') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_4 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC 60-100') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))

lads_map_scale_1 = st_set_geometry(lads_map_d1,lads_map_scale_1)
lads_map_scale_2 = st_set_geometry(lads_map_d1,lads_map_scale_2)
lads_map_scale_3 = st_set_geometry(lads_map_d1,lads_map_scale_3)
lads_map_scale_4 = st_set_geometry(lads_map_d1,lads_map_scale_4)

st_crs(lads_map_scale_1) <- st_crs(lads_map)
st_crs(lads_map_scale_2) <- st_crs(lads_map)
st_crs(lads_map_scale_3) <- st_crs(lads_map)
st_crs(lads_map_scale_4) <- st_crs(lads_map)

pdf('./ProtoMap/UKMap_affine_fluxWaverley.pdf')
ggplot()  + 
  geom_sf(col='black',fill=NA,data=lads_map,size=0.1) + 
  geom_sf(aes(col='Under 18'),
          fill=NA,data=lads_map_scale_1,size=0.1) +
  geom_sf(aes(col='18-30'),
          fill=NA,data=lads_map_scale_2,size=0.1) +
  geom_sf(aes(col='30-60'),
          fill=NA,data=lads_map_scale_3,size=0.1) +
  geom_sf(aes(col='60-100'),
          fill=NA,data=lads_map_scale_4,size=0.1) +
  scale_color_manual(name   = 'Age Group',
                     breaks = c('Under 18','18-30','30-60','60-100'),
                     values = c('Under 18'=brewer.pal(4,'Spectral')[4],
                                '18-30'=brewer.pal(4,'Spectral')[3],
                                '30-60'=brewer.pal(4,'Spectral')[2],
                                '60-100'=brewer.pal(4,'Spectral')[1])) + 
  theme(legend.position='bottom',legend.direction=c('horizontal')) 
dev.off()

flux_net <- imputed_flux %>% filter(data=='BBC Under 18' & Var1=='281') 
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 18-30' & Var1=='281') 
                                   )
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 30-60' & Var1=='281') 
                                   )
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 60-100' & Var1=='281') 
                                   )



flux_net <- flux_net %>% mutate(from = lads_map$lad17nm[Var1],to=lads_map$lad17nm[Var2],weight=value)

flux_net <- flux_net %>% filter(to!='Birmingham')


flux_net <- flux_net %>% group_by(to) %>% mutate(weight=weight/(max(weight)+0.1*max(weight))) %>% ungroup()

#foi_net <- foi_net %>% mutate(weight=weight/max(weight))

nodes <- data.frame(id=unique(c(flux_net$from[1],flux_net$to)),
                    X=NA,
                    Y=NA) 

locations = t(sapply(nodes$id,function(x){st_coordinates(st_centroid(lads_map %>% filter(lad17nm==x)))}))

nodes$X = locations[,1]
nodes$Y = locations[,2]


edges <- data.frame(from=flux_net$from,
                    to=flux_net$to,
                    weight=flux_net$weight,
                    data=flux_net$data)

gr <- tbl_graph(edges=edges,directed=TRUE)

gr2 <- create_layout(gr,'manual',x=nodes$X,y=nodes$Y)

require(sf)
require(ggspatial)

lads_map_d1 = lads_map %>% filter(lad17nm!='Birmingham')

lads_map_scale_1 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC Under 18') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_2 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC 18-30') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_3 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC 30-60') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))
lads_map_scale_4 = (st_geometry(lads_map_d1$geometry) - st_geometry(st_centroid(lads_map_d1))) * unlist(flux_net %>% filter(data=='BBC 60-100') %>% select(weight)) + st_geometry(st_centroid(lads_map_d1))

lads_map_scale_1 = st_set_geometry(lads_map_d1,lads_map_scale_1)
lads_map_scale_2 = st_set_geometry(lads_map_d1,lads_map_scale_2)
lads_map_scale_3 = st_set_geometry(lads_map_d1,lads_map_scale_3)
lads_map_scale_4 = st_set_geometry(lads_map_d1,lads_map_scale_4)

st_crs(lads_map_scale_1) <- st_crs(lads_map)
st_crs(lads_map_scale_2) <- st_crs(lads_map)
st_crs(lads_map_scale_3) <- st_crs(lads_map)
st_crs(lads_map_scale_4) <- st_crs(lads_map)

pdf('./ProtoMap/UKMap_affine_flux_Birmingham.pdf')
ggplot()  + 
  geom_sf(col='black',fill=NA,data=lads_map,size=0.1) + 
  geom_sf(aes(col='Under 18'),
          fill=NA,data=lads_map_scale_1,size=0.1) +
  geom_sf(aes(col='18-30'),
          fill=NA,data=lads_map_scale_2,size=0.1) +
  geom_sf(aes(col='30-60'),
          fill=NA,data=lads_map_scale_3,size=0.1) +
  geom_sf(aes(col='60-100'),
          fill=NA,data=lads_map_scale_4,size=0.1) +
  scale_color_manual(name   = 'Age Group',
                     breaks = c('Under 18','18-30','30-60','60-100'),
                     values = c('Under 18'=brewer.pal(4,'Spectral')[4],
                                '18-30'=brewer.pal(4,'Spectral')[3],
                                '30-60'=brewer.pal(4,'Spectral')[2],
                                '60-100'=brewer.pal(4,'Spectral')[1])) + 
  theme(legend.position='bottom',legend.direction=c('horizontal')) 
dev.off()