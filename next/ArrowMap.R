flux_net <- imputed_flux %>% filter(data=='BBC Under 18' & Var1=='234') %>% slice_max(value,n=4) %>% mutate(rank=1:4)
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 18-30' & Var1=='234') 
                                   %>% slice_max(value,n=4)%>% mutate(rank=1:4))
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 30-60' & Var1=='234') 
                                   %>% slice_max(value,n=4)%>% mutate(rank=1:4))
flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 60-100' & Var1=='234') 
                                   %>% slice_max(value,n=4)%>% mutate(rank=1:4))

# flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC Under 18' & Var1=='234') 
#                                    %>% slice_min(value,n=5))
# flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 18-30' & Var1=='234') 
#                                    %>% slice_min(value,n=5))
# flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 30-60' & Var1=='234') 
#                                    %>% slice_min(value,n=5))
# flux_net <- flux_net %>% bind_rows(imputed_flux %>% filter(data=='BBC 60-100' & Var1=='234') 
#                                    %>% slice_min(value,n=5))


require(ggraph)
require(tidygraph)

locations<-st_coordinates(st_centroid(lads_map$geometry[c(234,flux_net$Var2)]))

nodes <- data.frame(id=lads_map$lad17nm[c(234,flux_net$Var2)],
                    X=locations[,1],
                    Y=locations[,2]) %>% unique()

edges <- data.frame(from=lads_map$lad17nm[flux_net$Var1],
           to=lads_map$lad17nm[flux_net$Var2],
           weight=flux_net$value,
           data=flux_net$data,
           rank=as.factor(flux_net$rank))

gr <- tbl_graph(edges=edges,directed=TRUE)

gr2 <- create_layout(gr,'manual',x=nodes$X,y=nodes$Y)

require(sf)
require(ggspatial)


lads_map_scale = (st_geometry(lads_map$geometry) - st_geometry(st_centroid(lads_map))) * lads_map$TotPop/max(lads_map$TotPop) + st_geometry(st_centroid(lads_map))
st_set_geometry(lads_map,lads_map_scale)

p = ggraph(gr,layout=gr2[,c(1,2)]) + 
  geom_edge_fan(aes(edge_width = weight,edge_colour=data),edge_alpha=0.8, 
                arrow = arrow(length = unit(12, "pt"), type = "closed")) + 
  geom_node_text(aes(label=name)) + annotation_spatial(lads_map,fill=NA) +
  scale_colour_brewer(palette ='Spectral',aesthetics=c('edge_colour'),direction=-1) +
  theme(legend.position='bottom',legend.direction=c('horizontal')) + 
  labs(edge_colour='Age Group',edge_width='Flux')


pdf('./ProtoMap/HaselmereFlux.pdf')
print(p)  
dev.off()

p = ggraph(gr,layout=gr2[,c(1,2)]) + 
  geom_edge_fan(aes(edge_width = weight,edge_colour=data),edge_alpha=0.8, 
                arrow = arrow(length = unit(12, "pt"), type = "closed")) + 
  geom_node_text(aes(label=name)) + annotation_spatial(lads_map,fill=NA) +
  scale_colour_brewer(palette ='Spectral',aesthetics=c('edge_colour'),direction=-1) +
  theme(legend.position='bottom',legend.direction=c('horizontal')) + 
  labs(edge_colour='Age Group',edge_width='Flux') +
  facet_wrap(~rank)

pdf('./ProtoMap/HaselmereFlux_facet.pdf')
print(p)  
dev.off()

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


foi_net <- tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_gU$avg_inf_rate,data='Under 18') %>% slice_max(weight,n=5) %>% mutate(rank=0:4)
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_g18_30$avg_inf_rate,data='18-30') %>% slice_max(weight,n=5)%>% mutate(rank=0:4))
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_g30_60$avg_inf_rate,data='30-60') %>% slice_max(weight,n=5)%>% mutate(rank=0:4))
foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[234],to=lads_map$lad17nm,weight=foi_g60_100$avg_inf_rate,data='60-100') %>% slice_max(weight,n=5)%>% mutate(rank=0:4))

#foi_net <- foi_net %>% group_by(to) %>% mutate(weight=weight/sum(weight)) %>% ungroup()

foi_net <- foi_net %>% filter(to!='Waverley')


nodes <- data.frame(id=unique(c(foi_net$from[1],foi_net$to)),
                    X=NA,
                    Y=NA) 

locations = t(sapply(nodes$id,function(x){st_coordinates(st_centroid(lads_map %>% filter(lad17nm==x)))}))

nodes$X = locations[,1]
nodes$Y = locations[,2]

edges <- data.frame(from=foi_net$from,
                    to=foi_net$to,
                    weight=foi_net$weight,
                    data=foi_net$data,
                    rank=foi_net$rank)

gr <- tbl_graph(edges=edges,directed=TRUE)

gr2 <- create_layout(gr,'manual',x=nodes$X,y=nodes$Y)

require(sf)
require(ggspatial)

lads_map_scale = (st_geometry(lads_map$geometry) - st_geometry(st_centroid(lads_map))) * (lads_map$TotPop)/max((lads_map$TotPop)) + st_geometry(st_centroid(lads_map))
st_set_geometry(lads_map,lads_map_scale)

p = ggraph(gr,layout=gr2[,c(1,2)]) + 
  geom_edge_fan(aes(edge_width = weight,edge_colour=data),edge_alpha=0.8, 
                arrow = arrow(length = unit(12, "pt"), type = "closed")) + 
  geom_node_text(aes(label=name)) + annotation_spatial(lads_map,fill=NA) +
  scale_colour_brewer(palette ='Spectral',aesthetics=c('edge_colour'),direction=-1) +
  theme(legend.position='bottom',legend.direction=c('horizontal')) + 
  labs(edge_colour='Age Group',edge_width='FOI')

pdf('./ProtoMap/HaselmereFOI.pdf')
print(p)  
dev.off()

p = ggraph(gr,layout=gr2[,c(1,2)]) + 
  geom_edge_fan(aes(edge_width = weight,edge_colour=data),edge_alpha=0.8, 
                arrow = arrow(length = unit(12, "pt"), type = "closed")) + 
  geom_node_text(aes(label=name)) + annotation_spatial(lads_map,fill=NA) +
  scale_colour_brewer(palette ='Spectral',aesthetics=c('edge_colour'),direction=-1) +
  theme(legend.position='bottom',legend.direction=c('horizontal')) + 
  labs(edge_colour='Age Group',edge_width='FOI') + facet_wrap(~rank)

pdf('./ProtoMap/HaselmereFOI_facet.pdf')
print(p)  
dev.off()

