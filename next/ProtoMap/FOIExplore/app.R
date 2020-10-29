#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
require(tidyverse)
require(ggplot2)
require(sf)
require(RColorBrewer)
require(patchwork)
require(reshape2)
load('ShinyDat.RData')

lads_map = lads_map_crunch
ukoutline = ukoutline_crunch

imputed_flux<-as_tibble(melt(ppCDOU_move$mean)) %>% mutate(data='BBC Under 18')
imputed_flux<-imputed_flux %>% bind_rows(as_tibble(melt(ppCDO18_30_move$mean)) %>% mutate(data='BBC 18-30'))
imputed_flux<-imputed_flux %>% bind_rows(as_tibble(melt(ppCDO30_60_move$mean)) %>% mutate(data='BBC 30-60'))
imputed_flux<-imputed_flux %>% bind_rows(as_tibble(melt(ppCDO60_100_move$mean)) %>% mutate(data='BBC 60-100'))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("BBC Mobility - Dominant Age Group"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("index",
                        "Index location:",
                        choices=lads_map_crunch$lad17nm,
                        selected='Waverley'),
            selectInput("quantity",
                        "Quantity:",
                        choices=c('Flux out','Flux in','FoI'),
                        selected='FoI'),
            selectInput("comparison",
                        "Comparison:",
                        choices=c('Relative Difference','Total Population','Fraction 60-100'),
                        selected='Relative Difference')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mapPlot"),
           )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    rv <- reactiveValues()
    rv$lads_map_scale = lads_map
    rv$lads_map_scale_flux = lads_map
    rv$foi_net_norm = NULL
    rv$foi_net_diff = NULL
    rv$flux_out_norm = NULL
    rv$flux_out_diff = NULL
    rv$flux_in_norm = NULL
    rv$flux_in_diff = NULL
    rv$p1 = NULL
    rv$p2 = NULL
    rv$p3 = NULL
    rv$p4 = NULL
    rv$p5 = NULL
    rv$p6 = NULL
    rv$p7 = NULL
    rv$p8 = NULL
    
    eventReactive(input$index, 'Waverley')
    
    observeEvent(input$index,
                 {
 
                     rv$flux_out <- imputed_flux %>% 
                         filter(data=='BBC Under 18' & Var1==as.character(which(lads_map$lad17nm==input$index)))
                     rv$flux_out <- rv$flux_out %>% bind_rows(imputed_flux %>% filter(data=='BBC 18-30' & 
                                                        Var1==as.character(which(lads_map$lad17nm==input$index)))) 
    
                     rv$flux_out <- rv$flux_out %>% bind_rows(imputed_flux %>% filter(data=='BBC 30-60' & 
                                                        Var1==as.character(which(lads_map$lad17nm==input$index)))) 
                  
                     rv$flux_out <- rv$flux_out %>% bind_rows(imputed_flux %>% filter(data=='BBC 60-100' & 
                                                        Var1==as.character(which(lads_map$lad17nm==input$index))))
  
                     
                     rv$flux_out <- rv$flux_out %>% mutate(lad17nm=lads_map$lad17nm[Var2]) 
                     
                     rv$flux_out_norm <- rv$flux_out %>% group_by(Var2) %>% mutate(value=value/max(value)) %>% 
                         filter(value==1) %>% filter(Var2!=which(lads_map$lad17nm==input$index))
                     
                     rv$flux_out_diff <- rv$flux_out %>% group_by(Var2) %>% 
                         summarise(max_diff=diff(sort(value))[1]/max(value))  %>% 
                         mutate(lad17nm=lads_map$lad17nm[Var2]) %>%
                             filter(Var2!=which(lads_map$lad17nm==input$index))
                     
                     rv$flux_in <- imputed_flux %>% 
                         filter(data=='BBC Under 18' & Var2==as.character(which(lads_map$lad17nm==input$index)))
                     rv$flux_in <- rv$flux_in %>% bind_rows(imputed_flux %>% filter(data=='BBC 18-30' & 
                                                                                          Var2==as.character(which(lads_map$lad17nm==input$index)))) 
                     
                     rv$flux_in <- rv$flux_in %>% bind_rows(imputed_flux %>% filter(data=='BBC 30-60' & 
                                                                                          Var2==as.character(which(lads_map$lad17nm==input$index)))) 
                     
                     rv$flux_in <- rv$flux_in %>% bind_rows(imputed_flux %>% filter(data=='BBC 60-100' & 
                                                                                          Var2==as.character(which(lads_map$lad17nm==input$index))))
                     
                     
                     rv$flux_in <- rv$flux_in %>% mutate(lad17nm=lads_map$lad17nm[Var1]) 
                     
                     rv$flux_in_norm <- rv$flux_in %>% group_by(Var1) %>% mutate(value=value/max(value)) %>% 
                         filter(value==1) %>% filter(Var1!=which(lads_map$lad17nm==input$index))
                     
                     rv$flux_in_diff <- rv$flux_in %>% group_by(Var1) %>% 
                         summarise(max_diff=diff(sort(value))[1]/max(value))  %>% 
                         mutate(lad17nm=lads_map$lad17nm[Var1]) %>%
                         filter(Var1!=which(lads_map$lad17nm==input$index))
                     
                     I0=numeric(391)
                     I0[which(lads_map$lad17nm==input$index)]=1
                     
                     foi_gU<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                                           move_m=list(ppCDOU_move$mean / lads_map$under18,
                                                       ppCDO18_30_move$mean / lads_map$a18_30,
                                                       ppCDO30_60_move$mean / lads_map$a30_60,
                                                       ppCDO60_100_move$mean / lads_map$a60_100),
                                           N=list(lads_map$under18,lads_map$a18_30,
                                                  lads_map$a30_60,lads_map$a60_100),
                                           I=list(I0,rep(0,391),rep(0,391),rep(0,391))) 
                     
                     foi_g18_30<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                                               move_m=list(ppCDOU_move$mean / lads_map$under18,
                                                           ppCDO18_30_move$mean / lads_map$a18_30,
                                                           ppCDO30_60_move$mean / lads_map$a30_60,
                                                           ppCDO60_100_move$mean / lads_map$a60_100),
                                               N=list(lads_map$under18,lads_map$a18_30,
                                                      lads_map$a30_60,lads_map$a60_100),
                                               I=list(rep(0,391),I0,rep(0,391),rep(0,391)) ) 
                     
                     foi_g30_60<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                                               move_m=list(ppCDOU_move$mean / lads_map$under18,
                                                           ppCDO18_30_move$mean / lads_map$a18_30,
                                                           ppCDO30_60_move$mean / lads_map$a30_60,
                                                           ppCDO60_100_move$mean / lads_map$a60_100),
                                               N=list(lads_map$under18,lads_map$a18_30,
                                                      lads_map$a30_60,lads_map$a60_100),
                                               I=list(rep(0,391),rep(0,391),I0,rep(0,391)) ) 
                     
                     foi_g60_100<-calculate_FOI(theta=c(1.0,1.0,1.0,1.0),
                                                move_m=list(ppCDOU_move$mean / lads_map$under18,
                                                            ppCDO18_30_move$mean / lads_map$a18_30,
                                                            ppCDO30_60_move$mean / lads_map$a30_60,
                                                            ppCDO60_100_move$mean / lads_map$a60_100),
                                                N=list(lads_map$under18,lads_map$a18_30,
                                                       lads_map$a30_60,lads_map$a60_100),
                                                I=list(rep(0,391),rep(0,391),rep(0,391),I0) ) 
                     
                     
                     foi_net <- tibble(from=lads_map$lad17nm[which(lads_map$lad17nm==input$index)],to=lads_map$lad17nm,weight=foi_gU$avg_inf_rate,data='Under 18') 
                     foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[which(lads_map$lad17nm==input$index)],to=lads_map$lad17nm,weight=foi_g18_30$avg_inf_rate,data='18-30')) 
                     foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[which(lads_map$lad17nm==input$index)],to=lads_map$lad17nm,weight=foi_g30_60$avg_inf_rate,data='30-60')) 
                     foi_net <- foi_net %>% bind_rows(tibble(from=lads_map$lad17nm[which(lads_map$lad17nm==input$index)],to=lads_map$lad17nm,weight=foi_g60_100$avg_inf_rate,data='60-100'))
                     
                     foi_net <- foi_net %>% filter(to!=input$index)
                     
                     rv$foi_net_norm <- foi_net %>% group_by(to) %>% mutate(weight=weight/(max(weight))) %>% ungroup()
                     
                     #foi_net <- foi_net %>% mutate(weight=weight/max(weight))
                     
                     rv$foi_net_norm <- rv$foi_net_norm %>% mutate(data = factor(data,levels=c('Under 18','18-30','30-60','60-100')))
                     
                     lads_map_d1 = lads_map %>% filter(lad17nm!=input$index)
                     
                     rv$foi_net_norm <- rv$foi_net_norm %>% filter(weight==1)
                     rv$foi_net_diff <- foi_net %>% group_by(to) %>% summarise(max_diff=diff(sort(weight))[1]/max(weight))
                     
                     
                     scaling_factor <- (1-unlist(rv$foi_net_diff %>% select(-to)))
                     
                     rv$lads_map_scale = (st_geometry(lads_map_d1$geometry) - 
                                           st_geometry(st_centroid(lads_map_d1))) * scaling_factor + st_geometry(st_centroid(lads_map_d1))
                     
                     rv$lads_map_scale = st_set_geometry(lads_map_d1,rv$lads_map_scale)
                     
                     st_crs(rv$lads_map_scale) <- st_crs(lads_map)   
                     
                     scaling_factor <- (1-unlist(rv$flux_out_diff %>% ungroup() %>% select(max_diff)))
                     
                     rv$lads_map_scale_flux_out = (st_geometry(lads_map_d1$geometry) - 
                                              st_geometry(st_centroid(lads_map_d1))) * scaling_factor + st_geometry(st_centroid(lads_map_d1))
                     
                     rv$lads_map_scale_flux_out = st_set_geometry(lads_map_d1,rv$lads_map_scale_flux_out)
                     
                     st_crs(rv$lads_map_scale_flux_out) <- st_crs(lads_map) 
                     
                     scaling_factor <- (1-unlist(rv$flux_in_diff %>% ungroup() %>% select(max_diff)))
                     
                     rv$lads_map_scale_flux_in = (st_geometry(lads_map_d1$geometry) - 
                                                       st_geometry(st_centroid(lads_map_d1))) * scaling_factor + st_geometry(st_centroid(lads_map_d1))
                     
                     rv$lads_map_scale_flux_in = st_set_geometry(lads_map_d1,rv$lads_map_scale_flux_in)
                     
                     st_crs(rv$lads_map_scale_flux_in) <- st_crs(lads_map) 
                     
                     rv$p1=ggplot(rv$lads_map_scale %>% 
                                   inner_join(rv$foi_net_norm %>% mutate(lad17nm=to))) +
                         geom_sf(col='black',size=0.1,aes(fill=data)) + 
                         scale_fill_brewer(palette ='Spectral',direction=-1) +
                         labs(fill='Age Group')
                     
                     rv$p2=ggplot(lads_map %>% 
                                   inner_join(rv$foi_net_diff %>% mutate(lad17nm=to))) +
                         geom_sf(col='black',size=0.1,aes(fill=max_diff)) + 
                         scale_fill_distiller(palette ='Spectral',direction=-1) +
                         labs(fill='Relative Difference From Next')
                     
                     rv$p3=ggplot(lads_map) +
                         geom_sf(col='black',size=0.1,aes(fill=TotPop)) + 
                         scale_fill_distiller(palette ='Spectral',direction=-1) +
                         labs(fill='Total Population Size')
                     
                     rv$p4=ggplot(lads_map) +
                         geom_sf(col='black',size=0.1,aes(fill=a60_100/TotPop)) + 
                         scale_fill_distiller(palette ='Spectral',direction=-1) +
                         labs(fill='Fraction of population 60-100')
                     
                     rv$p5=ggplot(rv$lads_map_scale_flux_out %>% 
                                   inner_join(rv$flux_out_norm)) +
                         geom_sf(col='black',size=0.1,aes(fill=data)) + 
                         scale_fill_brewer(palette ='Spectral',direction=-1) +
                         labs(fill='Age Group')
                     
                     rv$p6=ggplot(lads_map %>% 
                                   inner_join(rv$flux_out_diff)) +
                         geom_sf(col='black',size=0.1,aes(fill=max_diff)) + 
                         scale_fill_distiller(palette ='Spectral',direction=-1) +
                         labs(fill='Relative Difference from Next')
                     
                     rv$p7=ggplot(rv$lads_map_scale_flux_in %>% 
                                   inner_join(rv$flux_in_norm)) +
                         geom_sf(col='black',size=0.1,aes(fill=data)) + 
                         scale_fill_brewer(palette ='Spectral',direction=-1) +
                         labs(fill='Age Group')
                     
                     rv$p8=ggplot(lads_map %>% 
                                   inner_join(rv$flux_in_diff)) +
                         geom_sf(col='black',size=0.1,aes(fill=max_diff)) + 
                         scale_fill_distiller(palette ='Spectral',direction=-1) +
                         labs(fill='Relative Difference from Next')
                 })
    
    
    output$mapPlot <- renderPlot({
      
  
    pout = NULL
    
    if(input$quantity == 'Flux out'){pout = rv$p5}else
        if(input$quantity == 'Flux in'){pout = rv$p7}else{pout=rv$p1}
    if(input$comparison=='Relative Difference')
        {if(input$quantity == 'Flux out'){pout = pout + rv$p6}else 
            if(input$quantity=='Flux in'){pout = pout + rv$p8}else
        {pout = pout + rv$p2}}else if(input$comparison == 'Total Population')
        {pout = pout + rv$p3}else{
        pout = pout + p4}
    
    print(pout) 
    
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
