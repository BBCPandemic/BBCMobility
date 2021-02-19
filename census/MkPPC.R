require(sf)
require(reshape2)
require(tidyverse)
require(ggplot2)
require(rstan)
require(loo)
require(tidybayes)
require(patchwork)
require(scales)
require(xtable)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source('./PPDfuncs.R')

load('../../lads_map.RData')
load('../flux/mobility_flux_next.RData')
load('../flux/total_census_flux.RData')
load('../flux/census_flux.RData')

UKcensus_flows_lads <- EWcensus_flows_lads %>% mutate(country='England & Wales')
UKcensus_flows_lads <- UKcensus_flows_lads %>% 
  bind_rows(scotland_census_flows_lads %>% mutate(country='Scotland'))
UKcensus_flows_lads <- UKcensus_flows_lads %>% 
  bind_rows(NI_census_flows_lads %>% mutate(country='Northern Ireland'))

load('../census/CDE/CDE_ECensus.RData')
load('../census/CDE/CDE_SCensus.RData')
load('../census/CDE/CDE_WCensus.RData')
load('../census/CDE/CDE_NICensus.RData')

load('../census/CDP/CDP_ECensus.RData')
load('../census/CDP/CDP_SCensus.RData')
load('../census/CDP/CDP_WCensus.RData')
load('../census/CDP/CDP_NICensus.RData')

load('../census/CDO/CDO_ECensus.RData')
load('../census/CDO/CDO_SCensus.RData')
load('../census/CDO/CDO_WCensus.RData')
load('../census/CDO/CDO_NICensus.RData')

load('../census/CDO/CDO_ECensus.RData')
load('../census/CDO/CDO_SCensus.RData')
load('../census/CDO/CDO_WCensus.RData')
load('../census/CDO/CDO_NICensus.RData')

load('../census/ERadiation/ERad_ECensus.RData')
load('../census/ERadiation/ERad_SCensus.RData')
load('../census/ERadiation/ERad_WCensus.RData')
load('../census/ERadiation/ERad_NICensus.RData')

load('../census/Impedance/Imp_ECensus.RData')
load('../census/Impedance/Imp_SCensus.RData')
load('../census/Impedance/Imp_WCensus.RData')
load('../census/Impedance/Imp_NICensus.RData')

load('../census/Stoufer/Sto_ECensus.RData')
load('../census/Stoufer/Sto_CSCensus.RData')
load('../census/Stoufer/Sto_WCensus.RData')
load('../census/Stoufer/Sto_NICensus.RData')

load('../census/IO/IO_ECensus.RData')
load('../census/IO/IO_CSCensus.RData')
load('../census/IO/IO_WCensus.RData')
load('../census/IO/IO_NICensus.RData')


load('../../furthest_from_home_meta.RData')

employment_freq <- furthest_from_home %>% select(employment_cat) %>% gather() %>% group_by(value) %>% count()

furthest_from_home <- furthest_from_home %>% mutate(home_country=str_extract(home_lad,''),furthest_country=str_extract(furthest_lad,''),nextFreq_country = str_extract(nextFreq_lad,''))

furthest_from_home <- furthest_from_home %>% mutate(home_country=factor(home_country,levels=c('E','W','S','N')),
                                                    furthest_country=factor(furthest_country,levels=c('E','W','S','N')),
                                                    nextFreq_country = factor(nextFreq_country,levels=c('E','W','S','N')))


source('./DropLADSCensus.R')

CPCpost <- CPCpp(Ecensus_mobility_dat,
                 fitCDO_CE,
                 'CDO','England Census',
                 CDOmovemat)

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitCDE_CE,
                                       'CDE','England Census',
                                       CDEmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitCDP_CE,
                                       'CDP','England Census',
                                       CDPmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitImp_CE,
                                       'Imp','England Census',
                                       Impmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitERad_CE,
                                       'ERad','England Census',
                                       ERadmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitStoufer_CE,
                                       'Sto','England Census',
                                       Stomovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitIO_CE,
                                       'IO','England Census',
                                       IOmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                 fitCDO_CW,
                 'CDO','Wales Census',
                 CDOmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitCDE_CW,
                                       'CDE','Wales Census',
                                       CDEmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitCDP_CW,
                                       'CDP','Wales Census',
                                       CDPmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitImp_CW,
                                       'Imp','Wales Census',
                                       Impmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitERad_CW,
                                       'ERad','Wales Census',
                                       ERadmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitStoufer_CW,
                                       'Sto','Wales Census',
                                       Stomovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitIO_CW,
                                       'IO','Wales Census',
                                       IOmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDO_CNI,
                                       'CDO','NI Census',
                                       CDOmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDE_CNI,
                                       'CDE','NI Census',
                                       CDEmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDP_CNI,
                                       'CDP','NI Census',
                                       CDPmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitImp_CNI,
                                       'Imp','NI Census',
                                       Impmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitERad_CNI,
                                       'ERad','NI Census',
                                       ERadmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitStoufer_CNI,
                                       'Sto','NI Census',
                                       Stomovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitIO_CNI,
                                       'IO','NI Census',
                                       IOmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDO_CS,
                                       'CDO','Scotland Census',
                                       CDOmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDE_CE,
                                       'CDE','Scotland Census',
                                       CDEmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDP_CS,
                                       'CDP','Scotland Census',
                                       CDPmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitImp_CS,
                                       'Imp','Scotland Census',
                                       Impmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitERad_CS,
                                       'ERad','Scotland Census',
                                       ERadmovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitStoufer_CS,
                                       'Sto','Scotland Census',
                                       Stomovemat))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitIO_CS,
                                       'IO','Scotland Census',
                                       IOmovemat))

save(CPCpost,file='CPCpost.RData')




