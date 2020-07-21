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


Ecensus_mobility_dat$N = Ecensus_mobility_dat$N[-c(294,326)]
Ecensus_mobility_dat$flux = Ecensus_mobility_dat$flux[-c(294,326)]
Ecensus_mobility_dat$A = Ecensus_mobility_dat$A[-c(294,326)]
Ecensus_mobility_dat$mv = Ecensus_mobility_dat$mv[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$r = Ecensus_mobility_dat$r[-c(294,326),-c(294,326)]
Ecensus_mobility_dat$no_patches = length(Ecensus_mobility_dat$N)
Ecensus_mobility_dat$L = array(c(-1))
Ecensus_mobility_dat$Lno = 0

Ecensus_mobility_dat$non_zero = Ecensus_mobility_dat$flux>0
Ecensus_mobility_dat$no_non_zero = sum(Ecensus_mobility_dat$non_zero)

scotland_census_mobility_dat$N = scotland_census_mobility_dat$N[-c(9)]
scotland_census_mobility_dat$flux = scotland_census_mobility_dat$flux[-c(9)]
scotland_census_mobility_dat$A = scotland_census_mobility_dat$A[-c(9)]
scotland_census_mobility_dat$mv = scotland_census_mobility_dat$mv[-c(9),-c(9)]
scotland_census_mobility_dat$r = scotland_census_mobility_dat$r[-c(9),-c(9)]
scotland_census_mobility_dat$s = scotland_census_mobility_dat$s[-c(9),-c(9)]
scotland_census_mobility_dat$no_patches = length(scotland_census_mobility_dat$N)
scotland_census_mobility_dat$L = array(c(-1))
scotland_census_mobility_dat$Lno = 0

scotland_census_mobility_dat$non_zero = scotland_census_mobility_dat$flux>0
scotland_census_mobility_dat$no_non_zero = sum(scotland_census_mobility_dat$non_zero)

CPCpost <- CPCpp(Ecensus_mobility_dat,
                 fitCDO_CE,
                 'CDO','England Census',
                 CDOmatflux)

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitCDE_CE,
                                       'CDE','England Census',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitCDP_CE,
                                       'CDP','England Census',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitImp_CE,
                                       'Imp','England Census',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitERad_CE,
                                       'ERad','England Census',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitStoufer_CE,
                                       'Sto','England Census',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitIO_CE,
                                       'IO','England Census',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                 fitCDO_CW,
                 'CDO','Wales Census',
                 CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitCDE_CW,
                                       'CDE','Wales Census',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitCDP_CW,
                                       'CDP','Wales Census',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitImp_CW,
                                       'Imp','Wales Census',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitERad_CW,
                                       'ERad','Wales Census',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitStoufer_CW,
                                       'Sto','Wales Census',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitIO_CW,
                                       'IO','Wales Census',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDO_CNI,
                                       'CDO','NI Census',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDE_CNI,
                                       'CDE','NI Census',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDP_CNI,
                                       'CDP','NI Census',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitImp_CNI,
                                       'Imp','NI Census',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitERad_CNI,
                                       'ERad','NI Census',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitStoufer_CNI,
                                       'Sto','NI Census',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitIO_CNI,
                                       'IO','NI Census',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDO_CS,
                                       'CDO','Scotland Census',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDE_CE,
                                       'CDE','Scotland Census',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDP_CS,
                                       'CDP','Scotland Census',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitImp_CS,
                                       'Imp','Scotland Census',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitERad_CS,
                                       'ERad','Scotland Census',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitStoufer_CS,
                                       'Sto','Scotland Census',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitIO_CS,
                                       'IO','Scotland Census',
                                       IOmatflux))

save(CPCpost,file='CPCpost.RData')




