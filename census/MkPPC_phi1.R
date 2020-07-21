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

source('./PPDfuncs_nophi.R')

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

load('../census/CDE/CDE_ECensusphi1.RData')
load('../census/CDE/CDE_SCensusphi1.RData')
load('../census/CDE/CDE_WCensusphi1.RData')
load('../census/CDE/CDE_NICensusphi1.RData')

load('../census/CDE/CDE_ECensusphi2.RData')
load('../census/CDE/CDE_SCensusphi2.RData')
load('../census/CDE/CDE_WCensusphi2.RData')
load('../census/CDE/CDE_NICensusphi2.RData')

load('../census/CDE/CDE_ECensusphi3.RData')
load('../census/CDE/CDE_SCensusphi3.RData')
load('../census/CDE/CDE_WCensusphi3.RData')
load('../census/CDE/CDE_NICensusphi3.RData')

load('../census/CDE/CDE_ECensusphi4.RData')
load('../census/CDE/CDE_SCensusphi4.RData')
load('../census/CDE/CDE_WCensusphi4.RData')
load('../census/CDE/CDE_NICensusphi4.RData')

load('../census/CDE/CDE_ECensusphi5.RData')
load('../census/CDE/CDE_SCensusphi5.RData')
load('../census/CDE/CDE_WCensusphi5.RData')
load('../census/CDE/CDE_NICensusphi5.RData')

load('../census/CDP/CDP_ECensus.RData')
load('../census/CDP/CDP_SCensus.RData')
load('../census/CDP/CDP_WCensus.RData')
load('../census/CDP/CDP_NICensus.RData')

load('../census/CDP/CDP_ECensusphi1.RData')
load('../census/CDP/CDP_SCensusphi1.RData')
load('../census/CDP/CDP_WCensusphi1.RData')
load('../census/CDP/CDP_NICensusphi1.RData')

load('../census/CDP/CDP_ECensusphi2.RData')
load('../census/CDP/CDP_SCensusphi2.RData')
load('../census/CDP/CDP_WCensusphi2.RData')
load('../census/CDP/CDP_NICensusphi2.RData')

load('../census/CDP/CDP_ECensusphi3.RData')
load('../census/CDP/CDP_SCensusphi3.RData')
load('../census/CDP/CDP_WCensusphi3.RData')
load('../census/CDP/CDP_NICensusphi3.RData')

load('../census/CDP/CDP_ECensusphi4.RData')
load('../census/CDP/CDP_SCensusphi4.RData')
load('../census/CDP/CDP_WCensusphi4.RData')
load('../census/CDP/CDP_NICensusphi4.RData')

load('../census/CDP/CDP_ECensusphi5.RData')
load('../census/CDP/CDP_SCensusphi5.RData')
load('../census/CDP/CDP_WCensusphi5.RData')
load('../census/CDP/CDP_NICensusphi5.RData')

load('../census/CDO/CDO_ECensus.RData')
load('../census/CDO/CDO_SCensus.RData')
load('../census/CDO/CDO_WCensus.RData')
load('../census/CDO/CDO_NICensus.RData')

load('../census/CDO/CDO_ECensusphi1.RData')
load('../census/CDO/CDO_SCensusphi1.RData')
load('../census/CDO/CDO_WCensusphi1.RData')
load('../census/CDO/CDO_NICensusphi1.RData')

load('../census/CDO/CDO_ECensusphi2.RData')
load('../census/CDO/CDO_SCensusphi2.RData')
load('../census/CDO/CDO_WCensusphi2.RData')
load('../census/CDO/CDO_NICensusphi2.RData')

load('../census/CDO/CDO_ECensusphi3.RData')
load('../census/CDO/CDO_SCensusphi3.RData')
load('../census/CDO/CDO_WCensusphi3.RData')
load('../census/CDO/CDO_NICensusphi3.RData')

load('../census/CDO/CDO_ECensusphi4.RData')
load('../census/CDO/CDO_SCensusphi4.RData')
load('../census/CDO/CDO_WCensusphi4.RData')
load('../census/CDO/CDO_NICensusphi4.RData')

load('../census/CDO/CDO_ECensusphi5.RData')
load('../census/CDO/CDO_SCensusphi5.RData')
load('../census/CDO/CDO_WCensusphi5.RData')
load('../census/CDO/CDO_NICensusphi5.RData')

load('../census/GE/GE_ECensus.RData')
load('../census/GE/GE_SCensus.RData')
load('../census/GE/GE_WCensus.RData')
load('../census/GE/GE_NICensus.RData')

load('../census/GE/GE_ECensusphi1.RData')
load('../census/GE/GE_SCensusphi1.RData')
load('../census/GE/GE_WCensusphi1.RData')
load('../census/GE/GE_NICensusphi1.RData')

load('../census/GE/GE_ECensusphi2.RData')
load('../census/GE/GE_SCensusphi2.RData')
load('../census/GE/GE_WCensusphi2.RData')
load('../census/GE/GE_NICensusphi2.RData')

load('../census/GE/GE_ECensusphi3.RData')
load('../census/GE/GE_SCensusphi3.RData')
load('../census/GE/GE_WCensusphi3.RData')
load('../census/GE/GE_NICensusphi3.RData')

load('../census/GE/GE_ECensusphi4.RData')
load('../census/GE/GE_SCensusphi4.RData')
load('../census/GE/GE_WCensusphi4.RData')
load('../census/GE/GE_NICensusphi4.RData')

load('../census/GE/GE_ECensusphi5.RData')
load('../census/GE/GE_SCensusphi5.RData')
load('../census/GE/GE_WCensusphi5.RData')
load('../census/GE/GE_NICensusphi5.RData')

load('../census/GP/GP_ECensus.RData')
load('../census/GP/GP_SCensus.RData')
load('../census/GP/GP_WCensus.RData')
load('../census/GP/GP_NICensus.RData')

load('../census/GP/GP_ECensusphi1.RData')
load('../census/GP/GP_SCensusphi1.RData')
load('../census/GP/GP_WCensusphi1.RData')
load('../census/GP/GP_NICensusphi1.RData')

load('../census/GP/GP_ECensusphi2.RData')
load('../census/GP/GP_SCensusphi2.RData')
load('../census/GP/GP_WCensusphi2.RData')
load('../census/GP/GP_NICensusphi2.RData')

load('../census/GP/GP_ECensusphi3.RData')
load('../census/GP/GP_SCensusphi3.RData')
load('../census/GP/GP_WCensusphi3.RData')
load('../census/GP/GP_NICensusphi3.RData')

load('../census/GP/GP_ECensusphi4.RData')
load('../census/GP/GP_SCensusphi4.RData')
load('../census/GP/GP_WCensusphi4.RData')
load('../census/GP/GP_NICensusphi4.RData')

load('../census/GP/GP_ECensusphi5.RData')
load('../census/GP/GP_SCensusphi5.RData')
load('../census/GP/GP_WCensusphi5.RData')
load('../census/GP/GP_NICensusphi5.RData')

load('../census/GO/GO_ECensus.RData')
load('../census/GO/GO_SCensus.RData')
load('../census/GO/GO_WCensus.RData')
load('../census/GO/GO_NICensus.RData')

load('../census/GO/GO_ECensusphi1.RData')
load('../census/GO/GO_SCensusphi1.RData')
load('../census/GO/GO_WCensusphi1.RData')
load('../census/GO/GO_NICensusphi1.RData')

load('../census/GO/GO_ECensusphi2.RData')
load('../census/GO/GO_SCensusphi2.RData')
load('../census/GO/GO_WCensusphi2.RData')
load('../census/GO/GO_NICensusphi2.RData')

load('../census/GO/GO_ECensusphi3.RData')
load('../census/GO/GO_SCensusphi3.RData')
load('../census/GO/GO_WCensusphi3.RData')
load('../census/GO/GO_NICensusphi3.RData')

load('../census/GO/GO_ECensusphi4.RData')
load('../census/GO/GO_SCensusphi4.RData')
load('../census/GO/GO_WCensusphi4.RData')
load('../census/GO/GO_NICensusphi4.RData')

load('../census/GO/GO_ECensusphi5.RData')
load('../census/GO/GO_SCensusphi5.RData')
load('../census/GO/GO_WCensusphi5.RData')
load('../census/GO/GO_NICensusphi5.RData')

load('../census/ERadiation/ERad_ECensus.RData')
load('../census/ERadiation/ERad_SCensus.RData')
load('../census/ERadiation/ERad_WCensus.RData')
load('../census/ERadiation/ERad_NICensus.RData')

load('../census/ERadiation/ERad_ECensusphi1.RData')
load('../census/ERadiation/ERad_SCensusphi1.RData')
load('../census/ERadiation/ERad_WCensusphi1.RData')
load('../census/ERadiation/ERad_NICensusphi1.RData')

load('../census/ERadiation/ERad_ECensusphi2.RData')
load('../census/ERadiation/ERad_SCensusphi2.RData')
load('../census/ERadiation/ERad_WCensusphi2.RData')
load('../census/ERadiation/ERad_NICensusphi2.RData')

load('../census/ERadiation/ERad_ECensusphi3.RData')
load('../census/ERadiation/ERad_SCensusphi3.RData')
load('../census/ERadiation/ERad_WCensusphi3.RData')
load('../census/ERadiation/ERad_NICensusphi3.RData')

load('../census/ERadiation/ERad_ECensusphi4.RData')
load('../census/ERadiation/ERad_SCensusphi4.RData')
load('../census/ERadiation/ERad_WCensusphi4.RData')
load('../census/ERadiation/ERad_NICensusphi4.RData')

load('../census/ERadiation/ERad_ECensusphi5.RData')
load('../census/ERadiation/ERad_SCensusphi5.RData')
load('../census/ERadiation/ERad_WCensusphi5.RData')
load('../census/ERadiation/ERad_NICensusphi5.RData')

load('../census/Impedance/Imp_ECensus.RData')
load('../census/Impedance/Imp_SCensus.RData')
load('../census/Impedance/Imp_WCensus.RData')
load('../census/Impedance/Imp_NICensus.RData')

load('../census/Impedance/Imp_ECensusphi1.RData')
load('../census/Impedance/Imp_SCensusphi1.RData')
load('../census/Impedance/Imp_WCensusphi1.RData')
load('../census/Impedance/Imp_NICensusphi1.RData')

load('../census/Impedance/Imp_ECensusphi2.RData')
load('../census/Impedance/Imp_SCensusphi2.RData')
load('../census/Impedance/Imp_WCensusphi2.RData')
load('../census/Impedance/Imp_NICensusphi2.RData')

load('../census/Impedance/Imp_ECensusphi3.RData')
load('../census/Impedance/Imp_SCensusphi3.RData')
load('../census/Impedance/Imp_WCensusphi3.RData')
load('../census/Impedance/Imp_NICensusphi3.RData')

load('../census/Impedance/Imp_ECensusphi4.RData')
load('../census/Impedance/Imp_SCensusphi4.RData')
load('../census/Impedance/Imp_WCensusphi4.RData')
load('../census/Impedance/Imp_NICensusphi4.RData')

load('../census/Impedance/Imp_ECensusphi5.RData')
load('../census/Impedance/Imp_SCensusphi5.RData')
load('../census/Impedance/Imp_WCensusphi5.RData')
load('../census/Impedance/Imp_NICensusphi5.RData')

load('../census/Stoufer/Sto_ECensus.RData')
load('../census/Stoufer/Sto_CSCensus.RData')
load('../census/Stoufer/Sto_WCensus.RData')
load('../census/Stoufer/Sto_NICensus.RData')

load('../census/Stoufer/Sto_ECensusphi1.RData')
load('../census/Stoufer/Sto_SCensusphi1.RData')
load('../census/Stoufer/Sto_WCensusphi1.RData')
load('../census/Stoufer/Sto_NICensusphi1.RData')

load('../census/Stoufer/Sto_ECensusphi2.RData')
load('../census/Stoufer/Sto_SCensusphi2.RData')
load('../census/Stoufer/Sto_WCensusphi2.RData')
load('../census/Stoufer/Sto_NICensusphi2.RData')

load('../census/Stoufer/Sto_ECensusphi3.RData')
load('../census/Stoufer/Sto_SCensusphi3.RData')
load('../census/Stoufer/Sto_WCensusphi3.RData')
load('../census/Stoufer/Sto_NICensusphi3.RData')

load('../census/Stoufer/Sto_ECensusphi4.RData')
load('../census/Stoufer/Sto_SCensusphi4.RData')
load('../census/Stoufer/Sto_WCensusphi4.RData')
load('../census/Stoufer/Sto_NICensusphi4.RData')

load('../census/Stoufer/Sto_ECensusphi5.RData')
load('../census/Stoufer/Sto_SCensusphi5.RData')
load('../census/Stoufer/Sto_WCensusphi5.RData')
load('../census/Stoufer/Sto_NICensusphi5.RData')

load('../census/IO/IO_ECensus.RData')
load('../census/IO/IO_CSCensus.RData')
load('../census/IO/IO_WCensus.RData')
load('../census/IO/IO_NICensus.RData')

load('../census/IO/IO_ECensusphi1.RData')
load('../census/IO/IO_SCensusphi1.RData')
load('../census/IO/IO_WCensusphi1.RData')
load('../census/IO/IO_NICensusphi1.RData')

load('../census/IO/IO_ECensusphi2.RData')
load('../census/IO/IO_SCensusphi2.RData')
load('../census/IO/IO_WCensusphi2.RData')
load('../census/IO/IO_NICensusphi2.RData')

load('../census/IO/IO_ECensusphi3.RData')
load('../census/IO/IO_SCensusphi3.RData')
load('../census/IO/IO_WCensusphi3.RData')
load('../census/IO/IO_NICensusphi3.RData')

load('../census/IO/IO_ECensusphi4.RData')
load('../census/IO/IO_SCensusphi4.RData')
load('../census/IO/IO_WCensusphi4.RData')
load('../census/IO/IO_NICensusphi4.RData')

load('../census/IO/IO_ECensusphi5.RData')
load('../census/IO/IO_SCensusphi5.RData')
load('../census/IO/IO_WCensusphi5.RData')
load('../census/IO/IO_NICensusphi5.RData')

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
                 fitCDO_CEphi1,
                 'CDO','England Census',
                 CDOmatflux,0.1)

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitCDE_CEphi1,
                                       'CDE','England Census',
                                       CDEmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitCDP_CEphi1,
                                       'CDP','England Census',
                                       CDPmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitImp_CEphi1,
                                       'Imp','England Census',
                                       Impmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitERad_CEphi1,
                                       'ERad','England Census',
                                       ERadmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitSto_CEphi1,
                                       'Sto','England Census',
                                       Stomatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Ecensus_mobility_dat,
                                       fitIO_CEphi1,
                                       'IO','England Census',
                                       IOmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                 fitCDO_CEphi1,
                 'CDO','Wales Census',
                 CDOmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitCDE_CWphi1,
                                       'CDE','Wales Census',
                                       CDEmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitCDP_CWphi1,
                                       'CDP','Wales Census',
                                       CDPmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitImp_CWphi1,
                                       'Imp','Wales Census',
                                       Impmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitERad_CWphi1,
                                       'ERad','Wales Census',
                                       ERadmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitSto_CWphi1,
                                       'Sto','Wales Census',
                                       Stomatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(Wcensus_mobility_dat,
                                       fitIO_CWphi1,
                                       'IO','Wales Census',
                                       IOmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDO_CNIphi1,
                                       'CDO','NI Census',
                                       CDOmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDE_CNIphi1,
                                       'CDE','NI Census',
                                       CDEmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitCDP_CNIphi1,
                                       'CDP','NI Census',
                                       CDPmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitImp_CNIphi1,
                                       'Imp','NI Census',
                                       Impmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitERad_CNIphi1,
                                       'ERad','NI Census',
                                       ERadmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitSto_CNIphi1,
                                       'Sto','NI Census',
                                       Stomatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NIcensus_mobility_dat,
                                       fitIO_CNIphi1,
                                       'IO','NI Census',
                                       IOmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDO_CSphi1,
                                       'CDO','Scotland Census',
                                       CDOmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDE_CSphi1,
                                       'CDE','Scotland Census',
                                       CDEmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitCDP_CSphi1,
                                       'CDP','Scotland Census',
                                       CDPmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitImp_CSphi1,
                                       'Imp','Scotland Census',
                                       Impmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitERad_CSphi1,
                                       'ERad','Scotland Census',
                                       ERadmatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitSto_CSphi1,
                                       'Sto','Scotland Census',
                                       Stomatflux,0.1))

CPCpost <- CPCpost %>% bind_rows(CPCpp(scotland_census_mobility_dat,
                                       fitIO_CSphi1,
                                       'IO','Scotland Census',
                                       IOmatflux,0.1))

save(CPCpost,file='CPCpost_phi1.RData')




