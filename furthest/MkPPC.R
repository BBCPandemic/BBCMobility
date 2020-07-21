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

load('../../lads_map.RData')
load('../flux/mobility_flux_furthest.RData')
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

load('./ERadiation/uk/ERadTotal.RData')
load('./ERadiation/uk/ERadUnder18.RData')
load('./ERadiation/uk/ERadEducation.RData')
load('./ERadiation/uk/ERadEmployed.RData')
load('./ERadiation/uk/ERadNEET.RData')

load('./CDE/uk/CDETotal.RData')
load('./CDE/uk/CDEUnder18.RData')
load('./CDE/uk/CDEEducation.RData')
load('./CDE/uk/CDEEmployed.RData')
load('./CDE/uk/CDENEET.RData')

load('./CDO/uk/CDOTotal.RData')
load('./CDO/uk/CDOUnder18.RData')
load('./CDO/uk/CDOEducation.RData')
load('./CDO/uk/CDOEmployed.RData')
load('./CDO/uk/CDONEET.RData')

load('./CDP/uk/CDPTotal.RData')
load('./CDP/uk/CDPUnder18.RData')
load('./CDP/uk/CDPEducation.RData')
load('./CDP/uk/CDPEmployed.RData')
load('./CDP/uk/CDPNEET.RData')

load('./IO/uk/IOTotal.RData')
load('./IO/uk/IOUnder18.RData')
load('./IO/uk/IOEducation.RData')
load('./IO/uk/IOEmployed.RData')
load('./IO/uk/IONEET.RData')

load('./Stoufer/uk/StoTotal.RData')
load('./Stoufer/uk/StoUnder18.RData')
load('./Stoufer/uk/StoEducation.RData')
load('./Stoufer/uk/StoEmployed.RData')
load('./Stoufer/uk/StoNEET.RData')

load('./Impedance/uk/ImpTotal.RData')
load('./Impedance/uk/ImpUnder18.RData')
load('./Impedance/uk/ImpEducation.RData')
load('./Impedance/uk/ImpEmployed.RData')
load('./Impedance/uk/ImpNEET.RData')

load('./ERadiation/subnational/E_ERadTotal.RData')
load('./ERadiation/subnational/W_ERadTotal.RData')
load('./ERadiation/subnational/S_ERadTotal.RData')
load('./ERadiation/subnational/NI_ERadTotal.RData')

load('./CDP/subnational/E_CDPTotal.RData')
load('./CDP/subnational/W_CDPTotal.RData')
load('./CDP/subnational/S_CDPTotal.RData')
load('./CDP/subnational/NI_CDPTotal.RData')

load('./CDE/subnational/E_CDETotal.RData')
load('./CDE/subnational/W_CDETotal.RData')
load('./CDE/subnational/S_CDETotal.RData')
load('./CDE/subnational/NI_CDETotal.RData')

load('./CDO/subnational/E_CDOTotal.RData')
load('./CDO/subnational/W_CDOTotal.RData')
load('./CDO/subnational/S_CDOTotal.RData')
load('./CDO/subnational/NI_CDOTotal.RData')

load('./IO/subnational/E_IOTotal.RData')
load('./IO/subnational/W_IOTotal.RData')
load('./IO/subnational/S_IOTotal.RData')
load('./IO/subnational/NI_IOTotal.RData')

load('./Impedance/subnational/E_ImpTotal.RData')
load('./Impedance/subnational/W_ImpTotal.RData')
load('./Impedance/subnational/S_ImpTotal.RData')
load('./Impedance/subnational/NI_ImpTotal.RData')

load('./Stoufer/subnational/E_StoTotal.RData')
load('./Stoufer/subnational/W_StoTotal.RData')
load('./Stoufer/subnational/S_StoTotal.RData')
load('./Stoufer/subnational/NI_StoTotal.RData')

#load('./CDE/age/CDEUnder18.RData')
load('./CDE/age/CDE18_30.RData')
load('./CDE/age/CDE30_60.RData')
load('./CDE/age/CDE60_100.RData')

#load('./CDO/age/CDOUnder18.RData')
load('./CDO/age/CDO18_30.RData')
load('./CDO/age/CDO30_60.RData')
load('./CDO/age/CDO60_100.RData')

#load('./CDP/age/CDPUnder18.RData')
load('./CDP/age/CDP18_30.RData')
load('./CDP/age/CDP30_60.RData')
load('./CDP/age/CDP60_100.RData')

#load('./ERadiation/age/ERadUnder18.RData')
load('./ERadiation/age/ERad18_30.RData')
load('./ERadiation/age/ERad30_60.RData')
load('./ERadiation/age/ERad60_100.RData')

#load('./IO/age/IOUnder18.RData')
load('./IO/age/IO18_30.RData')
load('./IO/age/IO30_60.RData')
load('./IO/age/IO60_100.RData')

#load('./Impedance/age/ImpUnder18.RData')
load('./Impedance/age/Imp18_30.RData')
load('./Impedance/age/Imp30_60.RData')
load('./Impedance/age/Imp60_100.RData')

#load('./Stoufer/age/StoUnder18.RData')
load('./Stoufer/age/Sto18_30.RData')
load('./Stoufer/age/Sto30_60.RData')
load('./Stoufer/age/Sto60_100.RData')

source('./PPDfuncs.R')

CPCpost <- CPCpp(total_mobility_dat,
                                    fitCDOT,
                                    'CDO','BBC Total (UK)',
                                    CDOmatflux)  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitCDOU,
                                       'CDO','BBC Under 18 (UK)',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitCDOU,
                                       'CDO','BBC 18-30 (UK)',
                                       CDOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitCDOU,
                                       'CDO','BBC 30-60 (UK)',
                                       CDOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitCDOU,
                                       'CDO','BBC 60-100 (UK)',
                                       CDOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitCDOEd,
                                       'CDO','BBC Education (UK)',
                                       CDOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitCDOEm,
                                       'CDO','BBC Employed (UK)',
                                       CDOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitCDON,
                                       'CDO','BBC NEET (UK)',
                                       CDOmatflux))  



CPCpost <- CPCpost %>% bind_rows(CPCpp(total_mobility_dat,
                                       fitCDET,
                                       'CDE','BBC Total (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitCDEU,
                                       'CDE','BBC Under 18 (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitCDEU,
                                       'CDE','BBC 18-30 (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitCDEU,
                                       'CDE','BBC 30-60 (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitCDEU,
                                       'CDE','BBC 60-100 (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitCDEEd,
                                       'CDE','BBC Education (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitCDEEm,
                                       'CDE','BBC Employed (UK)',
                                       CDEmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitCDEN,
                                       'CDE','BBC NEET (UK)',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(total_mobility_dat,
                                       fitCDPT,
                                       'CDP','BBC Total (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitCDPU,
                                       'CDP','BBC Under 18 (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitCDPU,
                                       'CDP','BBC 18-30 (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitCDPU,
                                       'CDP','BBC 30-60 (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitCDPU,
                                       'CDP','BBC 60-100 (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitCDPEd,
                                       'CDP','BBC Education (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitCDPEm,
                                       'CDP','BBC Employed (UK)',
                                       CDPmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitCDPN,
                                       'CDP','BBC NEET (UK)',
                                       CDPmatflux))




CPCpost <- CPCpost %>% bind_rows(CPCpp(total_mobility_dat,
                                       fitERadT,
                                       'ERad','BBC Total (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitERadU,
                                       'ERad','BBC Under 18 (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitERadU,
                                       'ERad','BBC 18-30 (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitERadU,
                                       'ERad','BBC 30-60 (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitERadU,
                                       'ERad','BBC 60-100 (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitERadEd,
                                       'ERad','BBC Education (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitERadEm,
                                       'ERad','BBC Employed (UK)',
                                       ERadmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitERadN,
                                       'ERad','BBC NEET (UK)',
                                       ERadmatflux))




CPCpost <- CPCpost %>% bind_rows(CPCpp(total_mobility_dat,
                                       fitStoT,
                                       'Sto','BBC Total (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitStoU,
                                       'Sto','BBC Under 18 (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitStoU,
                                       'Sto','BBC 18-30 (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitStoU,
                                       'Sto','BBC 30-60 (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitStoU,
                                       'Sto','BBC 60-100 (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitStoEd,
                                       'Sto','BBC Education (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitStoEm,
                                       'Sto','BBC Employed (UK)',
                                       Stomatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitStoN,
                                       'Sto','BBC NEET (UK)',
                                       Stomatflux))





CPCpost <- CPCpost %>% bind_rows(CPCpp(total_mobility_dat,
                                       fitImpT,
                                       'Imp','BBC Total (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitImpU,
                                       'Imp','BBC Under 18 (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitImpU,
                                       'Imp','BBC 18-30 (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitImpU,
                                       'Imp','BBC 30-60 (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitImpU,
                                       'Imp','BBC 60-100 (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitImpEd,
                                       'Imp','BBC Education (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitImpEm,
                                       'Imp','BBC Employed (UK)',
                                       Impmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitImpN,
                                       'Imp','BBC NEET (UK)',
                                       Impmatflux))



CPCpost <- CPCpost %>% bind_rows(CPCpp(total_mobility_dat,
                                       fitIOT,
                                       'IO','BBC Total (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(under18_mobility_dat,
                                       fitIOU,
                                       'IO','BBC Under 18 (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a18_30_mobility_dat,
                                       fitIOU,
                                       'IO','BBC 18-30 (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a30_60_mobility_dat,
                                       fitIOU,
                                       'IO','BBC 30-60 (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(a60_100_mobility_dat,
                                       fitIOU,
                                       'IO','BBC 60-100 (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(education_mobility_dat,
                                       fitIOEd,
                                       'IO','BBC Education (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(employed_mobility_dat,
                                       fitIOEm,
                                       'IO','BBC Employed (UK)',
                                       IOmatflux))  

CPCpost <- CPCpost %>% bind_rows(CPCpp(neet_mobility_dat,
                                       fitION,
                                       'IO','BBC NEET (UK)',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_CDOT,
                                       'CDO','BBC Total (England)',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_CDET,
                                       'CDE','BBC Total (England)',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_CDPT,
                                       'CDP','BBC Total (England)',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_ERadT,
                                       'ERad','BBC Total (England)',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_IOT,
                                       'IO','BBC Total (England)',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_ImpT,
                                       'Imp','BBC Total (England)',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(E_total_mobility_dat,
                                       fit_E_StoT,
                                       'Sto','BBC Total (England)',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_CDOT,
                                       'CDO','BBC Total (Wales)',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_CDET,
                                       'CDE','BBC Total (Wales)',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_CDPT,
                                       'CDP','BBC Total (Wales)',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_ERadT,
                                       'ERad','BBC Total (Wales)',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_IOT,
                                       'IO','BBC Total (Wales)',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_ImpT,
                                       'Imp','BBC Total (Wales)',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(W_total_mobility_dat,
                                       fit_W_StoT,
                                       'Sto','BBC Total (Wales)',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_CDOT,
                                       'CDO','BBC Total (Scotland)',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_CDET,
                                       'CDE','BBC Total (Scotland)',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_CDPT,
                                       'CDP','BBC Total (Scotland)',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_ERadT,
                                       'ERad','BBC Total (Scotland)',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_IOT,
                                       'IO','BBC Total (Scotland)',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_ImpT,
                                       'Imp','BBC Total (Scotland)',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(S_total_mobility_dat,
                                       fit_S_StoT,
                                       'Sto','BBC Total (Scotland)',
                                       Stomatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_CDOT,
                                       'CDO','BBC Total (NI)',
                                       CDOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_CDET,
                                       'CDE','BBC Total (NI)',
                                       CDEmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_CDPT,
                                       'CDP','BBC Total (NI)',
                                       CDPmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_ERadT,
                                       'ERad','BBC Total (NI)',
                                       ERadmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_IOT,
                                       'IO','BBC Total (NI)',
                                       IOmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_ImpT,
                                       'Imp','BBC Total (NI)',
                                       Impmatflux))

CPCpost <- CPCpost %>% bind_rows(CPCpp(NI_total_mobility_dat,
                                       fit_NI_StoT,
                                       'Sto','BBC Total (NI)',
                                       Stomatflux))

save(CPCpost,file='CPCpost.RData')

