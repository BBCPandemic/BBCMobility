require(tidyverse)
require(sf)
require(ggplot2)
require(ggspatial)
require(sp)
require(viridis)
require(rmapshaper)

uk_map<- st_read('./uknew/uk-new.shp') %>% st_transform(27700)
ireland_map<-st_read('./Census2011_NUTS2_generalised20m/Census2011_NUTS2_generalised20m.shp') %>% st_transform(27700)
wz_map<- st_read('./Census2011/Workplace_Zones_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales/Workplace_Zones_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp') %>% st_transform(27700)
ni_map <- st_read('./NIcensus2011/OSNI_Open_Data__Largescale_Boundaries__Local_Government_Districts_2012/OSNI_Open_Data__Largescale_Boundaries__Local_Government_Districts_2012.shp') %>% st_transform(27700)

# NI map is high resolution - simplify using default parameters of ms_simplify (rmapshaper package)

ni_map <- ms_simplify(ni_map)

lookup_wz   <- as_tibble(read.csv('Census2011/Workplace_Zone_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_December_2017_Lookup_in_Great_Britain__Classifications.csv',stringsAsFactors = FALSE))
lookup_lads <- as_tibble(read.csv('uknew/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_December_2017_Lookup_in_Great_Britain__Classification_Version_2.csv',stringsAsFactors = FALSE))
# lookup_nads <- as_tibble(read.csv('uknew/NISAOLookup.csv',stringsAsFactors = FALSE))
NISOA2LGD   <- as_tibble(read.csv('./NIcensus2011/SOA2001_2_LGD2014.csv',stringsAsFactors = FALSE))


lads_and_nads = lookup_lads %>% 
  select(mlcode=MSOA11CD,lad17cd=LAD17CD,lad17nm=LAD17NM,ctry11NM=CTRY11NM)

# Use LGD 1992 name as lad code and name for NI LSOA
# nads_to_lads <- lookup_nads %>% 
#   select(mlcode=SOA.Code,
#          lad17cd=LGD1992NAME,
#          lad17nm=LGD1992NAME)
# Use LGD 2014 name as lad code and name for NI LSOA for consistency with census

nads_to_lads <- NISOA2LGD %>% 
  select(mlcode=SOA2001,
         lad17cd=LGD2014,
         lad17nm=LGD2014NAME) %>%
  mutate(ctry11NM='Northern Ireland')

lads_and_nads <- rbind(lads_and_nads,nads_to_lads)
lads_and_nads <- lads_and_nads %>% distinct()

save(lads_and_nads,file='lads_and_nads_lookup.RData')

lads_map=uk_map %>% 
  inner_join(lads_and_nads) 

lads_map <- lads_map %>% 
  group_by(lad17cd) %>% 
  summarise(lad17nm=first(lad17nm),
            ctry11NM=first(ctry11NM),
            TotPop=sum(TotPop),
            AreaKm2=sum(AreaKm2),
            geometry=st_union(geometry))

# census_LADS <- (lookup_lads %>% filter(CTRY11NM=='England' | CTRY11NM =='WALES') %>% select(LAD17CD,LAD17NM) %>% group_by(LAD17CD) %>% summarize(first(LAD17NM)))

# Age distribution

NIcensusage<-as_tibble(read.csv('./NIcensus2011/KS102NI.csv'))
censusage <- as_tibble(read.csv('./Census2011/ukmidyear_age.csv'))

censusage <- censusage %>% mutate(under18=censusage %>% 
                                select(num_range('X',0:18)) %>% 
                                rowSums())

censusage <- censusage %>% mutate(a18_30=censusage %>% 
                                select(num_range('X',19:30)) %>% 
                                rowSums())

censusage <- censusage %>% mutate(a30_60=censusage %>% 
                                select(num_range('X',31:60)) %>% 
                                rowSums())

censusage <- censusage %>% mutate(a60_100=censusage %>% 
                                select(num_range('X',61:100)) %>% 
                                rowSums())

censusage = censusage %>% select(lad17cd,all,under18,a18_30,a30_60,a60_100)

NIcensusage <- NIcensusage %>% mutate(under18=Age.0.4.years+Age.5.7.years+Age.8.9.years+Age.10.14.years+Age.15.years+Age.16.17.years)
NIcensusage <- NIcensusage %>% mutate(a18_30=Age.18.19.years + Age.20.24.years + Age.25.29.years)
NIcensusage <- NIcensusage %>% mutate(a30_60=Age.30.44.years+Age.45.59.years)
NIcensusage <- NIcensusage %>% mutate(a60_100=Age.60.64.years+Age.65.74.years+Age.75.84.years+Age.85.89.years)

censusage <- censusage %>% bind_rows(NIcensusage %>% select(lad17cd=LGD2014,all=Total,under18,a18_30,a30_60,a60_100))

lads_map <- lads_map %>% inner_join(censusage)

save(lads_map,file='lads_map.RData')
# save(census_map,file='census_map.RData')
