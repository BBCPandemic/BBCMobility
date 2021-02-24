# BBC Mobility Data

The BBC Mobility data is provided as a set of origin-destination flux matrices aggregated at the (2011) local administrative district (LAD) level as described in: https://www.medrxiv.org/content/10.1101/2021.02.19.21252079v1

The origin and destination for each user was calculated using the first 24 hours of recorded positions after they first used the BBC Pandemic app. Two sets of data products are provide for alternate definitions of origin and destination - the furthest extent (furthest) and next most frequent (next).

# Definition of Origin-Destination Matrices

User locations were first snapped to the nearest MSOA based on the generalised (20m resolution) shape file provided by the Open Geography portal from the Office of National Statistics (ONS).

Locations outside of the boundary of any MSOA were either excluded or snapped to the MSOA with the greatest area of overlap within a 1km buffer centered around the user location. A home (origin) location was defined for each user as their modal MSOA. In defining the duration of time spent in a location we needed to account for missing location logs for some users who moved into an area with poor service, or switched off their phone, during the observation period. For such gaps we make the assumption the user remained in the last seen location until a new location was logged and use the duration of time within each location to calculate the modal location. In the event of a tie we chose the location with the least amount of time spent in the 12 hours between 7am and 6pm (inclusive). Users to which we could not assign a home location were removed from the data set for analysis.

For destination locations, two alternative definitions were considered - the furthest extent and the second most frequent recorded location after home (which we will refer to as the ‘next’ location for convenience). For the ‘next’ location we again ranked locations based on the total duration of time spent including the inferred location between gaps as described above. In the event of ties the location with the greater proportion of time between 7am and 6pm was chosen. Once again, users to whom we could not assign a unique destination location were removed from the data set. In total there were 4,450 users we could not assign a unique origin and destination location according these definitions leaving a total of 43,291 users within the final BBC mobility data set. For users with all location records in the same MSOA, their home and destination locations are both set to this unique value. As the LAD origin and destinations are mapped from these MSOA locations, at the LAD level, users can therefore have the same inferred origin and destination locations (even though they have moved between different MSOAs over the course of the reporting period).

This public data set is limited to this (LAD) scale to preserve the anonymity of BBC Pandemic app participants and allow for comparison to the 2011 census workflow data published separately by the Office of National Statistics (for England and Wales), Scotland's Census and the The Northern Ireland Statistics and Research Agency. 

For convenience we provide a copy of the aggregated simple feature collection (sf) map used for this aggregation ('./maps/lads_map.RData') along with copies of the original open access data (links to original sources below), the scripts to create this map and associated look-up tables. However, due to size constraints the England and Wales census workflow data has been removed - it can be downloaded directly from NOMIS: https://www.nomisweb.co.uk/census/2011/wf02ew

The raw BBC Mobility data is found within the "./flux" directory under several aggregations at the national and subnational level (england, ni, scotland and wales subdirectories). 

Files with the "total" subscript correspond to the aggregate flux across all stratifications of the BBC Pandemic data set. These flows are stratified further by employment category (as defined in manuscript above) with the relevant prefix under18, employed, eduction, neet. The total flux at the national or subnational level is therefore the sum of these four categories.

Stratifications by age are provided at the national level only within "./flux/age/".

# Code and Analysis

Code for all analyses presented in the manuscript can be found in "./next" with supplementary analysis using the "furthest" extent definition in "./furthest". Shared stan code implementing the mobility models estimated in the paper can be found in "./stan" while analysis of the Census workflow data resides in "./census".

Finally we provided imputed national level flux matrices ("./imputed") for the best fit (CDO) models from the manuscript. 

# Source files for maps and census workflow data

Demography, Total population, age distribution

https://www.nomisweb.co.uk/census/2011/ks102uk

https://www.ninis2.nisra.gov.uk/public/PivotGrid.aspx?ds=7462&lh=75&yn=2011&sk=136&sn=Census%202011&yearfilter=

https://www.nomisweb.co.uk/census/2011/qs103uk

Lookup Tables

https://www.ninis2.nisra.gov.uk/public/SearchResults.aspx?sk=CT0285NI;

https://data.nicva.org/dataset/northern-ireland-council-lookup-tables/resource/6c374733-c805-409e-8e98-d042ca9a9143

https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/11DC_Lookup_1_1.xls.

https://data.gov.uk/dataset/61fc9c81-03c7-4c3d-a952-6c450eb6f50c/workplace-zone-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales

https://www.nrscotland.gov.uk/files/geography/2011-census/OA_DZ_IZ_2011.xlsx

Shape files

Mid-layer Super Output Areas for England and Wales
http://geoportal.statistics.gov.uk/datasets?q=MSOA_Boundaries_2011&sort=name

Scotland Intermediate Zone Boundaries 2011
https://data.gov.uk/dataset/intermediate-zone-boundaries-2011/resource/27521094-895a-4644-b2cc-57ffb69a0ed2

Northern Ireland Super Output Areas 
https://www.nisra.gov.uk/support/geography/northern-ireland-super-output-areas

Northern Ireland Local Government Districts

https://data.gov.uk/dataset/e03cde94-639c-4faa-bed5-8e57d9e1fc80/osni-open-data-largescale-boundaries-local-government-districts-2012

NUTS Level 2 (UK)

https://geoportal.statistics.gov.uk/datasets/48b6b85bb7ea43699ee85f4ecd12fd36_0

Workplace Zones (December 2011, England and Wales)

https://data.gov.uk/dataset/606f3297-bb39-4d89-91ef-34943ef786af/workplace-zones-december-2011-generalised-clipped-boundaries-in-england-and-wales

Workflow data

Location of usual residence and place of work (England and Wales)

https://www.nomisweb.co.uk/census/2011/wf02ew

Scotlands Census Usual residence and place of work

http://www.scotlandscensus.gov.uk/documents/additional_tables/WU03BSC_IZ2011_Scotland.xlsx