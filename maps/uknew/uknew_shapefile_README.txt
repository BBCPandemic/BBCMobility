*****************************************************************************
*** Readme file for uknew shapefile
*** Author: Petra Klepac, LSHTM
*****************************************************************************


This shape file is a composite mid-layer geography for the entire UK. The data attached to this shape file consists of the following fields: 

1. mlcode (mid layer code for each polygon, the original code names differ for England&Wales, Scotland and Northern Ireland and are described below), 
2. Name (name of this mid-layer region)
3.  TotPop  ( total population from UK 2011 census (file 3257816271.csv, ONS reference file KS102UK - Age Structure, downloaded from ONS Crown Copyright Reserved [from Nomis on 14 March 2018]) 
4. AreaKm2 (calculated from the polygons)
5. PopDensity (TotPop/AreaKm2)          
6. x       (longitude of the centroid) 
7. y       (latitude of the centroid)
8. RU 	   (rural -urban classification - see rural urban readme text-file for details)
9.  country (England, Scotland, Wales, Northern Ireland)


SHAPEFILE can be constructed by unifying 3 different geographies:

1) Mid-layer Super Output Areas for England and Wales
	- geography code for lookup tables “msoa11cd” 
	- 7201 patches
	- available from: http://geoportal.statistics.gov.uk/datasets?q=MSOA_Boundaries_2011&sort=name

2) Scotland Intermediate Zone Boundaries 2011
	- geography code for lookup tables “InterZone”
	- 1279 patches
	- available from: https://data.gov.uk/dataset/intermediate-zone-boundaries-2011/resource/27521094-895a-4644-b2cc-57ffb69a0ed2

3) Northern Ireland Super Output Areas 
	- geography code for lookup tables “SOA_CODE”
	- 890 patches	
	- available from https://www.nisra.gov.uk/support/geography/northern-ireland-super-output-areas


*****************************************************************************

Details that might be useful (for changing projection later, etc.). This shape file is projected in 
CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"). Needs to be changed to OSGB36

summary(uk.new) * after spTransform(uk.new, CRS("+init=epsg:27700"))
Object of class SpatialPolygonsDataFrameCoordinates:         min       maxx  -70.18508  655595.9y 7439.62914 1220301.5Is projected: TRUE proj4string :[+init=epsg:27700 +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000+datum=OSGB36 +units=m +no_defs +ellps=airy+towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894]Data attributes:      mlcode             Name          TotPop         AreaKm2           PopDensity        95AA01S1:   1   South     : 251   Min.   :  364   Min.   :   0.128   Min.   :    0.884   95AA01S2:   1   North     : 200   1st Qu.: 5605   1st Qu.:   1.606   1st Qu.:  476.344   95AA01S3:   1   East      : 162   Median : 7006   Median :   3.146   Median : 2143.437   95AA02W1:   1   Birmingham: 132   Mean   : 6743   Mean   :  29.445   Mean   : 2773.756   95AA03W1:   1   Leeds     : 107   3rd Qu.: 8313   3rd Qu.:  13.454   3rd Qu.: 3836.258   95AA04W1:   1   Cheshire  :  98   Max.   :16342   Max.   :3943.236   Max.   :27197.510   (Other) :9364   (Other)   :8420                                                                x                 y             RU       country  Min.   :-7.9964   Min.   :49.92   Rural:2091   9: 890   1st Qu.:-3.1992   1st Qu.:51.55   Urban:7279   E:6791   Median :-1.8721   Median :52.78                S:1279   Mean   :-2.1423   Mean   :53.09                W: 410   3rd Qu.:-0.5151   3rd Qu.:54.47                         Max.   : 1.7474   Max.   :60.64  