# Title: "Shrubification in the Western Arctic and its Effects on the Porcupine Caribou Herd Habitat"

## Erica Zaja - 13/10/2021
## Proposed methods 

***********
#### Aim: 

•	Investigate vegetation change in the Porcupine Caribou Herd Alaskan summer range

#### Objectives:

•	Study the relationships between temperature, precipitation, phenology and shrub cover.

•	Compare landscape and plot-based vegetation cover estimates.

*****************

# Research Questions: 

### Q1: How much of the PCH Alaska summer range is shrub cover? - Static shrub cover in 2016

##### Methods
1.	Overlay shrub map of North Slope of Alaska of 2016 (Berner et al., 2021) over caribou summer range data 1983 - 2001 (Griffith, 2002). 
2.	Crop shrub map to caribou summer range.
3.	Estimate of shrub % cover within the PCH range.

##### Results and Figures:
-	Shrub % cover (+/- S.E) within the PCH summer range in 2016. 
-	Figure 1: shrub cover in summer range. 

##### Questions:
•	How do I quantify % cover from map?

*********

### Q2: How much vegetation change has occurred in the PCH Alaska summer range? - Change in NDVI (2007 to 2016)

##### Methods: 
1.	Extract NDVI values in the caribou summer range (Fig.1).
2.	Plot mean NDVI (+/- S.E.) over time (2007-2016). 
3.	Calculate percentage change in NDVI over the timeline. 

##### Results and Figures:
-	Result is a % increase or decrease in NDVI over the 2007-2016 period.
-	Figure 2: Point-plot (point +/- S.E.): years on x axis and NDVI estimates on y axis.

##### Stats Analysis: 
-	lm(avg_NDVI ~ year + (1|year/region/plot))

##### Questions:
•	How do I relate NDVI to shrub? NDVI doesnt discriminate veg types...

********
### Q3: Are shrubs found in warmer and wetter areas of the PCH Alaska summer range? 

##### Methods:
1.	Subset the cropped shrub map (Figure 1) into sub-regions (polygons), within each region, decide on number of plots. 
2.	Randomly point-extract summer temperatures and precipitation from each plot. 
3.	See if each pixel that you extracted temperature and precipitation from is shrub cover or is not shrub cover (record ‘yes’ or ‘no’ or ‘0’ or ‘1’) – Binary data
4.	Calculate average summer temperature and average precipitation per plot. 
5.	Calculate average shrub (‘yes or no’) per plot (i.e. on average, shrubs are found in this plot yes or no). 
6.	Plot the data and establish if shrubs are found in plots with high average summer temperature and precipitation. 

##### Results and Figures: 
-	Result is a statement on whether shrubs are found in warmer and wetter areas of the PCH summer range. 
-	Figure 3: boxplot with shrub (yes or no BINARY data) x axis and average temperatures and precipitation on y axes (continuous variable).

##### Stats Analysis: 
-	lm(shrub_presence ~ avg_temperature + (1|region/plot))
-	lm(shrub_presence ~ avg_precipitation + (1|region/plot))

##### Questions:
•	CHELSA Data stops at 2013…? If it’s an issue I can: Download Google Earth Pro + from CRU website download kml file. Open CRU into Google Earth, divide area into plots and extract temp and precipitation data OR Use climatologies for temperature and precipitation? 

•	Need to understand how to divide map into polygons, how to make plots (how many/how big), point extract from grid (?), random sampling?

****************

### Q4: Is there more vegetation change in areas that have warmed more in PCH Alaska summer range?

##### Methods:
1.	Relate mean NDVI with average summer temperatures over the years (2007-2016). 

##### Results and Figures: 
-	Result is a statement on the relationship between NDVI change and temperature change over the years.
-	“NDVI significantly increased with temperature (LM: …)”

##### Stats analysis
-	lm(avg_NDVI ~ temperature_change + year) + (1|region/plot))

**************
### Q5: Do plot-based estimates of shrub cover change match with landscape scale estimates?

##### Methods: 
1.	Analyse vegetation change (cover of moss, lichen, shrubs) in the Arctic National Wildlife Refuge area (ITEX data) over time. 

##### Results and Figures:
-	Result will be increase/decrease/no change in each vegetation type over time  
-	Figure: years on x, percentage cover on y (one line per vegetation type or facet plot). 

##### Stats analysis:
lm(shrub_cover ~ year )
lm(lichen_cover ~year)
lm(moss_cover~year)
-	Extract slopes , and plot cover change variable over time.
-	Compare change variables over time (and space??)

##### Questions:
•	Need to pick main functional classes of vegetation 

******************

### Q6: What are the early versus late phenology years in the PCH Alaska summer range region? 

##### Methods:
1.	Pick a phenology variable (eg. 50% max NDVI, first salix leaf bud appearance) from ITEX data. 
2.	Plot phenology variable over time: phenology variable on the y , Day of year DOI on x
3.	Determine in which year phenology variable is earliest VS latest in the year

##### Results and Figures
-	Results: early year = , late year = 
-	Figure: phenology variable over time , 
-	Make a threshold (‘first leaf bud after day n is late phenology year’)
- look into library(esquisse)

**************

### Q7: Is there more shrub cover and shrub cover change in calving grounds in early (2015) versus late (2018) phenology years (polygons from Severson et al. 2021)

##### confused about this one

**************

### Datasets:

●	Shrub cover - Berner et al. shrub cover map: https://arcticdata.io/catalog/view/doi%3A10.18739%2FA25Q4RN03 

●	Climate - Chelsa data, climatology and climate change 
-	average mean summer temperature per year (1979-2013) 
-	average mean precipitation per year (1979-2013) 

OR Google Earth + CRU temeprature and precipitation data 

●	Vegetation (shrub, moss, lichen) and phenology data from ITEX 

●	PCH summer range from https://catalog.data.gov/dataset?q=porcupine+caribou+herd&sort=score+desc%2C+name+asc&as_sfid=AAAAAAVkyuEO6_imG5g3XShopgdatMrb4oxngePMAutcoIVz3ASylYJjWYpD6RDzHFYR7TU6p8EQPOMpUnfy0wIO5RNYjwVv-lUbSPUr6GM3EjK8UKRhsj1oKIz_dnsIy3BxCC8%3D&as_fid=ac32b54adbfcdcdf8dbd1bed5ffe7f3a2be82a89 


