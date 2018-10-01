## Map for FrenchScholarships 

## MapSource http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip 
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(RColorBrewer)

region_location <- "C:/Users/Kroutz-/Dropbox/MOOCS/DATA/DATA_Knight/WORLD_SIMPL/TM_WORLD_BORDERS_SIMPL-0.3.shp"


theRegions <- st_read(region_location)

theRegions$REGION <- as.factor(theRegions$REGION)
theRegions$SUBREGION <- as.factor(theRegions$SUBREGION)

# number of subregions -> nb of colors necessary
# NbSubregions <- length(levels(theRegions$SUBREGION))
# NbRegions  <- length(levels(theRegions$SUBREGION))


## Getting an overview of the subregions to adapt them to our own dataset
## This gives a 3-column dataframe (as a subset from the original shp(sf)-dataframe, automatically includes geometry)

myRegions <- theRegions[, c("NAME", "SUBREGION")]
myRegions$NAME <- as.character(myRegions$NAME)


## Transforming data: ordering subregions by level to check and reassign countries to "our" subregions as second step
OrderedList <- myRegions[order(myRegions$SUBREGION),]
#creates a vector of integers with names for each value, thus adding for each subregion number (here "names") the # of occurences.
levelSumm <- summary(myRegions$SUBREGION)
# MO:145 (+ Iran), EUR:39,151,154,155, Maghreb:15+Egypt+Sudan, 
# AfSubS: 11, 14, 17, 18
# Asia_0: 30,34, 35, 53, 54, 57, 61, 143

#add a column
myRegions$Regions <- as.character(myRegions$SUBREGION)
myRegions$Regions[myRegions$Regions =="0"] <- "Antarctica"

myRegions$Regions[myRegions$SUBREGION %in% c(39, 151, 154, 155)] <- "Europe"
myRegions$Regions[myRegions$SUBREGION %in% c(11, 14, 17, 18)] <- "Sub-Saharan Africa"
myRegions$Regions[myRegions$SUBREGION %in% c(30, 34, 35, 53, 54, 57, 61, 143)] <- "Asia & Oceania"
myRegions$Regions[myRegions$SUBREGION %in% c(5, 13, 21, 29)] <- "America"
myRegions$Regions[myRegions$SUBREGION == 145] <- "Middle East"
myRegions$Regions[myRegions$SUBREGION == 15] <- "Maghreb"

myRegions$Regions[myRegions$NAME=="Iran (Islamic Republic of)"]<- "Middle East"
myRegions$Regions[myRegions$NAME=="Sudan"]<- "Sub-Saharan Africa"
myRegions$Regions[myRegions$NAME=="Egypt"]<- "Middle East"

myRegions$schol <- myRegions$Regions
myRegions$schol[myRegions$schol == "Europe"]<- 2755
myRegions$schol[myRegions$schol == "Middle East"]<- 1697
myRegions$schol[myRegions$schol == "Maghreb"]<- 2897
myRegions$schol[myRegions$schol == "Sub-Saharan Africa"]<- 2906
myRegions$schol[myRegions$schol == "Asia & Oceania"]<- 2662
myRegions$schol[myRegions$schol == "America"]<- 1770
myRegions$schol[myRegions$schol == "Antarctica"]<- 0

myRegions$schol <- as.numeric(myRegions$schol)


myColors <- brewer.pal(12,"Set3") 
names(myColors) <- levels(as.factor(myRegions$Regions))
#colScale <- scale_color_manual(name = "SUBREGION",values = myColors)

myMap <- ggplot(myRegions) + geom_sf(aes(fill=schol,color = Regions))+
        scale_fill_distiller(direction = 1, name =" Number of Scholarships", palette="Oranges")+
        labs(caption="Number of Scholarships Given in 2011 by World Region", subtitle="Scholarships to Foreign Students by the French Government")

print(myMap)

# It is the expected chloropeth map. However, it doesn't look good.
# So we try something else for the final presentation.

myMap2 <- ggplot(myRegions) + geom_sf(aes(fill = Regions))+
        labs(caption="Number of Scholarships Given in 2011 by World Region", subtitle="Scholarships to Foreign Students by the French Government")

# print(myMap2)

ggsave('myMap2.png', myMap2, width = 16, height = 9, dpi = 100)
ggsave('myMap1.png', myMap, width = 16, height = 9, dpi = 100)

