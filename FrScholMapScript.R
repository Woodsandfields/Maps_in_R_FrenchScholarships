# Map for FrenchScholarships practice case (Knight Foundation)
#*************************************************************
# **** *******************************************************



# LOADING PACKAGES. Checking first they are already available else downloading them.

packages <- c("R.utils", "data.table", "ggplot2", "sf", "readr", "dplyr", "tidyr", "RColorBrewer")

for (package in packages) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package)}
        library(package, character.only=T)
        }



## MapSource http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip 


#In order to work of the file and save time not downloading the shapefile every time, 
# and once you set your working directory to the current location, you might use 
# (comment/uncomment the following line in opposite way to what to have 
# for the preceding download/extract procedure:

region_location <- "WORLD_SIMPL/TM_WORLD_BORDERS_SIMPL-0.3.shp"

theRegions <- st_read(region_location)

# theRegions$REGION <- as.factor(theRegions$REGION)
# theRegions$SUBREGION <- as.factor(theRegions$SUBREGION)

# number of subregions -> nb of colors necessary
# NbSubregions <- length(levels(theRegions$SUBREGION))


## Getting an overview of the subregions to adapt them to our own dataset
## This gives a 3-column dataframe (as a subset from the original shp(sf)-dataframe, automatically includes geometry)

myRegions <- tbl_df(theRegions) %>%
        select(NAME, SUBREGION, LON, LAT, geometry) %>%
        mutate(SUBREGION=as.factor(SUBREGION), NAME=as.character(NAME))


## Transforming data: ordering subregions by level to check and reassign countries to "our" subregions as second step
OrderedList <- myRegions[order(myRegions$SUBREGION),]
#creates a vector of integers with names for each value, thus adding for each subregion number (here "names") the # of occurences.

#levelSumm <- summary(myRegions$SUBREGION)

# MO:145 (+ Iran), EUR:39,151,154,155, Maghreb:15+Egypt+Sudan, 
# AfSubS: 11, 14, 17, 18
# Asia_0: 30,34, 35, 53, 54, 57, 61, 143

#add a column


myRegions <- myRegions %>%
        mutate(Regions_schol = case_when
               (SUBREGION == 0 ~ "Antarctica_NA", 
                SUBREGION %in% c(39,151, 154, 155) ~ "Europe_2755",
                (SUBREGION  == 15 & NAME != "Egypt" & NAME !="Sudan") ~ "Maghreb_2997",
                SUBREGION %in% c(30, 34, 35, 53, 54, 57, 61, 143) ~ "AsiaOceania_2662",
                SUBREGION %in% c(5, 13, 21, 29) ~ "America_1770",
                (SUBREGION == 145 | NAME=="Iran (Islamic Republic of)"
                 | NAME == "Egypt")  ~ "MiddleEast_1697", 
                SUBREGION %in% c(11, 14, 17, 18) | NAME == "Sudan" ~ "SubSaharanAfrica_2906"
                 )) %>%
                separate(Regions_schol, c("Regions", "schol")) 
                                         

myRegions$schol <- as.numeric(myRegions$schol)

# Substituting the regional mean for LON and LAT,
# & Correcting LON/LAT for Asia & Oceania and Antartica Regions 
# to have them more conveniently placed on the map.

myRegions_notRegrouped <- myRegions
myRegions <- myRegions %>%
        group_by(Regions) %>%
        mutate(LON = mean(LON), LAT = mean(LAT),
               LON = case_when(Regions == "AsiaOceania" ~ LON+50, TRUE  ~ LON),
               LAT = case_when(Regions == "Antarctica" ~ LAT-50, TRUE ~ LAT))




### MAKING THE MAP

# myColors <- brewer.pal(12,"Set3") 
# names(myColors) <- levels(as.factor(myRegions$Regions))
# #colScale <- scale_color_manual(name = "SUBREGION",values = myColors)



# Here is the choropeth map as expected in the final document to produce for the Knight Foundation.
# However, it doesn't look good and add less than the format we will try as a next step.
# So we'll try something else for the final presentation and produce two maps instead of one.

myRegions$schol <- as.numeric(myRegions$schol)

myColors <- brewer.pal(12,"Set3") 
names(myColors) <- levels(as.factor(myRegions$Regions))
#colScale <- scale_color_manual(name = "SUBREGION",values = myColors)

myMap <- ggplot(myRegions) + geom_sf(aes(fill=schol,color = Regions))+
        scale_fill_distiller(direction = 1, 
                             name =" Number of Scholarships", palette="Oranges")+
        labs(caption="Number of Scholarships Given by World Region in 2011", 
             subtitle="Scholarships to Foreign Students by the French Government")+
        geom_label(aes(x=LON, y=LAT), 
                   color="black", fill = "grey", 
                   label=myRegions$Regions, size=2)+
        xlab("") + ylab("") +
        guides(color = FALSE)

 
print(myMap)

# Here is another, more satisfying map.

myMap2 <- ggplot(myRegions) + geom_sf(aes(fill=Regions,color = Regions))+
        scale_fill_brewer(palette="Set1")+
        labs(caption="Number of Scholarships Given by World Region in 2011", 
             title="World Repartition of French Government Scholarships")+
        geom_label(aes(x=LON, y=LAT), 
                   color="black", fill = "grey", 
                   label=myRegions$schol, size=2.2)+
        xlab("") + ylab("")

print(myMap2)
# 
ggsave('myMap2.png', myMap2, width = 32, height = 18, dpi = 100)
ggsave('myMap1.png', myMap, width = 32, height = 18, dpi = 100)
# 
