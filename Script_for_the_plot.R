

packages <- c("R.utils", "here", "data.table", "readr", "ggplot2", "dplyr")

for (package in packages) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package)}
        library(package, character.only=T)
}






## (ggplot) + annotate("text",label= "Lost 8 points because of bad luck!", x= "Arsenal", y=-3) +

#### DATA DWNLD #### 

here()
WD <- getwd()
if (!is.null(WD)) setwd(WD)


# You might want to uncomment the next three lines in order to avoid downloading again and again.



dataUrl <- 'http://www.data.gouv.fr/fr/datasets/r/dd3b4807-d90c-46fe-b03a-2ad250db979f'

dir.create("originalData", showWarnings = F)
download.file(dataUrl, "./originalData/myData.csv")

globalData <- read.table("originalData/myData.csv", sep=";")


#### DATA TIDYING ####

tidyData <- globalData[4:15,2:7] %>% 
        apply(2, as.character) %>% 
        sub(" ", "", .) %>% 
        apply(2, as.numeric) %>%
        as.data.frame(.)


#Putting years as rows (inverting rows and columns)
Years <- as.factor(as.character(c(2006:2011)))

finalTidy <- as.data.frame(t(tidyData)) %>% 
        cbind(Years, .)

Regions2 <- as.character(globalData[4:15,1])
Regions <- c("Years", "UE", "Eur_hors_UE", "Total_Eur", "Am_N", "Total_Am", "Total_Asie_O", "MO", "AfSubS_F", "AfSubS_nF","Total_AfSubS", "Maghreb", "Total_Ge")
colnames(finalTidy) <- Regions
rownames(finalTidy) <- Years


##
data_Eur <- cbind(finalTidy[,c(1,4)], Region=rep("Europe", 6))
data_Am <- cbind(finalTidy[,c(1,6)], Region=rep("America", 6))
data_Asia <- cbind(finalTidy[,c(1,7)], Region=rep("Asia/OCeania", 6))
data_ME <- cbind(finalTidy[,c(1,8)], Region=rep("Middle East", 6))
data_Afr <- cbind(finalTidy[,c(1,11)], Region=rep("Sub-Saharan Africa", 6))
data_Magh <- cbind(finalTidy[,c(1,12)], Region=rep("Maghreb", 6))


names(data_Eur)<- c("Years", "Total", "Region")
names(data_Am)<- c("Years", "Total", "Region")
names(data_Asia)<- c("Years", "Total", "Region")
names(data_ME)<- c("Years", "Total", "Region")
names(data_Afr)<- c("Years", "Total", "Region")
names(data_Magh)<- c("Years", "Total", "Region")

long_Data <- rbind(data_Afr, data_Am, data_Asia, data_Eur, data_Magh, data_ME)

#### DATA VIZ ####

ScholPlot <- ggplot(data=long_Data) + geom_point(aes(x=Years, y=Total, color = Region, size= 10)) +
        geom_line(aes(x=Years, y=Total, group = Region)) + guides(size=FALSE)
print(ScholPlot)

ggsave('ScholPlot.png', width = 16, height = 9, dpi = 100)