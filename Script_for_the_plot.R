

packages <- c("R.utils", "here", "data.table", "readr", "ggplot2", "dplyr", "tidyr")

for (package in packages) {
        if (!require(package, character.only=T, quietly=T)) {
                install.packages(package)}
        library(package, character.only=T)
}

print ("-------------------> If error by first launch, launch it again: packages you need might take long to install, throwing a temporary error.")


#### DATA DWNLD #### 

here()
WD <- getwd()
if (!is.null(WD)) setwd(WD)


# You might want to uncomment the next three lines in order to avoid downloading again and again.

dataUrl <- 'http://www.data.gouv.fr/fr/datasets/r/dd3b4807-d90c-46fe-b03a-2ad250db979f'

dir.create("originalData", showWarnings = F)

# Comment / Uncomment next line (uncomment if program run for 1st time / dataset not yet downloaded)

# download.file(dataUrl, "./originalData/myData.csv")



globalData <- read.table("originalData/myData.csv", sep=";")

# Some data exploring gives a view of what has to be done to tidy the set. 
# As the dataset is short, a View(globalData) is any case a good option for a start.

# View(globalData)

#### DATA TIDYING ####

tidyData <- globalData[4:15,2:7] %>% 
        apply(2, as.character) %>% 
        sub(" ", "", .) %>% 
        apply(2, as.numeric) %>%
        as.data.frame(.)


#Putting years as rows after inverting rows and columns with the t transpose function

Years <- as.factor(as.character(c(2006:2011)))
Regions <- c("Years", "UE", "Eur_hors_UE", "Europe", "Am_N", "America", "Asia/Oceania", "Middle East", "AfSubS_F", "AfSubS_nF","Sub-Saharan Africa","Maghreb", "Total_Ge")

tidySet <- as.data.frame(t(tidyData)) %>% 
        cbind(Years, .)

colnames(tidySet) <- Regions
rownames(tidySet) <- Years


tidySet <- tidySet %>% 
        gather(Years, count) 

colnames(tidySet) <- c("Years", "Region", "Total")

tidySet <- tidySet %>%
        filter(Region %in% c("Europe", "America", 
                             "Asia/Oceania", "Middle East",
                             "Sub-Saharan Africa", "Maghreb"))

#### DATA VIZ ####


ScholPlot <- ggplot(data=tidySet) + geom_point(aes(x=Years, y=Total, color = Region, size= 10)) +
        geom_line(aes(x=Years, y=Total, group = Region)) + guides(size=FALSE)+ 
        ylab("Number of Scholarships")

# print(ScholPlot)


#### integrating legend to plot with annotations####

# Retrieving colors through the console
g <- ggplot_build(ScholPlot)
h <-unique(g$data[[1]]["colour"])
print(h)

#Matching colors with their regions

ScholPlot2 <- ScholPlot + annotate("text", label="Europe", color = "#00BA38", x=2, y = 3450)+
        annotate("text", label="America", color = "#F8766D", x=2, y = 1550)+
        annotate("text", label="Asia/OCeania", color = "#B79F00", x=2, y = 2850)+
        annotate("text", label="Middle East", color = "#619CFF", x=2, y = 2180)+
        annotate("text", label="Maghreb", color = "#F564E3", 1.5, y = 3820)+
        annotate("text", label="Sub-Saharan Africa", color = "#00BFC4", x=4, y = 3600)+
        theme(legend.position = "none")
        

print(ScholPlot2)

ggsave('ScholPlot.png', width = 16, height = 9, dpi = 100) 
ggsave('ScholPlot2.png', width = 16, height = 9, dpi = 100) 