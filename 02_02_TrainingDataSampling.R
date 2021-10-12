#Classification: Preparing Samples ----------------------------------------------


#Installing required packages, prerequisites, import
#Install.packages("raster")
library(raster)

#Working directory
setwd("D:/R-Kurs_WiSe2122/R-Kurs_WiSe2021-2022_Spatial_Analysis/Data")

getwd()

dir()

#Import LS8 subset
img <- brick("D:/R-Kurs_WiSe2122/R-Kurs_WiSe2021-2022_Spatial_Analysis/Data/Landsat_8_all/LC081960252020091901RT-SC20200925100850.tif")
img

#Import shapefile containing training data
shp <- shapefile("training_data.shp")
shp

#Check CRS
compareCRS(img,shp)

#Plot data
plotRGB(img, r = 4, g = 3, b = 2, stretch = "lin")
plot(shp, col="red", add=TRUE)

#Conversion of class-characters
levels(as.factor(shp$class))

for (i in 1:length(unique(shp$class))) {cat(paste0(i, " ", levels(as.factor(shp$class))[i]), sep="\n")}

#Rename bands of LS image
names(img)
names(img) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")
names(img)

#Create dataframe
smp <- extract(img, shp, df = TRUE)


#Matching ID of smp and class of shp to new column "cl", delete "ID"-column
smp$cl <- as.factor(shp$class[match(smp$ID, seq(nrow(shp)))])
smp <- smp[-1]

#Save dataframe to your wd
save(smp, file = "smp.rda")

#Load dataframe from your wd
load(file = "smp.rda")

#Check out the summary of the class-column smp
summary(smp$cl)

#Aggregate cl-column 
sp <- aggregate( . ~ cl, data = smp, FUN = mean, na.rm = TRUE )

#Plot empty plot of a defined size
plot(0,
     ylim = c(min(sp[2:ncol(sp)]), max(sp[2:ncol(sp)])), 
     xlim = c(1, ncol(smp)-1), 
     type = 'n', 
     xlab = "L8 bands", 
     ylab = "reflectance [% * 100]"
)

#Define colors for class representation - one color per class necessary!
mycolors <- c("#fbf793", "#006601", "#bfe578", "#d00000", "#6569ff")

#Draw one line for each class
for (i in 1:nrow(sp)){
  lines(as.numeric(sp[i, -1]), 
        lwd = 4, 
        col = mycolors[i]
  )
}

#Add a grid
grid()

#Add a legend
legend(as.character(sp$cl),
       x = "topleft",
       col = mycolors,
       lwd = 5,
       bty = "n"
)
