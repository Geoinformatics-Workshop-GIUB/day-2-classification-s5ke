#Visualization of LS-8 Data -----------------------------------------------------

#Installing required packages, prerequisites, import
#Install.packages("raster")
library(raster)

#Working directory
setwd("~/Studium_nicht_Sciebo/RSRG/Teaching_material/Data")

img <- brick("LC081960252020091901RT-SC20200925100850.tif")
img

#Plot
plotRGB(img,
        r = 3, g = 3, b = 3,
        stretch = "lin"
)

plotRGB(img,
        r = 4, g = 3, b = 2,
        stretch = "lin",
        ext = extent(362699.321, 369890.701, 5619755.014, 5624836.560)
)

#Save subset
img.subset_vis <- crop(img, extent(362699.321, 369890.701, 5619755.014, 5624836.560))

writeRaster(img.subset_vis,
            filename = "C081960252020091901RT-SC20200925100850_subset_vis.tif",
            format = "GTiff",
            overwrite = TRUE
)

#Plot histogram/distribution
green <- img.subset_vis[[3]]

hist(green,
     breaks = 200,
     xlim = c(0, 1500),
     ylim = c(0, 15000),
     xlab = "band 3 reflectance value [DN * 0.01]",
     ylab = "frequency",
     main = "histogram L8 band 3 (green)"
)
