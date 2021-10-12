### RESEDA Tutorial: Visualization, Classification, Regression and Validation

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


#Classification: Preparing Samples ----------------------------------------------


#Installing required packages, prerequisites, import
#Install.packages("raster")
library(raster)

#Working directory
setwd("~/Studium_nicht_Sciebo/RSRG/Teaching_material/Data")

getwd()

dir()

#Import LS8 subset
img <- brick("LS8_Bonn_SC20200925100850.tif")
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


#Classification: Random Forest (RF) ---------------------------------------------

#Install additional packages
#install.packages("randomForest")
library(randomForest)

#Access training samples
summary(smp$cl)

#Down-Sampling via minority class 
smp.size <- rep(min(summary(smp$cl)), nlevels(smp$cl))
smp.size

rfmodel <- tuneRF(x = smp[-ncol(smp)],
                  y = smp$cl,
                  sampsize = smp.size,
                  strata = smp$cl,
                  ntree = 250,
                  importance = TRUE,
                  doBest = TRUE,
                  plot = TRUE
)

#Print useful information about our model
rfmodel

#Access/Plot importance variables
varImpPlot(rfmodel)

#Plot rfmodel
plot(rfmodel, col = c("#000000", "#fbf793", "#006601", "#bfe578", "#d00000", "#6569ff"), lwd = 3)

#Save model-file
save(rfmodel, file = "rfmodel.RData")

#Load model-file
load("rfmodel.RData")

#Predict all pixels/run classification
result <- predict(img,
                  rfmodel,
                  filename = "RF_classification.tif",
                  overwrite = TRUE
)

#Plot Classification
plot(result, 
     axes = FALSE, 
     box = FALSE,
     col = mycolors
     )


#Classification: Support Vector Machine (SVM) -----------------------------------

#Install required packages
#install.packages("e1071")
library(e1071)

#Load dataframe from your wd
load(file = "smp.rda")

#Get distribution of available training samples
summary(smp$cl)

#Print head of smp
head(smp)

#Shuffle/Sample all rows of smp
smp <- smp[sample(nrow(smp)),]

#Print head of smp
head(smp)

#Get distribution of available training samples
summary(smp$cl)

#Get min value to determine max sample-size
smp.maxsamplesize <- min(summary(smp$cl))
smp.maxsamplesize

#Select each class in the same size via smp.maxsamplesize
smp <- smp[ave(1:(nrow(smp)), smp$cl, FUN = seq) <= smp.maxsamplesize, ]

#Get distribution of available training samples
summary(smp$cl)

#Find hyperparameters
gammas = 2^(-8:5)
gammas

costs = 2^(-5:8)
costs

#Run SVM-Classification
svmgs <- tune(svm,
              train.x = smp[-ncol(smp)],
              train.y = smp$cl,
              type = "C-classification",
              kernel = "radial", 
              scale = TRUE,
              ranges = list(gamma = gammas, cost = costs),
              tunecontrol = tune.control(cross = 5)
)

#Check output
svmgs

#Plot gridsearch
plot(svmgs)

#Extract best model
svmmodel <- svmgs$best.model
svmmodel

#Save/load file
save(svmmodel, file = "svmmodel.RData")
#load("svmmodel.RData")

#Predict entire dataset
result <- predict(img,
                  svmmodel,
                  filename = "SVM_classification.tif",
                  overwrite = TRUE
)

#Visualize data in a plot
plot(result, 
     axes = FALSE, 
     box = FALSE,
     col = mycolors
     )

#Classification: Learning Curve -------------------------------------------------

#Import LS8 subset
img <- brick("LS8_Bonn_SC20200925100850.tif")

#Import shapefile containing training data
shp <- shapefile("training_data.shp")

#Load dataframe from your wd
load(file = "smp.rda")

#Number of samples per class chosen for each run
steps <- 2^(1:11) #2, 4, 8, 16, 32...

#Number of repetitions of each run
nrepeat = 20

#Create empty matrix for final results
r <- matrix(0, 5, length(steps))

for (i in 1:length(steps)) {
  #Create empty vector for OOB error from each repetition
  rtmp <- rep(0, nrepeat)
  cat(paste0("Step: ", i, " - "))
  for (j in 1:nrepeat) {
    #Shuffle all samples and subset according to size defined in "steps"
    sub <- smp[sample(nrow(smp)),]
    sub <- sub[ave(1:(nrow(sub)), sub$cl, FUN = seq) <= steps[i], ] 
    #RF classify as usual
    sub.size <- rep(min(summary(sub$cl)), nlevels(sub$cl))
    rfmodel <- randomForest(x = sub[-ncol(sub)],
                            y = sub$cl,
                            sampsize = sub.size,
                            strata = sub$cl,
                            ntree = 500)
    #Extract OOB error rate of last tree (the longest trained) and save to rtmp
    ooberrors <- rfmodel$err.rate[ , 1]
    rtmp[j] <- ooberrors[length(ooberrors)]
    cat(j, " ")
  }  #Use repetitions to calc statistics (mean, min, max, CI) & save it to final results matrix
  ttest <- t.test(rtmp, conf.level = 0.95)
  r[ , i] <- c(mean(rtmp), ttest$conf.int[2], ttest$conf.int[1], max(rtmp), min(rtmp))
  cat("\n")
}

#Conversion in percent
r <- r * 100

#Plot empty plot without x-axis
plot(x = 1:length(steps),
     y = r[1,],
     ylim = c(min(r), max(r)),
     type = "n",
     xaxt = "n",
     xlab = "number of samples per class",
     ylab = "OOB error [%]"
)

#Complete the x-axis 
axis(1, at=1:length(steps), labels=steps)

#Add a grid
grid()

#Draw min-max range of OOB errors
polygon(c(1:length(steps), rev(1:length(steps))), 
        c(r[5, ], rev(r[4, ])),
        col = "grey80", 
        border = FALSE
)

#Draw confidence interval 95%
polygon(c(1:length(steps), rev(1:length(steps))), 
        c(r[3, ], rev(r[2, ])),
        col = "grey60", 
        border = FALSE
)

#Draw line of mean OOB 
lines(1:length(steps), r[1, ], lwd = 3)

#Add a legend
legend("topright",
       c("mean", "t-test CI 95%", "min-max range"),
       col = c("black", "grey80", "grey60"),
       lwd = 3,
       bty = "n"
)

#Regression: Preparing Samples --------------------------------------------------

#Add required libraries
#install.packages("rgeos")
library(rgeos)
library(raster)
library(randomForest)

#Import LS8 subset
img <- brick("LS8_Bonn_SC20200925100850.tif")

#Import digitized polygon areas
shp <- shapefile("reg_train_data_Bonn.shp")

#Check files
shp
img
compareCRS(img, shp)

#Plot shp
plot(shp)

plot(img[[4]], col=gray.colors(100))
plot(shp, add=TRUE)

#Rename img-bands
names(img) <- c("b1", "b2", "b3", "b4", "b5", "b6", "b7")

#Create buffer with no width to avoid topology problems
shp <- gBuffer(shp, byid=TRUE, width=0)

#Crop img to extent of shp
img.subset <- crop(img, shp)

#Mask/Generate a raster of shp and img 
img.mask <- rasterize(shp, img.subset, getCover = TRUE)

#Set pixels with less coverage to NA / Extract pixels that are 100% covered by the polygons
#img.mask[img.mask < 100] <- NA

#Mask result
img.subset <- mask(img.subset, img.mask)

#Plot shp and img.subset
plot(img.subset[[4]])
plot(shp, add=TRUE)

#Transform raster to Polygons
grid <- rasterToPolygons(img.subset)

#Assign ID to each of these cells
grid$ID <- seq.int(nrow(grid))

#Access and plot grid
grid

#Create dataframe and iterate over entire grid
smp <- data.frame()
for (i in 1:length(grid)) {
  cell <- intersect(grid[i, ], shp)
  cell <- cell[cell$fclass == "park", ]
  if (length(cell) > 0) {
    areaPercent <- sum(area(cell) / area(grid)[1])
  } else {
    areaPercent <- 0
  }
  newsmp <- cbind(grid@data[i, 1:nlayers(img)], areaPercent)
  smp <- rbind(smp, newsmp)
}

(smp)

#Regression: SVM Regression -----------------------------------------------------

#Add libraries
library(e1071)
library(rgeos)
library(raster)

#Set hyperparameters
gammas = 2^(-8:3)
costs = 2^(-5:8)
epsilons = c(0.1, 0.01, 0.001)

#SVM
svmgs <- tune(svm,
              train.x = smp[-ncol(smp)],
              train.y = smp[ncol(smp)],
              type = "eps-regression",
              kernel = "radial", 
              scale = TRUE,
              ranges = list(gamma = gammas, cost = costs, epsolon = epsilons),
              tunecontrol = tune.control(cross = 5)
)

#Check paramters of svmgs
svmgs

#Use best parameters
svrmodel <- svmgs$best.model
svrmodel

#Save result
save(svrmodel, file = "svrmodel.RData")

load("svrmodel.RData")

#Predict all pixels
result <- predict(img, svrmodel)
result

#Equalize values <0 and >1
result[result > 1] = 1
result[result < 0] = 0

#Save result as tif-file
writeRaster(result, filename="regression.tif")

#Plot regression
plot(result, col=gray.colors(100))

#Validation: Creating Samples ---------------------------------------------------

#Import required packages
#library(raster)

#Import RF-Classification
img.classified <- raster("RF_classification.tif")

smp.test <- sampleStratified(x = img.classified,
                             size = 50,
                             na.rm = TRUE,
                             sp = TRUE)

#Check out result
smp.test$RF_classification

#Sample smp.test
smp.test <- smp.test[sample(nrow(smp.test)), ]

#Check out resampled smp.test-df
smp.test$RF_classification

#Delete variables (cell, RF_classification) of smp.test and replace them by "ID"
smp.test <- smp.test[, -c(1, 2)]
smp.test$ID <- 1:nrow(smp.test)

#Check out attributes of smp.test
smp.test

#Plot distribution of sampling on top of classification map
plot(img.classified, 
     axes = FALSE, 
     box = FALSE,
     col = mycolors
)
points(smp.test)

#Save smp.test as df 
shapefile(smp.test,
          filename = "RF_validation.shp",
          overwrite = TRUE
)

#Validation: Accuracy Statistics ------------------------------------------------

#Import files
img.classified <- raster("RF_classification.tif")
shp.train <- shapefile("training_data.shp")
shp.valid <- shapefile("RF_validation.shp")

#Access validclass-column of shp.valid, transfer it to factors
reference <- as.factor(shp.valid$validclass)
reference

#Access shp.valid of RF-classification, transfer it to factors
predicted <- as.factor(extract(img.classified, shp.valid))
predicted

#Generate table of predicted and reference
accmat <- table("pred" = predicted, "ref" = reference)
accmat

#Generate user's accuracy
UA <- diag(accmat) / rowSums(accmat) * 100
UA

#Generate producer's accuracy
PA <- diag(accmat) / colSums(accmat) * 100
PA

#Generate overall accuracy
OA <- sum(diag(accmat)) / sum(accmat) * 100
OA

#Generate nicely looking matrix
accmat.ext <- addmargins(accmat)
accmat.ext <- rbind(accmat.ext, "Users" = c(PA, NA))
accmat.ext <- cbind(accmat.ext, "Producers" = c(UA, NA, OA))
colnames(accmat.ext) <- c(levels(as.factor(shp.train$class)), "Sum", "PA")
rownames(accmat.ext) <- c(levels(as.factor(shp.train$class)), "Sum", "UA")
accmat.ext <- round(accmat.ext, digits = 1)
dimnames(accmat.ext) <- list("Prediction" = colnames(accmat.ext),
                             "Reference" = rownames(accmat.ext))
class(accmat.ext) <- "table"
accmat.ext

#Validation: Significance Test --------------------------------------------------

sign <- binom.test(x = sum(diag(accmat)),
                   n = sum(accmat),
                   alternative = c("two.sided"),
                   conf.level = 0.95
)

pvalue <- sign$p.value
pvalue

CI95 <- sign$conf.int[1:2]
CI95

#Validation: Kappa-Coefficient --------------------------------------------------

#Write Kappa-Coefficient function
kappa <- function(m) {
  N <- sum(m)
  No <- sum(diag(m))
  Ne <- 1 / N * sum(colSums(m) * rowSums(m))
  return( (No - Ne) / (N - Ne) )
}

#Use accmat as arguments for kappa
kappa(accmat)

#Validation: Area Adjusted Accuracies -------------------------------------------
#According to Olofsson et al. 2014

#Import files
img.classified <- raster("RF_classification.tif")
shp.train <- shapefile("training_data.shp")
shp.valid <- shapefile("RF_validation.shp")

#Create regular accuracy matrix 
confmat <- table(as.factor(extract(img.classified, shp.valid)), as.factor(shp.valid$validclass))

#Get number of pixels per class and convert in km²
imgVal <- as.factor(getValues(img.classified))
nclass <- length(unique(shp.train$class))
maparea <- sapply(1:nclass, function(x) sum(imgVal == x))
maparea
maparea <- maparea * res(img.classified)[1] ^ 2 / 1000000
maparea

#Set confidence interval
conf <- 1.96

#Total map area
A <- sum(maparea)

#Proportion of area mapped as class i
W_i <- maparea / A

#Number of reference points per class
n_i <- rowSums(confmat)

#Population error matrix 
p <- W_i * confmat / n_i
p[is.na(p)] <- 0
round(p, digits = 4)
p

#Area estimation
p_area <- colSums(p) * A

#Area estimation confidence interval 
p_area_CI <- conf * A * sqrt(colSums((W_i * p - p ^ 2) / (n_i - 1)))

#Overall accuracy (Eq.1)
OA <- sum(diag(p))

#Producers accuracy (Eq.3)
PA <- diag(p) / colSums(p)

#Users accuracy (Eq.2)
UA <- diag(p) / rowSums(p)

#Overall accuracy confidence interval (Eq.5)
OA_CI <- conf * sqrt(sum(W_i ^ 2 * UA * (1 - UA) / (n_i - 1)))

#User accuracy confidence interval (Eq.6)
UA_CI <- conf * sqrt(UA * (1 - UA) / (n_i - 1))

#Producer accuracy confidence interval (Eq.7)
N_j <- sapply(1:nclass, function(x) sum(maparea / n_i * confmat[ , x]) )
tmp <- sapply(1:nclass, function(x) sum(maparea[-x] ^ 2 * confmat[-x, x] / n_i[-x] * ( 1 - confmat[-x, x] / n_i[-x]) / (n_i[-x] - 1)) )
PA_CI <- conf * sqrt(1 / N_j ^ 2 * (maparea ^ 2 * ( 1 - PA ) ^ 2 * UA * (1 - UA) / (n_i - 1) + PA ^ 2 * tmp))

#Gather results
result <- matrix(c(p_area, p_area_CI, PA * 100, PA_CI * 100, UA * 100, UA_CI * 100, c(OA * 100, rep(NA, nclass-1)), c(OA_CI * 100, rep(NA, nclass-1))), nrow = nclass)
result <- round(result, digits = 2) 
rownames(result) <- levels(as.factor(shp.train$class))
colnames(result) <- c("km²", "km²±", "PA", "PA±", "UA", "UA±", "OA", "OA±")
class(result) <- "table"
result