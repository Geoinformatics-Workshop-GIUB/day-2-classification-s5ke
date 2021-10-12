#Classification: Random Forest (RF) ---------------------------------------------


#Install additional packages
#install.packages("randomForest")
library(randomForest)

#Working directory
setwd("~/R-Kurs_WiSe2021-2022_Spatial_Analysis/Data")

getwd()

dir()

#Load training sample dataframe from your wd
load(file = "smp.rda")
#Access training samples
summary(smp$cl)

#Down-Sampling via minority class 
smp.size <- rep(min(summary(smp$cl)), nlevels(smp$cl))
smp.size

help(tuneRF)
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

#Classification: Learning Curve -------------------------------------------------

#Import LS8 subset
img <- brick("LS8_Bonn_SC20200925100850.tif")

#Import shapefile containing training data
shp <- shapefile("training_data.shp")

#Load dataframe from your wd, already done for classification
#load(file = "smp.rda")

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

