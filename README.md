<p align="center">
  <img width="450" height="247" src="./LULCLogosmall.png">
</p>


# Day 2 - Visualizing Rasters in R, Extracting Training Samples, Creating a Random Forest Model and Classification

---
### Licensing
Code used in this class was adapted from XYZ and are licensed under XYZ license. Any additional material, presentations, figures and GitHub classroom pages created for this class are licensed under [GPL-3.0 License](./LICENSE.md). 

---

## Today you will... 

  - display raster data in R
  - extract your training samples' spectral signatures using the polygons you created yesterday. 
  - fit a Random Forest model.
  - use your model to classify image features.

## At home you are supposed to...
  - read about the concept of validation data and its purpose.

## Tomorrow you will...
  - create a validation data set in R.
  - perform accuracy assessment and evaluate your classification.
---
  
# Problem 1: Visualizing Raster Data in R
In this task we will display a Landsat-8 scene in R. The ability to plot raster images in R will come handy if we want to visualize the results of our image classification. 

[Link to the PDF assignment](./02_01_VisualizationR.pdf)

[Link to the code](./02_01_Raster_Visualization.R)

# Porblem 2: Extracting your Training Samples' Spectral Signatures 
Next, we will have to prepare our training samples so we can use them to fit our classification model. Yesterday, you have created the training area polygons in QGIS. Today, we will use those polygons to extract the underlying spectral signatures to a dataframe. 

[Link to the PDF assignment](./02_02_PreparingSamples.pdf)

[Link to the code](./02_02_TrainingDataSampling.R)

# Problem 3: Classification and Learning Curve

[Link to the code](./02_03_Classification.R)

# @Home: Validation Data and its Purpose
If you didn't finish the steps above yet, we recommend to wrap them up now. After that, have a look at [this explanation](./02_H_ValidationCreatingSamplesR.pdf) of validation data. You do not have to follow the code instructions (you will do that tomorrow) but should be able to make out the fundamental difference between training, testing and validation data. 

You may use these questions to test your understanding:
- What is the purpose of validation in the land use classification workflow?

>YOUR ANSWER HERE

- Why is it necessary to create an independent validation dataset?

>YOUR ANSWER HERE

- Briefly discern random, stratified and equalized stratified random strategies in your own words.

>YOUR ANSWER HERE

