# Programmers: Shivam Shishangia and Jonathan Chen 
# MIS 545 Section 01
# Lab08Group20ShishangiaChen.R
# Implement Naive Bayes model to predict a person's dwelling type 
# (apartment, condo, or home) based on other demographic data.

# Install the packages
# install.packages("tidyverse")
# install,packages("e1071")

# Load the packages
library("tidyverse")
library("e1071")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab08")

# Load the values from .csv file
dwellingType <- read_csv(file= "DwellingType.csv",
                         col_types = "filll",
                         col_names = TRUE)

# Display the tibble on console
print(dwellingType)

# Display the structure
str(dwellingType)

# Display the summary
summary(dwellingType)

# Set seed to 154
set.seed(154)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(dwellingType),
                    round(nrow(dwellingType) * 0.75),
                    replace = FALSE)

# Split the dataset 
dwellingTypeTraining <- dwellingType[sampleSet, ]
dwellingTypeTesting <- dwellingType[-sampleSet, ]

# Generate the Naive Bayes model
dwellingTypeModel <- naiveBayes(formula = DwellingType ~ .,
                                data = dwellingTypeTraining,
                                laplace = 1)

# Build probabilities for each record in the testing dataset
dwellingTypeProbability <- predict(dwellingTypeModel,
                                   dwellingTypeTesting,
                                   type = "raw")

# Display dwellingTypeProbability on the console
print(dwellingTypeProbability)

# Predict classes for each record in the testing dataset
dwellingTypePrediction <- predict(dwellingTypeModel,
                                  dwellingTypeTesting,
                                  type = "class")

# Display dwellingTypePrediction on the console
print(dwellingTypePrediction)

# Evaluate the model by forming a confusion matrix
dwellingTypeConfusionMatrix <- table(dwellingTypeTesting$DwellingType,
                                     dwellingTypePrediction)

# Display the confusion matrix
print(dwellingTypeConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(dwellingTypeConfusionMatrix)) / 
    nrow(dwellingTypeTesting)

# Display the predictive accuracy
print(predictiveAccuracy)