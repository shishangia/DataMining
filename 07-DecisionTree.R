# Programmers: Shivam Shishangia and Gurleen Kohli 
# MIS 545 Section 01
# Lab09Group05ShishangiaKohli.R
# Using decision tree model to predict farm ownership for Indonesian Rice Farms
# based on Hectares, SeedPrice, HiredLabor, FamilyLabor, Average Wage 
# and Region

# Install the packages
# install.packages("tidyverse")
# install.packages("rpart.plot")

# Load the packages
library("tidyverse")
library("rpart")
library("rpart.plot")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab09")

# Load the values from .csv file
riceFarms <- read_csv(file= "IndonesianRiceFarms.csv",
                      col_types = "fniiinf",
                      col_names = TRUE)

# Display the tibble on console
print(riceFarms)

# Display the structure
str(riceFarms)

# Display the summary
summary(riceFarms)

# Set seed to 370
set.seed(370)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(riceFarms),
                    round(nrow(riceFarms) * 0.75),
                    replace = FALSE)

# Split the dataset 
riceFarmsTraining <- riceFarms[sampleSet, ]
riceFarmsTesting <- riceFarms[-sampleSet, ]

# Generate the Decision tree model
riceFarmDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                   method = "class",
                                   data = riceFarmsTraining)

# Display the decision tree plot
rpart.plot(riceFarmDecisionTreeModel)

# Predict classes for each record in the testing dataset
riceFarmsPrediction <- predict(riceFarmDecisionTreeModel,
                               riceFarmsTesting,
                               type = "class")

# Display riceFarmsPrediction on the console
print(riceFarmsPrediction)

# Evaluate the model by forming a confusion matrix
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Display the confusion matrix
print(riceFarmsConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) / 
    nrow(riceFarmsTesting)

# Display the predictive accuracy
print(predictiveAccuracy)

# Create a new decision tree model using 0.007 as the complexity parameter
riceFarmDecisionTreeModel <- rpart(formula = FarmOwnership ~ .,
                                   method = "class",
                                   cp = 0.007,
                                   data = riceFarmsTraining)

# Display the decision tree plot
rpart.plot(riceFarmDecisionTreeModel)

# Predict classes for each record in the testing dataset
riceFarmsPrediction <- predict(riceFarmDecisionTreeModel,
                               riceFarmsTesting,
                               type = "class")

# Display riceFarmsPrediction on the console
print(riceFarmsPrediction)

# Evaluate the model by forming a confusion matrix
riceFarmsConfusionMatrix <- table(riceFarmsTesting$FarmOwnership,
                                  riceFarmsPrediction)

# Display the confusion matrix
print(riceFarmsConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(riceFarmsConfusionMatrix)) / 
    nrow(riceFarmsTesting)

# Display the predictive accuracy
print(predictiveAccuracy)