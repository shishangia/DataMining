# Programmers: Shivam Shishangia and Michael Baluyot
# MIS 545 Section 01
# Lab12Group21ShishangiaPBaluyot.R
# TODO: Add description.

# Install the packages
# install.packages("tidyverse")
# install.packages("neuralnet")

# Load the packages
library("tidyverse")
library("neuralnet")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab12")

# Load the values from .csv file
fishingCharter <- read_csv(file = "FishingCharter.csv",
                           col_types = "lnn",
                           col_names = TRUE)

# Display the tibble on console
print(fishingCharter)

# Display the structure
str(fishingCharter)

# Display the summary
summary(fishingCharter)

# Scale the AnnualIncome feature from 0 to 1
fishingCharter <- fishingCharter %>%
    mutate(AnnualIncomeScaled = (AnnualIncome - min(AnnualIncome))/
               (max(AnnualIncome) - min(AnnualIncome)))

# Scale the CatchRate feature from 0 to 1
fishingCharter <- fishingCharter %>%
    mutate(CatchRateScaled = (CatchRate - min(CatchRate))/
               (max(CatchRate) - min(CatchRate)))

# Set the random seed to 591
set.seed(591)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(fishingCharter),
                    round(nrow(fishingCharter) * 0.75),
                    replace = FALSE)

# Split the dataset 
fishingCharterTraining <- fishingCharter[sampleSet, ]
fishingCharterTesting <- fishingCharter[-sampleSet, ]

# Generate the neural network model
fishingCharterNeuralNet <- neuralnet(
    formula = CharteredBoat ~ CatchRateScaled + AnnualIncomeScaled,
    data = fishingCharterTraining,
    hidden = 3,
    act.fct = "logistic",
    linear.output = FALSE)

# Display the neural network numeric results
print(fishingCharterNeuralNet$result.matrix)

# Visualize the neural network
plot(fishingCharterNeuralNet)

# Generate probabilities on fishingCharterTesting
fishingCharterProbability <- compute(fishingCharterNeuralNet,
                                     fishingCharterTesting)

# Display the probabilities from the testing dataset on the console
print(fishingCharterProbability$net.result)

# Convert probability predictions into 0/1 predictions
fishingCharterPrediction <- 
    ifelse(fishingCharterProbability$net.result > 0.5, 1, 0)

# Display the 0/1 predictions on the console
print(fishingCharterPrediction)

# Evaluate the model by forming a confusion matrix
fishingCharterConfusionMatrix <- table(fishingCharterTesting$CharteredBoat,
                                       fishingCharterPrediction)

# Display the confusion matrix
print(fishingCharterConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(fishingCharterConfusionMatrix)) / 
    nrow(fishingCharterTesting)

# Display the predictive accuracy
print(predictiveAccuracy)