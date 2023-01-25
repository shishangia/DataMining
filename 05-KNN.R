# Programmers: Shivam Shishangia and Abhinav
# MIS 545 Section 01
# Lab07Group01ShishangiaAbhinav.R
# Perform KNN classification to determine the size of the sedan based on the
# price, roadTest and reliability.

# Install the packages
# install.packages("tidyverse")
# install.packages("class")

# Load the packages
library("tidyverse")
library("class")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab07")

# Load the values from .csv file
sedanSize <- read_csv(file= "SedanSize.csv",
                      col_types = "cfnii",
                      col_names = TRUE)

# Display the tibble on console
print(sedanSize)

# Display the structure
str(sedanSize)

# Display the summary
summary(sedanSize)

# Remove MakeModel from the tibble
sedanSize <- sedanSize %>% select(-MakeModel)

# Separate the tibble in two, one with values and other with just the labels
sedanSizeLabels <- sedanSize %>% select(SedanSize)
sedanSize <- sedanSize %>% select(-SedanSize)

# Creating the displayAllHistograms() function
displayAllHistograms <- function(tibbleDataset) {
    tibbleDataset %>%
        keep(is.numeric) %>%
        gather() %>%
        ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                                  color="black") + 
        facet_wrap(~ key, scales="free") +
        theme_minimal()
}

# Call the displayAllHistograms() function by passing the data tibble
displayAllHistograms(sedanSize)

# Set seed to 517
set.seed(517)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(sedanSize),
                    round(nrow(sedanSize) * 0.75),
                    replace = FALSE)

# Split the dataset 
sedanSizeTraining <- sedanSize[sampleSet, ]
sedanSizeLabelsTraining <- sedanSizeLabels[sampleSet, ]

sedanSizeTesting <- sedanSize[-sampleSet, ]
sedanSizeLabelsTesting <- sedanSizeLabels[-sampleSet, ]

# Create the k-Nearest Neighbors model
sedanSizePrediction <- knn(train = sedanSizeTraining,
                           test = sedanSizeTesting,
                           cl = sedanSizeLabelsTraining$SedanSize,
                           k = 7)

# Display the predictions of the model
print(sedanSizePrediction)

# Display the summary of the model
summary(sedanSizePrediction)

# Evaluate the model by forming a confusion matrix
sedanSizeConfusionMatrix <- table(sedanSizeLabelsTesting$SedanSize,
                                  sedanSizePrediction)

# Display the confusion matrix
print(sedanSizeConfusionMatrix)

# Calculate the model predictive accuracy
predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix)) / 
    nrow(sedanSizeTesting)

# Display the predictive accuracy
print(predictiveAccuracy)

# Create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names of "k value" and "Predictive accuracy" to kValueMatrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Loop through odd values of k from 1 up to the number of records in the 
# training dataset. With each pass through the loop, store the k-value along 
# with its predictive accuracy.
for (kValue in 1:nrow(sedanSizeLabelsTraining)) {
    # Only calculate model accuracy for odd k-value
    if (kValue %% 2 == 0) {
        next
    }

    # Generate the model
    sedanSizePrediction <- knn(train = sedanSizeTraining,
                               test = sedanSizeTesting,
                               cl = sedanSizeLabelsTraining$SedanSize,
                               k = kValue)

    # Evaluate the model by forming a confusion matrix
    sedanSizeConfusionMatrix <- table(sedanSizeLabelsTesting$SedanSize,
                                      sedanSizePrediction)

    # Calculate the model predictive accuracy
    predictiveAccuracy <- sum(diag(sedanSizeConfusionMatrix)) / 
        nrow(sedanSizeTesting)

    # Add a new row to the matrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, predictiveAccuracy))
}

# Display the kValueMatrix on the console
print(kValueMatrix)