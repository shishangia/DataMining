# Programmers: Shivam Shishangia and Harini Sathish Kumar
# MIS 545 Section 01
# Lab06Group13ShishangiaSathishKumar.R
# Predict if a customer of a mobile phone plan will cancel their contract
# based on multiple factors using multiple logistic regression model.

# Install the packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")

# Load the packages
library("tidyverse")
library("corrplot")
library("olsrr")
library("smotefamily")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab06")

# Load the values from .csv file
mobilePhone <- read_csv(file= "MobilePhoneSubscribers.csv",
                        col_types = "lillnininn",
                        col_names = TRUE)

# Display the tibble on console
print(mobilePhone)

# Display the structure
str(mobilePhone)

# Display the summary
summary(mobilePhone)

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
displayAllHistograms(mobilePhone)

# Display a correlation matrix of mobilePhone rounded to two decimal places
round(cor(mobilePhone),2)

# Display a correlation plot and limit output to the bottom left
corrplot(cor(mobilePhone),
         method = "number",
         type = "lower")

# Delete data plan and data usage columns
mobilePhone1 <- mobilePhone %>%
    select(-c(DataUsage, DataPlan))

# Set seed to 203
set.seed(203)

# Create a vector of 75% randomly sampled rows from the original dataset
sampleSet <- sample(nrow(mobilePhone1),
                    round(nrow(mobilePhone1) * 0.75),
                    replace = FALSE)

# Split the dataset into mobilePhoneTraining and mobilePhoneTesting 
mobilePhoneTraining <- mobilePhone1[sampleSet, ]

mobilePhoneTesting <- mobilePhone1[-sampleSet, ]

# Check if there is a class imbalance
summary(mobilePhoneTraining$CancelledService)

# Deal with class imbalance
mobilePhoneTrainingSmoted <- 
    tibble(SMOTE(X = data.frame(mobilePhoneTraining),
                 target = mobilePhoneTraining$CancelledService,
                 dup_size = 3)$data)

# Convert CancelledService and RecentRenewal back into logical types
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
    mutate(CancelledService = as.logical(CancelledService),
           RecentRenewal = as.logical(RecentRenewal))

# Get rid of the "class" column in the tibble (added by SMOTE)
mobilePhoneTrainingSmoted <- mobilePhoneTrainingSmoted %>%
    select(-class)

# Check for class imbalance on the smoted dataset
summary(mobilePhoneTrainingSmoted)

# Generate the logistic regression model
mobilePhoneModel <- glm(data = mobilePhoneTrainingSmoted,
                        family = binomial,
                        formula = CancelledService ~ .)

# Display the summary of the logistic regression model
summary(mobilePhoneModel)

# Calculating odds ratio for each of the 7 independent variables
exp(coef(mobilePhoneModel)["AccountWeeks"])
exp(coef(mobilePhoneModel)["RecentRenewalTRUE"])
exp(coef(mobilePhoneModel)["CustServCalls"])
exp(coef(mobilePhoneModel)["AvgCallMinsPerMonth"])
exp(coef(mobilePhoneModel)["AvgCallsPerMonth"])
exp(coef(mobilePhoneModel)["MonthlyBill"])
exp(coef(mobilePhoneModel)["OverageFee"])

# Use the model to predict the outcomes of the testing dataset
mobilePhonePrediction <- predict(mobilePhoneModel,
                                 mobilePhoneTesting,
                                 type = "response")

# Display mobilePhonePrediction on console
print(mobilePhonePrediction)

# Treat anything above 0.5 as 1 and below or equal to 0.5 as 0
mobilePhonePrediction <- ifelse(mobilePhonePrediction > 0.5, 1, 0)

# Display mobilePhonePrediction on console
print(mobilePhonePrediction)

# Create confusion matrix
mobilePhoneConfusionMatrix <- table(mobilePhoneTesting$CancelledService,
                                    mobilePhonePrediction)

# Display the confusion matrix
print(mobilePhoneConfusionMatrix)

# Calculate false positive rate
mobilePhoneConfusionMatrix[1,2] / (mobilePhoneConfusionMatrix[1,1] 
                                   + mobilePhoneConfusionMatrix[1,2])

# Calculate false negative rate
mobilePhoneConfusionMatrix[2,1] / (mobilePhoneConfusionMatrix[2,1] 
                                   + mobilePhoneConfusionMatrix[2,2])

# Calculate model prediction accuracy
sum(diag(mobilePhoneConfusionMatrix)) / nrow(mobilePhoneTesting)