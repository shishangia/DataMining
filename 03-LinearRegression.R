# Programmers: Shivam Shishangia and Mohammed Yaseen Shaik
# MIS 545 Section 01
# Lab05Group18ShishangiaShaik.R
# This code creates a linear regression model of the dataset by analysing the 
# histograms for the dataset, checking the correlation matrix and correlation
# plot, finding the relation of zoo spending on party size, miles from the zoo 
# and zoo membership. It also tests for multicollinearity.

# Installing the tidyverse, corrplot, and olsrr packages
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")

# Loading the packages
library("tidyverse")
library("corrplot")
library("olsrr")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab05")

# Load the values from .csv file
zooSpending <- read_csv(file= "ZooVisitSpending.csv",
                             col_types = "niil",
                             col_names = TRUE)

# Display the tibble on console
print(zooSpending)

# Display the structure
str(zooSpending)

# Display the summary
summary(zooSpending)

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

# Call the displayAllHistograms() function by passing zooSpending to it
displayAllHistograms(zooSpending)

# Display a correlation matrix of zooSpending rounded to two decimal places
round(cor(zooSpending),2)

# Display a correlation plot and limit output to the bottom left
corrplot(cor(zooSpending),
         method = "number",
         type = "lower")

# Generate the linear regression model and save it in an object 
# called zooSpendingModel
zooSpendingModel <- lm(data = zooSpending,
                       formula = VisitSpending ~ .)

# Display the beta coefficients for the model
print(zooSpendingModel)

# Display the summary of the linear regression model
summary(zooSpendingModel)

# Test for multicollinearity
ols_vif_tol(zooSpendingModel)