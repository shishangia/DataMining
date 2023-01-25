# Programmers: Shivam Shishangia and Gurleen Kohli 
# MIS 545 Section 01
# Lab10Group19ShishangiaKohli.R
# Create a model to identify the association rules in a dataset of 
# Instacart Transactions.

# Install the packages
# install.packages("tidyverse")
# install.packages("arules")

# Load the packages
library("tidyverse")
library("arules")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab10")

# Load the values from .csv file
instacartTransactions <- read.transactions(file = "InstacartTransactions.csv",
                                           format = "single",
                                           header = TRUE,
                                           sep = ",",
                                           cols = c("OrderID", "ItemID"))

# Display the summary
summary(instacartTransactions)

# Display the first three transactions on the console
inspect(instacartTransactions[1:3])

# Examine the frequency of item: 24852 (bananas)
itemFrequency(instacartTransactions[, "24852"])

# Convert the frequency values into a tibble
instacartTransactionsFrequency <-
    tibble(Items = names(itemFrequency(instacartTransactions)),
           Frequency = itemFrequency(instacartTransactions))

# Display the tibble on the console
print(instacartTransactionsFrequency)

# Display the 10 most frequently purchased items on the console
instacartTransactionsFrequency %>%
    arrange(desc(Frequency)) %>%
    slice(1:10)

# Generate association rules models
instacartTransactionRules <- apriori(instacartTransactions,
                                     parameter = list(
                                         support = 0.005,
                                         confidence = 0.2,
                                         minlen = 2))

# Display the summary of the model
summary(instacartTransactionRules)

# Display the first 10 associaation rules
inspect(instacartTransactionRules[1:10])

# Sort the association rules by lift and view the top 10
instacartTransactionRules %>%
    sort(by = "lift") %>%
    head(n = 10) %>%
    inspect()