# Programmers: Shivam Shishangia and Malcolm Shepherd
# MIS 545 Section 01
# Lab03Group25ShishangiaSheperd.R
# Using tidyverse to summarize and analyse data from grocery transaction list. 
# Also used ggplot to visualize the data into histograms and boxplots.

# Install the tidyverse package
# install.packages("tidyverse")

# Load the library
library(tidyverse)

# Set the working directory
setwd('C:/Users/ual-laptop/Desktop/545/Lab03')

# Load the values from .csv file
groceryTransactions1 <- read_csv(file = "GroceryTransactions.csv",
                                col_types = "iDffffifffffffin",
                                col_names = TRUE)

# Display the tibble on console
print(groceryTransactions1)

# Display the first 20 values
head(groceryTransactions1, n = 20)

# Display the structure
str(groceryTransactions1)

# Display the summary
summary(groceryTransactions1)

# Use dplyr to summarize the data
print(summarize(.data = groceryTransactions1, mean(Revenue)))
print(summarize(.data = groceryTransactions1, median(UnitsSold)))
print(summarize(.data = groceryTransactions1, sd(Revenue)))
print(summarize(.data = groceryTransactions1, IQR(UnitsSold)))
print(summarize(.data = groceryTransactions1, min(Revenue)))
print(summarize(.data = groceryTransactions1, max(Children)))

# Create a new tibble
groceryTransactions2 <- select(.data = groceryTransactions1,
                               PurchaseDate,
                               Homeowner,
                               Children,
                               AnnualIncome,
                               UnitsSold,
                               Revenue)

# Display non-homeowners with atleast 4 childrens from groceryTransactions2
print(filter(.data = groceryTransactions2,
             Homeowner == 'N' & Children >= 4))

# Display records and features from groceryTransactions2 that were made by 
# customers in $150k+ annual income category or had more than 6 units
print(filter(.data = groceryTransactions2,
             AnnualIncome == '$150K +' | UnitsSold >= 6))

# Display avg transaction revenue grouped by annual income level 
# and also sort them by avg transaction revenue from high to low.
print(groceryTransactions2 %>%
          group_by(AnnualIncome) %>%
          summarize(AverageTransactionRevenue = mean(Revenue)) %>%
          arrange(desc(AverageTransactionRevenue)))

# Create a new tibble
groceryTransactions3 <- groceryTransactions2 %>%
    mutate(AveragePricePerUnit = Revenue / UnitsSold)

# Create a histogram of AveragePricePerUnit using ggplot()
histogramAveragePricePerUnit <- ggplot(data = groceryTransactions3,
                                       aes(x = AveragePricePerUnit))

# Geometry layer
histogramAveragePricePerUnit + geom_histogram(binWidth = 1,
                                              color = "black",
                                              fill = "orange",
                                              alpha = 0.5)

# Create a boxplot of revenue using ggplot()
boxplotRevenue <- ggplot(data = groceryTransactions3,
                         aes(x = Revenue))

# Geometry layer
boxplotRevenue + geom_boxplot(color = "#0C234B",
                              fill = "#AB0520")