# Programmers: Shivam Shishangia and Mohamad Jafar
# MIS 545 Section 01
# Lab04Group14ShishangiaJafar.R
# This code uses different data processing techniques like preprocessing the
# data by imputing missing data, then calculating the outliers, normalizing and 
# discretizing the UsageMonths, creating a dummy code and then plotting it on a
# scatterplot.

# Install the tidyverse package
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type="source")

# Load the library
library(tidyverse)
library(dummies)

# Set the working directory
setwd('C:/Users/ual-laptop/Desktop/545/Lab04')

# Load the values from .csv file
tireTread1 <- read_csv(file = "TireTread.csv",
                       col_types = "cfnni",
                       col_names = TRUE)

# Display the tibble on console
print(tireTread1)

# Display the structure
str(tireTread1)

# Display the summary
summary(tireTread1)

# Impute missing data with mean of UsageMonths
tireTread2 <- tireTread1 %>%
    mutate(UsageMonths = ifelse(is.na(UsageMonths), 
                                mean(UsageMonths, na.rm = TRUE), UsageMonths))

# Display the summary
summary(tireTread2)

# Calculate the minimum outlier for TreadDepth
outlierMin <- quantile(tireTread2$TreadDepth, .25) -
    (IQR(tireTread2$TreadDepth) * 1.5)

# Calculate the maximum outlier for TreadDepth
outlierMax <- quantile(tireTread2$TreadDepth, .75) +
    (IQR(tireTread2$TreadDepth) * 1.5)

# Store the outliers in their own tibble
treadDepthOutliers <- tireTread2 %>%
    filter(TreadDepth < outlierMin | TreadDepth > outlierMax)

# Normalize UsageMonths by storing its log in a feature called LogUsageMonths
tireTread3 <- tireTread2 %>%
    mutate(LogUsageMonths = log(UsageMonths))

# Discretize the UsageMonths and store it into new tireTread4 tibble
tireTread4 <- tireTread3 %>%
    mutate(NeedsReplacing = TreadDepth <= 1.6)

# Convert the tireTread4 tibble into a new data frame
tireTread4DataFrame <- data.frame(tireTread4)

# Dummy code by position (LF, RF, LR, RR)
tireTread5 <- as_tibble(dummy.data.frame(data = tireTread4DataFrame,
                                         names = "Position"))

# Create a scatterplot of TreadDepth using ggplot()
scatterPlotTireTread <- ggplot(data = tireTread5,
                               aes(x = Miles,
                                   y = TreadDepth))

# Add the geometry layer and the best fit line
scatterPlotTireTread + geom_point(color = "dark gray") +
    geom_smooth(method = lm,
                level = 0,
                color = "red") +
    ggtitle("Tire Miles and Tread Depth Scatter Plot")