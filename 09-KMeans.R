# Programmers: Shivam Shishangia and Unnati Palande
# MIS 545 Section 01
# Lab11Group06ShishangiaPalande.R
# Generate the dataset into clusters to based on the corruption index and the 
# number of days required to open a business. This is done using k-means. 
# Also use different techniques to optimize the value of k for optimal solution.

# Install the packages
# install.packages("tidyverse")
# install.packages("factoextra")

# Load the packages
library("tidyverse")
library("stats")
library("factoextra")
library("cluster")
library("gridExtra")

# Set the working directory
setwd("C:/Users/ual-laptop/Desktop/545/Lab11")

# Load the values from .csv file
countries <- read_csv(file = "CountryData.csv",
                      col_types = "cnnnnini",
                      col_names = TRUE)

# Display the tibble on console
print(countries)

# Display the structure
str(countries)

# Display the summary
summary(countries)

# Convert the column containing the country name to the row title of the tibble
countries <- countries %>% column_to_rownames(var = "Country")

# Remove countries from the tibble with missing data
countries <- countries %>% drop_na()

# Display the summary
summary(countries)

# Scale the tibble to only contain corruption index and the number of days 
# it takes to open a business
countriesScaled  <- countries %>% 
    select(CorruptionIndex, DaysToOpenBusiness) %>% scale()

# Set the random seed to 679
set.seed(679)

# Generate k-means clusters
countries4Clusters <- kmeans(x = countriesScaled,
                             centers = 4,
                             nstart = 25)

# Display cluster sizes
countries4Clusters$size

# Display cluster centers
countries4Clusters$centers

# Visualize the clusters
fviz_cluster(object = countries4Clusters,
             data = countriesScaled,
             repel = FALSE)

# Optimizing the value of k
# 1. Elbow method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "wss")

# 2. Average silhouette method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "silhouette")

# 3. Gap statistic method
fviz_nbclust(x = countriesScaled,
             FUNcluster = kmeans,
             method = "gap_stat")

# Regenerate the cluster analysis using the optimal number of clusters
countries3Clusters <- kmeans(x = countriesScaled,
                             centers = 3,
                             nstart = 25)

# Display cluster sizes
countries3Clusters$size

# Display cluster centers
countries3Clusters$centers

# Visualize the clusters
fviz_cluster(object = countries3Clusters,
             data = countriesScaled,
             repel = FALSE)

# Determine similarities and differences among the clusters using the 
# remaining features in the dataset
countries %>%
    mutate(cluster = countries3Clusters$cluster) %>%
    select(cluster,
           GiniCoefficient, 
           GDPPerCapita, 
           EduPercGovSpend, 
           EduPercGDP,
           CompulsoryEducationYears) %>%
    group_by(cluster) %>%
    summarise_all("mean")