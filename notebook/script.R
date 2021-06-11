## Required R libraries
load_libraries <- function(){
  library(usdm)
  library(sp)
  library(raster)
  library(dplyr)
  library(ggplot2)
  library(cluster)
  library(purrr)
  library(tidyverse)
  library(factoextra)
  library(DataExplorer)
  library(gridExtra)
  print("The libraries are loaded.")
}
load_libraries()

## Set working directory
setwd("/Users/schinlfc/data-science-R/e-commerce-clustering/data")

## Load dataset
ecommerce <- read.csv("Ecommerce.csv")

## Data preprocessing
# View the structure of the data frame
str(ecommerce)
# View basic summary statistics
summary(ecommerce)
# View number of rows and columns
dim(ecommerce)
# Find the number of missing values in the rows and columns
namat <- is.na(ecommerce)
table(rowSums(namat))
# Find the total number of missing values in each column
colSums(namat)
# The alternative way to find the the number of missing values in each column
naecommerce <- ecommerce %>% mutate_all(is.na)
print(naecommerce %>% summarise_all(sum))
# Remove column 'X' since the entire column fill with NA
ecommerce <- ecommerce[, -9]
# Remove missing rows for column 'CustomerID' 
ecommerce <- na.omit(ecommerce)
# View number of rows after removing NA's
nrow(ecommerce)
