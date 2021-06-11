High value customers identification for an E-Commerce company
================
Sayorn Chin
06/10/2021

### 1. Problem statement

A UK-based online retail store has captured the sales data for different
products for the period of one year (Nov 2016 to Dec 2017). The
organization sells gifts primarily on the online platform. The customers
who make a purchase consume directly for themselves. There are small
businesses that buy in bulk and sell to other customers through the
retail outlet channel.

### 2. Project objective

This project aims to find significant customers for the business who
make high purchases of their favourite products. The organization wants
to roll out a loyalty program to the high-value customers after
identification of segments. I use the clustering methodology to segment
customers into groups.

### 3. Data and methodology

#### 3.1 Data

``` r
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
```

    ## [1] "The libraries are loaded."

``` r
## Set working directory
setwd("/Users/schinlfc/data-science-R/e-commerce-clustering/data")

## Load dataset
ecommerce <- read.csv("Ecommerce.csv")

## Data preprocessing
# View the structure of the data frame
str(ecommerce)
```

    ## 'data.frame':    541909 obs. of  9 variables:
    ##  $ InvoiceNo  : chr  "536365" "536365" "536365" "536365" ...
    ##  $ StockCode  : chr  "85123A" "71053" "84406B" "84029G" ...
    ##  $ Description: chr  "WHITE HANGING HEART T-LIGHT HOLDER" "WHITE METAL LANTERN" "CREAM CUPID HEARTS COAT HANGER" "KNITTED UNION FLAG HOT WATER BOTTLE" ...
    ##  $ Quantity   : int  6 6 8 6 6 2 6 6 6 32 ...
    ##  $ InvoiceDate: chr  "29-Nov-16" "29-Nov-16" "29-Nov-16" "29-Nov-16" ...
    ##  $ UnitPrice  : num  2.55 3.39 2.75 3.39 3.39 7.65 4.25 1.85 1.85 1.69 ...
    ##  $ CustomerID : int  17850 17850 17850 17850 17850 17850 17850 17850 17850 13047 ...
    ##  $ Country    : chr  "United Kingdom" "United Kingdom" "United Kingdom" "United Kingdom" ...
    ##  $ X          : logi  NA NA NA NA NA NA ...

``` r
# View basic summary statistics
summary(ecommerce)
```

    ##   InvoiceNo          StockCode         Description           Quantity        
    ##  Length:541909      Length:541909      Length:541909      Min.   :-80995.00  
    ##  Class :character   Class :character   Class :character   1st Qu.:     1.00  
    ##  Mode  :character   Mode  :character   Mode  :character   Median :     3.00  
    ##                                                           Mean   :     9.55  
    ##                                                           3rd Qu.:    10.00  
    ##                                                           Max.   : 80995.00  
    ##                                                                              
    ##  InvoiceDate          UnitPrice           CustomerID       Country         
    ##  Length:541909      Min.   :-11062.06   Min.   :12346    Length:541909     
    ##  Class :character   1st Qu.:     1.25   1st Qu.:13953    Class :character  
    ##  Mode  :character   Median :     2.08   Median :15152    Mode  :character  
    ##                     Mean   :     4.61   Mean   :15288                      
    ##                     3rd Qu.:     4.13   3rd Qu.:16791                      
    ##                     Max.   : 38970.00   Max.   :18287                      
    ##                                         NA's   :135080                     
    ##     X          
    ##  Mode:logical  
    ##  NA's:541909   
    ##                
    ##                
    ##                
    ##                
    ## 

``` r
# View number of rows and columns
dim(ecommerce)
```

    ## [1] 541909      9

``` r
# Find the number of missing values in the rows and columns
namat <- is.na(ecommerce)
table(rowSums(namat))
```

    ## 
    ##      1      2 
    ## 406829 135080

``` r
# Find the total number of missing values in each column
colSums(namat)
```

    ##   InvoiceNo   StockCode Description    Quantity InvoiceDate   UnitPrice 
    ##           0           0           0           0           0           0 
    ##  CustomerID     Country           X 
    ##      135080           0      541909

``` r
# The alternative way to find the the number of missing values in each column
naecommerce <- ecommerce %>% mutate_all(is.na)
print(naecommerce %>% summarise_all(sum))
```

    ##   InvoiceNo StockCode Description Quantity InvoiceDate UnitPrice CustomerID
    ## 1         0         0           0        0           0         0     135080
    ##   Country      X
    ## 1       0 541909

``` r
# Remove column 'X' since the entire column fill with NA
ecommerce <- ecommerce[, -9]
# Remove missing rows for column 'CustomerID' 
ecommerce <- na.omit(ecommerce)
# View number of rows after removing NA's
nrow(ecommerce)
```

    ## [1] 406829
