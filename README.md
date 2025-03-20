# ffdata

<!-- badges: start -->
<!-- badges: end -->

ffdata is an R package which allows the automatic download of financial returns data from the [Kenneth R. French Data Library](https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html). For now, the implemented datasets include U.S. Research Returns Data, Bivariate sorts, Three-way sorts and Industry Portfolios.

## Installation

You can install this version of the package from Github with:

``` r
install.packages("devtools")
library(devtools)
install_github("antshi/ffdata")
library(ffdata)
```

## Examples

These are basic examples that show you how to use the main function ffdata_download().

### Example 1

Everything is set to default.
The function automatically downloads cleaned for NAs, monthly returns of 3-Fama-French factor portfolios with dividends from 1975/01 till today.

``` r
library(ffdata)
ffdata_download()
```

### Example 2

The function automatically downloads cleaned for NAs, monthly returns of 5-Fama-French factor portfolios with dividends from January 2001 to December 2018. The dataset is formatted and saved for a later usage in the package `portoptim`.

``` r
library(ffdata)
library(tidyverse)
ffdata_download(start = "200101", end = "201812", number_factors =  5)

ff_5factors <- read.csv("USResearch_m_200101_201812/F-F_Research_Data_5_Factors_2x3.csv")
str(ff_5factors)
ff_5factors$Date <- as.Date(paste0(ff_5factors$Date, "01"), format = "%Y%m%d")
ff_5factors <- ff_5factors %>%
  column_to_rownames("Date")
str(ff_5factors)
save(ff_5factors, file = "ff_5factors.rda")
```

### Example 3

Download monthly returns with dividends for the 48 Industry Portfolios from January 1990 till December 2018.

```r
library(ffdata)
ffdata_download(freq="m", type="Industry", factors.n=48, start="199001", end="201812")
```
