
# ryx

<!-- badges: start -->
<!-- badges: end -->

<img src="space.jpg" width="200"/>

The goal of ryx is to calculate correlation between different values in 
a data frame and give summary stats, a corelation matrix and a graph.

## Installation

You can install the development version of ryx from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("lmorril/QAC356_HW4")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ryx)
## basic example code
data(mtcars)
x <- ryx(mtcars, y="mpg", x=c("wt", "vs"))
```

