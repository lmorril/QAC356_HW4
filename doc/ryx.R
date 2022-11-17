## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ryx)

## -----------------------------------------------------------------------------
library(MASS)
x <- ryx(Boston, y="medv")

#or an example using the independent variable parameter
data(mtcars)
x <- ryx(mtcars, y="mpg", x=c("wt", "vs"))


## -----------------------------------------------------------------------------
print(ryx(Boston, y="medv"))
print(ryx(mtcars, y="mpg", x=c("wt", "vs")))

## -----------------------------------------------------------------------------
summary(ryx(Boston, y="medv"))
summary(ryx(mtcars, y="mpg", x=c("wt", "vs")))

## -----------------------------------------------------------------------------
plot(ryx(Boston, y="medv"))
plot(ryx(mtcars, y="mpg", x=c("wt", "vs")))

