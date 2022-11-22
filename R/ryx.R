#' ryx
#'
#' A function that calculates correlations between a dependent variable y and
#' one or more independent x variables.
#'
#' @param data The dataset that contains that relevant variables
#' @param y The name of a numeric variable in the data frame that will be the
#' independent variable
#' @param x is a character vector with the names of one or more numeric
#' variables in the data frame. Defaults to all numeric columns in the dataframe
#'
#' @return An obect of type 'ryx' that has 3 attributes, The dependent variable,
#' the list of independent variables, and a dataframe showing correlation and
#' significance of each independent variable with the dependent.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #calculate correlation on 'medv' in the dataframe, uses all numeric columns
#' ryx(Boston, y="medv")
#' #can specify independent vars, and be assigned to a var
#' x <- ryx(mtcars, y="mpg", x=c("wt", "vs"))
#' }
#'
ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}



#' print.ryx
#'
#' Takes an ryx object and prints a correlation dataframe displaying
#' correlation and significance for each variable
#' @param x A ryx object
#'
#' @return A dataframe containing the correlation and significance of the
#' specified object. Has variable, r, p, and sigif column.
#' @export
#'
#' @examples
#' #example 1
#' print(ryx(mtcars, y="mpg", x=c("wt", "vs")))
#'
print.ryx  <- function(x){
  if(!inherits(x, "ryx")){
    stop("This function requires a object of type 'ryx'")
  }
  cat("Correlations of", x$y, "with \n")
  print(x$df)
}

#' summary.ryx
#'
#' Provides some summary statistics about the correlations, such as:
#' median correlation, range of correlation, how many variables were
#' statistically significant.
#' @param x A 'ryx' object
#'
#' @return A string
#' @export
#'
#' @examples
#'
#' summary(ryx(mtcars, y="mpg", x=c("wt", "vs")))
#'
summary.ryx <- function(x){
  medianVal = median(x$df$r)
  maxval = max(x$df$r)
  minval = min(x$df$r)
  sig =ifelse(x$df$p < 0.05, 1, 0)
  sig_len = length(sig)
  cat("Correlating ", x$y, " with ", x$x , "\n",
            "The median absolute correlation was", medianVal,
            "with a range from ", minval, " to ", maxval, "\n",
            sig_len, "out of", length(x$df$variable), "was significant at the p < 0.05 level.")
}


#' plot.ryx
#'
#'Plots a ryx object as a scatter plot where the x axis is correlation and y-axis are independent
#'variables
#' @param x A ryx object
#'
#' @return A ggplot graph of the correlations
#' @export
#'
#' @examples
#' plot(ryx(mtcars, y="mpg", x=c("wt", "vs")))
#'
plot.ryx <- function(x){
  if(!inherits(x, "ryx")){
    stop("This function requires an object of type 'ryx'.")
  }
  library(ggplot2)
  title <- paste("Correlations with", x$y)
  direction = ifelse(x$df$r > 0, "postive", "negative")
  xvar = x$df$r
  yvar = x$df$variable
  ggplot(data=x$df,
         aes(x = abs(xvar), y = reorder(yvar, abs(xvar)),
             color=direction)) +
    geom_point() +
    geom_segment(aes(yend=reorder(yvar, abs(xvar)), xend=0), color="grey") +
    scale_x_continuous(limits = c(0,1), breaks = seq(0.0, 1.0, by = 0.1)) +
    labs(title = title,
         x = "Correlation (absolution value)",
         y = "Variables")  +
    theme_minimal()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour="grey", linetype="dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
}







