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

print.ryx  <- function(x){
  if(!inherits(x, "ryx")){
    stop("This function requires a object of type 'ryx'")
  }
  cat("Correlations of", x$y, "with \n")
  print(x$df)
}

summary.ryx <- function(x){
  
}
library(MASS)
x <- ryx(Boston, y="medv")
print(x)
plot.ryx = NULL

















