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


print()











