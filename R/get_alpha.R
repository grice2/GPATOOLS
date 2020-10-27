#' This function generates the p-values associated with
#' several treatments against a control, for a single survey question. 
#' 
#' 
#' Required Parameters:
#'
#' df - df containing survey response data
#' treatment - column specifying treatments of interest
#' counts - column containing response counts 
#' size - column containing sample size for each treatment 
#' alt - alternative hypothesis; can be "greater", "less", or "two.sided"

get_alpha <- function(df, treatment, counts, size, alt){
  pvals <- vector(length = length(df))
 
  for (i in which(df[[treatment]] != "Control")){
    pvals[i] <- 0
  }
   for (i in which(df[[treatment]] != "Control")){
    pvals[i] <-round(print(
      prop.test(x = c(df[[counts]][i], 
                      df[[counts]][df[[treatment]]=='Control']), 
                n = c(df[[size]][i], 
                      df[[size]][df[[treatment]]=='Control']),
                alternative = alt)$p.value),4)
    print(i)
  }
  return(pvals)
  }

