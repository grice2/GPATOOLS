#' This function generates the p-values associated with
#' several treatments against a SINGLE control, for a single survey question. 
#' 
#' #Preprocessing that generates input DF usually looks like this:
#' Q4.ad.analysis <- combined.data %>% group_by(Ad,Q4) %>% summarise(n_q4=n()) %>% group_by(Ad) %>% 
#' mutate(total_q4 = sum(n_q4)) %>% mutate(percent_q4 = n_q4/total_q4) %>% filter(Q4 == 1)
#' 
#' Required Parameters:
#'
#' df - df containing survey response data, summarized at the treatment level and containing counts of responses
#' treatment - column specifying treatments of interest
#' counts - column containing response counts 
#' size - column containing sample size for each treatment 
#' alt - alternative hypothesis; can be "greater", "less", or "two.sided"
#' NOTE: COLUMN NAMES MUST BE PASSED AS STRINGS

get_alpha <- function(df, treatment, counts, size, alt){
  pvals <- vector(mode="numeric", length = length(df))
 
  for (i in which(df[[treatment]] == "Control")){
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

