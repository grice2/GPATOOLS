#' This function generates p-values and power statistics 
#' for several treatments against a control, for a single survey question. 
#' 
#' Required Parameters:
#'
#' df - df containing survey response data
#' treatment - column specifying treatments of interest
#' alt - alternative hypothesis; can be "greater", "less", or "two.sided"
#' pval - column to hold p-values
#' power - column to hold power values 
#' NOTE: PVAL & POWER COLUMN NAMES MUST BE PASSED AS STRINGS

hypothesis_prep <- function(df, treatment, survey_q, value, pval, power){
  
  prep <- df %>% group_by({{treatment}},{{survey_q}}) %>% summarise(responses=n()) %>% 
    mutate(total = sum(responses)) %>% mutate(percent = responses/total) %>% filter({{survey_q}} == value)
  prep[[pval]] <- 0
  prep[[power]] <- 0
  return(prep)
  }
  