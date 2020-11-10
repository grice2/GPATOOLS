#' This function generates the power values associated with
#' several treatments against a control, for a single survey question where 
#' SAMPLE SIZE IS EQUAL. FOR UNEQUAL SAMPLE SIZES, SEE get_power2
#' 
#' Required Parameters:
#'
#' df - df containing survey response data
#' treatment - column specifying treatments of interest
#' prop - column containing response proportions
#' size - sample size; must be numeric
#' alt - alternative hypothesis; "one.sided" or "two.sided"
#' NOTE: COLUMN NAMES MUST BE PASSED AS STRINGS

get_power1 <- function(df, treatment, prop, size, alt){
  power_vals <- vector(mode="numeric", length = length(df))
  foo <- function(input){round(power.prop.test(n=size, 
                                    p1 = df[[prop]][which(df[[treatment]] != "Control")],
                                    p2 = df[[prop]][which(df[[treatment]] == "Control")],
                                    sig.level = 0.05,
                                    power = NULL,
                                    alternative = alt,
                                    strict = FALSE,
                                    tol = .Machine$double.eps^0.25)$power,4)}
    power <- sapply(df[[prop]], foo)[,1]
    idx <- which(df[[treatment]] == "Control", arr.ind=TRUE)
    power <- insert(power,ats=idx,values=0)
  return(power)
}
