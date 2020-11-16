#' Summazise Net Attentive Engagement by Specified Variable(s)
#'
#' This function groups engagement test data along up to three variables of interest,
#' and computes the denominator, positive engagement, negative engagement,
#' and net attentive engagement for each resulting group. It is most useful before plotting.
#'
#' @param data Dataset containing engagement metrics at the ad level (must contain attentive_engagement, denominator, responses)
#' @param var1 - first variable to group by
#' @param var2 - second variable to group by (optional)
#' @param var3 - third variable to group by (optional)
#' @return A smaller dataframe that contains the weighted mean of net attentive engagement by the specified variable(s)
#' @examples summarize_netp(data = df, var1 = ad_name, var2 = age)
#' @export
summ_netp <- function(data, var1, var2, var3) {
  for (feature in c("attentive_engagement","denominator","responses")){
    if (!(feature %in% colnames(data))){
      stop("Variable \"", feature, "\" is missing from \"", deparse(substitute(data)),"\"" )
    }
  }
  if (missing(var3)) {
    df <- data %>% group_by({{var1}}, {{var2}}) %>%
      summarise(ppos=weighted.mean(attentive_engagement, denominator),
                pneg=pmax(0,weighted.mean(responses-attentive_engagement, denominator)),
                denominator=sum(denominator,na.rm=T)) %>%
      mutate(netp=ppos-pneg,
             marginoferror=1.96*sqrt(
               (ppos*(1-ppos))/denominator+(pneg*(1-pneg))/denominator))
  } else if (missing(var2)) {
    df <- data %>% group_by({{var1}}) %>%
      summarise(ppos=weighted.mean(attentive_engagement, denominator),
                pneg=pmax(0,weighted.mean(responses-attentive_engagement, denominator)),
                denominator=sum(denominator,na.rm=T)) %>%
      mutate(netp=ppos-pneg,
             marginoferror=1.96*sqrt(
               (ppos*(1-ppos))/denominator+(pneg*(1-pneg))/denominator))
  } else {
    df <- data %>% group_by({{var1}}, {{var2}}, {{var3}}) %>%
      summarise(ppos=weighted.mean(attentive_engagement, denominator),
                pneg=pmax(0,weighted.mean(responses-attentive_engagement, denominator)),
                denominator=sum(denominator,na.rm=T)) %>%
      mutate(netp=ppos-pneg,
             marginoferror=1.96*sqrt(
               (ppos*(1-ppos))/denominator+(pneg*(1-pneg))/denominator))
  }
  return(df)
}
