#' The function below groups engagement test data along up to three variables of interest,
#' and computes the reach, positive engagement, negative engagement,
#' and net positive engagement for each resulting group. It is most useful before plotting.
#'
#' Required Parameters:
#'
#' input_df - dataset containing engagement metrics at the ad level
#' eng_var - typically corresponds to "Attentive.engagement"
#' resp_var -  typically corresponds to "responses"
#' weight_var - typically corresponds to "Denominator"
#' group_var1 - first variable to group by (the other two are optional)
#'
summarize_engagement <- function(input_df, eng_var, resp_var, weight_var, group_var1, group_var2, group_var3) {
  if (missing(group_var3)) {
    input_df %>% group_by({{group_var1}}, {{group_var2}}) %>%
      summarise(ppos=weighted.mean({{eng_var}}, {{weight_var}}),
                pneg=pmax(0,weighted.mean({{resp_var}}-{{eng_var}}, {{weight_var}})),
                reach=sum({{weight_var}},na.rm=T)) %>%
      mutate(netp=ppos-pneg,
             marginoferror=1.96*sqrt(
               (ppos*(1-ppos))/reach+(pneg*(1-pneg))/reach))
  } else if (missing(group_var2)) {
    input_df %>% group_by({{group_var1}}) %>%
      summarise(ppos=weighted.mean({{eng_var}}, {{weight_var}}),
                pneg=pmax(0,weighted.mean({{resp_var}}-{{eng_var}}, {{weight_var}})),
                reach=sum({{weight_var}},na.rm=T)) %>%
      mutate(netp=ppos-pneg,
             marginoferror=1.96*sqrt(
               (ppos*(1-ppos))/reach+(pneg*(1-pneg))/reach))
  } else {
    input_df %>% group_by({{group_var1}}, {{group_var2}}, {{group_var3}}) %>%
      summarise(ppos=weighted.mean({{eng_var}}, {{weight_var}}),
                pneg=pmax(0,weighted.mean({{resp_var}}-{{eng_var}}, {{weight_var}})),
                reach=sum({{weight_var}},na.rm=T)) %>%
      mutate(netp=ppos-pneg,
             marginoferror=1.96*sqrt(
               (ppos*(1-ppos))/reach+(pneg*(1-pneg))/reach))
  }

}
