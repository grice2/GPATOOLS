#' Compute GPA's net attentive engagement metric given Facebook's engagement and reaction metrics
#'
#' This function will compute GPA's net attentive engagement according to the formula:
#' Attentive Engagement = 0.008984 + 0.021879*Comments + 0.378715*Shares + (-1.978481)*Angry + 0.086403*HaHa + 3.720785*Love + (-0.016704)*Sad + 0.131512*Wow + 1*Like
#'
#' First, each of the metrics was turned into a proportion of Reach.
#' Reaction types are collected at the daily but not gender/age level, so the denominator there is larger
#' The Response row sum is a rough proportion of all reach that had any of the responses
#' there could be duplication, but, in practice, it's not a big deal.
#' This is net attentive engagement, which is 'Attentive' minus 'Not Attentive'.
#' Due to the larger than 1 coefficients, it is possible for 'Attentive Engagement' to be greater than the total responses
#' In that case, we assume that negative was zero.
#'
#' We used a glm version of a weighted average to create the formula.
#' GPA/RA/RE have used IRT and other approaches, but glm is simplest and seems most stable.
#' In the past, GPA/RA/RE have also weighted this by Reach, but that tends to create biases when FB has been allowed to optimize.
#' In this case, we have a couple of lines where denominator is
#' many many thousands, so taking the weights off shows the real patterns more clearly.
#'
#' @param df Data frame or tibble that contains the following columns:
#' Facebook engagement metrics: 'angry', 'haha', 'like', 'love', 'sad', 'wow', 'post_comments', 'post_shares';
#' Ad information: 'ad_name'; and
#' Normalization metric: denominator
#' @param denominator The normalization metric, which is usually 'impressions', 'reach', or 'video_views'
#' @return Data frame or tibble with the net attentive engagement score (i.e.,net_attentive_engagement), positive engagement rate (i.e., attentive_engagement)
#' and total engagement rate (i.e., responses)
#' @examples compute_netp(combined.data)
#' @export
compute_netp <- function(df, denominator){
  df <- janitor::clean_names(df)
  df$denominator <- df[[denominator]]
  reaction.metrics <- c('angry','haha','like','love','sad','wow')
  engagement.metrics <- c('post_comments', 'post_shares')
  for (feature in c("ad_name","denominator",reaction.metrics,engagement.metrics)){
    if (!(feature %in% colnames(df))){
      stop("Variable \"", feature, "\" is missing from \"",deparse(substitute(df)),"\"" )
    }
  }
  engagement <- df %>% group_by(ad_name) %>%
    mutate(denominator_reactions=sum(denominator, na.rm=T))
  summary(engagement$denominator_reactions)
  for.model.engagement <- apply(engagement[,engagement.metrics], 2,
                                function(x){
                                  x <- x/(engagement$denominator)
                                  x[is.na(x)] <- 0
                                  return(x)
                                })
  for.model.reactions <- apply(engagement[,reaction.metrics], 2, function(x){
    x <- x/as.numeric(engagement$denominator_reactions)
    x[is.na(x)] <- 0
    return(x)
  })
  for.model <- cbind(for.model.engagement,for.model.reactions) %>% as_tibble()
  head(for.model)
  summary(for.model)
  cor(for.model)
  summary(engagement$denominator)
  coefs <- c(0.021879,0.378715,-1.978481,0.086403,3.720785,-0.016704,0.131512)
  attentive_engagement_linear <-  as.matrix(for.model[,c('post_comments', 'post_shares','angry','haha','love','sad','wow')]) %*%
    as.matrix(coefs, ncol=1)
  engagement$attentive_engagement <- for.model$like+attentive_engagement_linear
  engagement$responses <- rowSums(for.model)
  engagement$net_attentive_engagement <- engagement$attentive_engagement  -
    pmax(0,(engagement$responses-engagement$attentive_engagement))
  return(engagement)
}
