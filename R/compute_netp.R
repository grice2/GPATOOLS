#' This script will compute the net attentive engagement metric given a dataset
#'
#' Reaction types are collected at the daily but not gender/age level
#' so the denominator there is larger
#'
#' First, turn each of the metrics into a proportion of Reach
#'
#' We used a glm version of a weighted average
#' I have used IRT and other approaches, but this is simplest and seems most stable
#' In the past, I have also weighted this by Reach, but that tends to create biases
#' when FB has been allowed to optimize. In this case, we have a couple of lines where Denominator is
#' many many thousands, so taking the weights off shows the real patterns more clearly
#'
#' Likes are added back
#'
#' The Response row sum is a rough proportion of all Denominator that had any of the responses
#' there could be duplication, but, in practice, it's not a big deal
#'
#' This is net Attentive engagement. It's Attentive minus not Attentive.
#' because of the larger than 1 coefficients, it is possible for Attentive engagement to be greater than the total responses
#' In that case, we assume that negative was zero
#'
#' data will need the following columns
#' 'angry','haha','like','love','sad','wow'
#' 'Post.Comments', 'Post.Shares'
#' Ad.Name,
#' Denominator
#'
compute_netp <- function(combined.data){
  reaction.metrics <- c('angry','haha','like','love','sad','wow')
  engagement.metrics <- c('Post.Comments', 'Post.Shares')
  for (feature in c("Ad.Name","Denominator",reaction.metrics,engagement.metrics)){
    if (!(feature %in% colnames(combined.data))){
      stop("Variable \"", feature, "\" is missing from \"",deparse(substitute(combined.data)),"\"" )
    }
  }
  engagement <- combined.data %>% group_by(Ad.Name) %>%
    mutate(Denominator.reactions=sum(Denominator, na.rm=T))
  summary(engagement$Denominator.reactions)
  for.model.engagement <- apply(engagement[,engagement.metrics], 2,
                                function(x){
                                  x <- x/(engagement$Denominator)
                                  x[is.na(x)] <- 0
                                  return(x)
                                })
  for.model.reactions <- apply(engagement[,reaction.metrics], 2, function(x){
    x <- x/as.numeric(engagement$Denominator.reactions)
    x[is.na(x)] <- 0
    return(x)
  })
  for.model <- cbind(for.model.engagement,for.model.reactions) %>% as_tibble()
  head(for.model)
  summary(for.model)
  cor(for.model)
  summary(engagement$Denominator)
  coefs <- c(0.021879,0.378715,-1.978481,0.086403,3.720785,-0.016704,0.131512)
  Attentive.engagement.linear <-  as.matrix(for.model[,c('Post.Comments', 'Post.Shares','angry','haha','love','sad','wow')]) %*%
    as.matrix(coefs, ncol=1)
  engagement$Attentive.engagement <- for.model$like+Attentive.engagement.linear
  engagement$responses <- rowSums(for.model)
  engagement$net.Attentive.engagement <- engagement$Attentive.engagement  -
    pmax(0,(engagement$responses-engagement$Attentive.engagement))
  return(engagement)
}
