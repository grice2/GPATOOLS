#' Format data from FB
#'
#' df_Reactions must have the following columns
#' Post Reaction Type
#' Post Reactions
#' Ad Name
#'
#' df_Reach must have the following column(s)
#' Ad Name
#' @export
format_fb_data <- function(df_Reactions,df_Reach) {
  df_Reactions <- df_Reactions %>% dcast(`Ad Name`~`Post Reaction Type`, value.var = 'Post Reactions')
  combined.data <- full_join(df_Reactions,df_Reach)
  colnames(combined.data) <- make.names(colnames(combined.data))
  combined.data$Audience <- str_extract(combined.data$Ad.Name, 'Audience [0-9]+')
  combined.data$AdID <- str_extract(combined.data$Ad.Name, 'Ad [0-9]+')
  return(combined.data)
}
