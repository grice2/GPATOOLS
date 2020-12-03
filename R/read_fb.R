#' Read and Format GPA Facebook Data
#'
#' This function will read in two data frames, i.e., the post reaction types data and the post engagement data, combine and ready them for analysis.
#'
#' @param df_reactions_dir Directory to GPA Facebook post reaction types data (must contains 'post_reaction_type', 'post_reactions', and 'ad_name')
#' @param df_engagement_dir Directory to GPA Facebook post engagement data (must contains 'ad_name')
#' @return A combined dataset with additional columns 'audience' and 'ad_id' needed for the next step of analysis.
#' @examples
#' read_fb(df_reactions_dir = "reaction_data.csv", df_engagement_dir = "engagement_data.csv")
#' @export
read_fb <- function(df_reactions_dir,df_engagement_dir) {
  df_reactions <- try(data.table::fread(df_reactions_dir))
  if (class(df_reactions) == "try-error") {
    df_reactions <-readxl::read_excel(df_reactions_dir)
  }
  df_engagement <- try(data.table::fread(df_engagement_dir))
  if (class(df_engagement) == "try-error") {
    df_engagement <-readxl::read_excel(df_engagement_dir)
  }
  df_engagement <- df_engagement %>% janitor::clean_names()
  df_reactions <- df_reactions %>% janitor::clean_names()
  for (feature in c("ad_name","post_reaction_type",'post_reactions')){
    if (!(feature %in% colnames(df_reactions))){
      stop("Variable \"", feature, "\" is missing from \"",deparse(substitute(df_reactions)),"\"" )
    }
  }
  for (feature in c("ad_name")){
    if (!(feature %in% colnames(df_engagement))){
      stop("Variable \"", feature, "\" is missing from \"",deparse(substitute(df_engagement)),"\"" )
    }
  }
  df_reactions <- df_reactions %>% reshape2::dcast(ad_name~post_reaction_type, value.var = 'post_reactions')
  df <- dplyr::full_join(df_engagement,df_reactions)
  df$audience <- str_extract(df$ad_name, 'Audience [0-9]+')
  df$ad_id <- str_extract(df$ad_name, 'Ad [0-9]+')
  return(df)
}
