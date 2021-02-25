#' Read All Data in the Directory
#'
#' This function allows user to read and combine all like-data with the extension .csv, .xlsx, or .rds in a folder.
#'
#' Developer note: read_all() is essectially useful when reading Pollfish data for a persuasion test.
#'
#' @param dir The directory where all of your data are located
#' @param sheet The name of the excel sheet where your data are located
#' @return A combined data frame of all data in the directory or a list of combined like-datasets
#' @examples df<- read_all("C:/Users/", "Individuals Coded")
#'
#' @export
read_all <- function(dir, sheet = "Individuals Coded"){
  file_list <- list.files(dir, pattern=c("*.csv|*.xlsx|*.rds"), full.names=TRUE)
  df <- try(readxl::read_excel(file_list[1], sheet = sheet))
  if (class(df) == "try-error") {
    df <-data.table::fread(file_list[1])
  }
  df$survey <- basename(file_list[1])
  df_list <- list(df)

  for (id in 2:length(file_list)){
    temp <- try(readxl::read_excel(file_list[id], sheet = sheet))
    if (class(temp) == "try-error") {temp <-data.table::fread(file_list[id])}
    temp$survey <- basename(file_list[id])
    i<-1
    while (i <= length(df_list)) {
      if (prod(colnames(df_list[[i]]) == colnames(temp)) == 1){
        df_list[[i]] <- rbindlist(list(df_list[[i]], temp), use.names = T)
        break
      }
      i<- i+1
    }
    if (i>length(df_list)) {
      df_list[[length(df_list)+1]] <- temp
    }
  }


  if (length(df_list)>1) {
    return(df_list)
  } else {
    return(df)
  }
}
