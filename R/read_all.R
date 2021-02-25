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
  j<- 1
  for (i in 2:length(file_list)){
    temp <- try(readxl::read_excel(file_list[i], sheet = sheet))
    if (class(temp) == "try-error") {
      temp <-data.table::fread(file_list[i])
    } 
    temp$survey <- basename(file_list[i])
    if (prod(colnames(df) == colnames(temp)) == 1){
      df <- rbindlist(list(df, temp), use.names = T) 
      df_list[[j]] <- df
    }  
    else {
      k <- 1
      need_to_append <- TRUE
      while (k <= j) {
        if(prod(colnames(temp) == colnames(df_list[[k]])) == 1){
          df_list[[k]] <- rbindlist(list(df_list[[k]], temp), use.names = T) 
          need_to_append <- FALSE
        }
        k <- k+1
      }
      if (k>j & need_to_append) {
        df_list[[k]] <- temp
        j <- k
      }
    }
  }
  
  
  if (length(df_list)>1) {
    return(df_list)
  } else {
    return(df)
  }
}