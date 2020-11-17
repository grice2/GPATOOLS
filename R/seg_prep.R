#' Re-format GPA Survey Data for Audience Segmentation
#'
#' This function gets the survey data ready for audience segmentation model
#'
#' @param data Data containing survey response data
#' @param start_idx The column index that correspond to the first value question in the survey
#' @return A new data frame with value-based questions properly coded
#' @examples get_seg(data = df, start_idx= 35)
#' @export
seg_prep <- function(data, start_idx) {
  colnames(data)[start_idx] <- "E018"
  data$E018 <- factor(data$E018, levels = c(1,2,3), labels = c(1,2,1.5))

  colnames(data)[start_idx+1] <- "A006"
  data$A006 <- factor(data$A006, levels= c(1,2,3,4))

  colnames(data)[start_idx+2] <- "A042"
  data$A042 <- factor(data$A042, levels = c(1,2,3,4), labels= c(1,1,1,0))

  colnames(data)[start_idx+3] <- "A029"
  data$A029 <- factor(data$A029, levels = c(1,2,3,4), labels= c(1,1,1,0))

  colnames(data)[start_idx+4] <- "A040"
  data$A040 <- factor(data$A040, levels = c(1,2,3,4), labels= c(1,1,1,0))

  colnames(data)[start_idx+5] <- "A039"
  data$A039 <- factor(data$A039, levels = c(1,2,3,4), labels= c(1,1,1,0))

  colnames(data)[start_idx+6] <- "D059"
  data$D059 <- factor(data$D059, levels = c(1,2,3,4))

  colnames(data)[start_idx+7] <- "C001"
  data$C001 <- factor(data$C001, levels = c(1,2,3))

  colnames(data)[start_idx+8] <- "D060"
  data$D060 <- factor(data$D060, levels = c(1,2,3,4))

  data$E003 <- case_when(data[,start_idx+9] == 1 ~ 1,
                         data[,start_idx+10] == 1 ~ 2,
                         data[,start_idx+11] == 1 ~ 3,
                         data[,start_idx+12] == 1 ~ 4)

  data$E004 <- case_when(data[,start_idx+9] == 2 ~ 1,
                         data[,start_idx+10] == 2 ~ 2,
                         data[,start_idx+11] == 2 ~ 3,
                         data[,start_idx+12] == 2 ~ 4)

  q_id <- c("E018","A006","A042","A029","A040","A039","D059","C001","D060","E003","E004")

  data[,q_id]<- mutate_if(data[,q_id],is.factor, as.character)
  data[,q_id]<- mutate_if(data[,q_id],is.factor, as.numeric)

  return(data)
}
