#' This script loaded up the GPA style for plotting.
#' @export
gpa_theme <- function(basesize){
  theme(text=element_text(size=basesize, colour = "black"),
        legend.position = 'top',
        panel.background = element_rect(fill= 'white'),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = basesize-3, hjust = 1, vjust=0),
        plot.subtitle = element_text(size = basesize-2, hjust = 0.5),
        plot.title = element_text(size = basesize, hjust = 0.5),
        axis.text.y = element_text(size = basesize-2,  colour = "black"),
        axis.text.x = element_text(size = basesize-2,  colour = "black"),
        axis.title.x = element_text(size = basesize-2,  colour = "black"),
        axis.title.y = element_text(size = basesize-2,  colour = "black"),
        legend.text = element_text(size = basesize-3))
}
