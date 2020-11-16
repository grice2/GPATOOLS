#' GPA Plotting Style
#'
#' This function is used as a supplement to 'ggplot' command to ensure the plots are done
#' in accordance to GPA Style Guide.
#'
#' @param basesize The size of your plot tittle, which will then be used to estimate size of other texts in your plot
#'
#' @examples
#' ggplot(ad.text.analysis, aes(x=short.text, y=netp)) +
#' geom_bar(stat='identity', position = "dodge", fill = primary.colors[1])+
#' coord_flip()+
#' geom_text(color = ad.text.analysis$colors,
#'           aes(x=short.text, y= netp, hjust = ifelse(netp<0.4/100,-0.1,1.1),
#'               label = format(round(netp,3)*100),nsmall=1),
#'               size = 3, fontface="bold") +
#' labs(x=NULL, y="\nNet Attentive Engagement (%)\n",
#'        caption = "Source: GPA/RA/RE Austria Creative Engagement Test Oct 2020")+
#' scale_y_continuous(labels = function(x) paste0(x*100))+
#' gpa_theme(11)+
#' scale_fill_manual('',values=primary.colors[c(2:1)])+
#' theme(axis.text.x = element_blank())+
#' ggsave('Austria-creative-ad-text.png', width = 6, height = 3, units = 'in')
#'
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
