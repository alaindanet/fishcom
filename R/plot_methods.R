#' Personal plot theme
#'
#' @details This theme was design from the theme developped in the hrbrthemes
#' package: https://github.com/hrbrmstr/hrbrthemes 
#' @return a ggplot theme
theme_alain <- function(){

  hrbrthemes::theme_ipsum_rc() +
  theme(#Whipe
    text = element_text(family = "Helvetica", hjust = .5),
    axis.title = element_text(family = "Helvetica", hjust = .5, face = "bold", size = 10),
    plot.title = NULL,
    axis.title.x = NULL,
    axis.title.y = NULL,
    axis.text.x = NULL,
    axis.text.y = NULL,
    strip.text = NULL) +
  theme(#Set up
    plot.title = element_text(family = "Helvetica", hjust = .5, face = "bold", size = 10),
    axis.title.y = element_text(angle = 90, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.text = element_text(size = 8),
    strip.text = element_text(size = 8),
    plot.margin = unit(c(.5, .5, .5, .5), "cm")
    )
}
