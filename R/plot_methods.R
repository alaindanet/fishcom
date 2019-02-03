#' Generate ggraph with a common layout
#'
#'  
#' @param net: data.frame describing edges 
#' @param glay a layout objet created by create_layout
#' @param title chr
#'
#' @return a ggraph object
set_layout_graph <- function (net, glay, title = NULL) {

      net <- graph_from_data_frame(net)
      # get node coord from the general:
      node_names <- attr(V(net), "name")
      coord <- filter(glay, name %in% node_names) %>%
	select(x, y, name)

      # local layout:
      llay <- create_layout(net, layout = "kk")
      saved_graph <- attr(llay, "graph")
      saved_circular_attr <- attr(llay, "circular")
      saved_class <- class(llay)
      llay <- select(llay, -x, -y) %>%
	left_join(., coord, by = "name") %>%
	select(x, y, everything())
      attr(llay, "graph") <- saved_graph
      attr(llay, "circular") <- saved_circular_attr
      class(llay) <- saved_class 

      p <- ggraph(llay) + 
	geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) + 
	geom_node_point()

      if(!is.null(title)) {
	p <- p + labs(title = title)
      }
      p + theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
}

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
