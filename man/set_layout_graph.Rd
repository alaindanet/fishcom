% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_methods.R
\name{set_layout_graph}
\alias{set_layout_graph}
\title{Generate ggraph with a common layout}
\usage{
set_layout_graph(net, glay, title = NULL, biomass = NULL,
  color_scale = NULL)
}
\arguments{
\item{glay}{a layout objet created by create_layout}

\item{title}{chr}

\item{net:}{data.frame describing edges}
}
\value{
a ggraph object
}
\description{
Generate ggraph with a common layout
}
