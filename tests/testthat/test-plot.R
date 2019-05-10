context("plot")
library('tidyverse')
library('magrittr')
library('lubridate')
library('igraph')
library('ggraph')
mypath <- rprojroot::find_package_root_file

myload(test_network, dir = mypath("data"))
test_network
test_network2 <- test_network
test_network2[1, 2] <- 1
test_network2 %<>%
  .[!rownames(.) %in% "Pikachu_6", !colnames(test_network2) %in% "Pikachu_6"]
  
net_test <- tibble(
  date = c("2001-01-01", "2002-01-01") %>% lubridate::year(.),
  net = list(test_network, test_network2)
  ) %>%
mutate(net = map(net, graph_from_adjacency_matrix))
node <- colnames(test_network)
x <- runif(length(node))
names(x) <- node

test_that("plot temporal networks works", {

  net_test %<>%
    mutate(
      y = purrr::map(net,
    function (x) {
      x <- as_adjacency_matrix(x, sparse = FALSE)
      node_pos <- NetIndices::TrophInd(x, Dead = "Chetiflor")$TL - 1
      names(node_pos) <- colnames(x)
      node_pos
    }))
  species_resource_list <- purrr::map(net_test$net, function(net) {
    if (class(net) == "data.frame") {
      net <- graph_from_data_frame(net, directed = TRUE)
    } else if (class(net) == "matrix") {
      net <- graph_from_adjacency_matrix(net, mode = "directed")
    } else {
      stopifnot(class(net) == "igraph")
    }
    attr(V(net), "name")
      }) %>%
  unlist(., use.names = FALSE) %>%
  str_extract(., "[A-Z][a-z]+") %>%
  unique
  color <- set_color_species(species_resource_list,
    species_list = c("Pikachu", "Salameche"),
    resource_list = c("Chetiflor", "Paras"))
  color2 <- c("yellow", "green", "brown", "red")
  names(color2) <- names(color)

net_test %<>%
  mutate(size = map(y, function(x){
      test <- rnorm(length(x), 10, 3); names(test) <- names(x)
      test
} ))

debug(set_layout_graph)
p <- set_layout_graph(
  net = net_test[["net"]][[1]],
  title = "test",
  x = x,
  y = net_test[["y"]][[1]],
  biomass = net_test[["size"]][[1]], color_scale = color)

net_test %<>%
  mutate(net = map(net, as_data_frame),
    net = map(net, as_tibble))

undebug(set_layout_graph)
source(mypath("R", "plot_methods.R"))
p1 <- plot_temporal_network(
   data = net_test,
   net_var = net,
   date = date,
   x = x,
   y = net_test$y,
   color = color,
   size = net_test$size)
p1
})

test_that("set_color_species returns a the good list",{

nodes <- net_test[["net"]][[1]] %>% as_adjacency_matrix(., sparse = FALSE)  %>% colnames()

output <- set_color_species(node_list = nodes,
  species_list = c("Salameche", "Pikachu"),
  resource_list = c("Paras", "Chetiflor"))

debug(set_color_species)
expected <- c("Salameche", "Pikachu", "Paras", "Chetiflor")

expect_true(all(names(output) %in% expected))

})

test_that("y is defined for all species", {

net_test2 <- net_test %>%
  mutate(
    y = map(y, function (x) {
      x[!names(x) %in% c("Chetiflor", "Paras")]
}),
  size = map(size, function (x) {
      x[!names(x) %in% c("Chetiflor", "Paras")]
}))

set_layout_graph(
  net = net_test2[["net"]][[1]],
  title = "test",
  x = x,
  y = net_test2[["y"]][[1]],
  biomass = net_test2[["size"]][[1]], color_scale = color)

source(mypath("R", "plot_methods.R"))
undebug(plot_temporal_network)
undebug(set_layout_graph)
p <- plot_temporal_network(
   data = net_test2,
   net_var = net,
   date = date,
   x = x,
   y = net_test2$y,
   color = color,
   size = net_test2$size) #net_test$size

})

