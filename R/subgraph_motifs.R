#' Return subgraph motifs
#'
#' The function return the 13 possible motifs that you can find in a
#' graph
#'  
#' @param adj an adjacency matrix
#' @details The original definition from by Borrelli (2015) filled the matrix
#' where prey gets -1 interaction values when eaten by a predator 
#' @references Borrelli, J. J. (2015). Selection against instability: stable subgraphs are most frequent in empirical food webs. Oikos, 124(12), 1583‑1588. https://doi.org/10.1111/oik.02176
#'
#' @return A list of matrix     
get_possible_subgraph <- function () {

s1 <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
s2 <- matrix(c(0, 0, 0, 1, 0, 0, 1, 1, 0), ncol = 3, byrow = TRUE)
s3 <- matrix(c(0, 0, 1, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
s4 <- matrix(c(0, 0, 0, 0, 0, 0, 1, 1, 0), ncol = 3, byrow = TRUE)
s5 <- matrix(c(0, 0, 0, 1, 0, 0, 1, 0, 0), ncol = 3, byrow = TRUE)
d1 <- matrix(c(0, 1, 0, 1, 0, 0, 1, 1, 0), ncol = 3, byrow = TRUE)
d2 <- matrix(c(0, 0, 0, 1, 0, 1, 1, 1, 0), ncol = 3, byrow = TRUE)
d3 <- matrix(c(0, 0, 1, 0, 0, 1, 0, 1, 0), ncol = 3, byrow = TRUE)
d4 <- matrix(c(0, 0, 1, 0, 0, 0, 1, 1, 0), ncol = 3, byrow = TRUE)
d5 <- matrix(c(1, 0, 1, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
d6 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), ncol = 3, byrow = TRUE)
d7 <- matrix(c(0, 0, 1, 1, 0, 1, 1, 1, 0), ncol = 3, byrow = TRUE)
d8 <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 0), ncol = 3, byrow = TRUE)

mot.lst <- list(s1, s2, s3, s4, s5, d1, d2, d3, d4, d5, d6, d7, d8)
names(mot.lst) <- c("s1", "s2", "s3", "s4", "s5", "d1", "d2", "d3", "d4", "d5",
"d6", "d7", "d8")

return(mot.lst)
}

#' Count subgraph motifs
#' 
#'  
#' @param list of graph of the igraph class
#' @details The function return the 13 possible motifs that you can find in a
#' graph   
#' @references Borrelli, J. J. (2015). Selection against instability: stable subgraphs are most frequent in empirical food webs. Oikos, 124(12), 1583‑1588. https://doi.org/10.1111/oik.02176
#'
#' @return A data.frame containing the frequency of each subgraph
count_motif <- function(graph.lists) {

  if (!is.list(graph.lists)) {
    stop("The input should be a list of graph objects")
  }
  triad.count <- lapply(graph.lists, igraph::triad.census)
  triad.matrix <- matrix(unlist(triad.count), nrow = length(graph.lists),
    ncol = 16, byrow = T)
  colnames(triad.matrix) <- c("empty", "single", "mutual", "s5", "s4", "s1",
    "d4", "d3", "s2", "s3", "d8", "d2", "d1", "d5", "d7", "d6")
  triad.df <- as.data.frame(triad.matrix)

  motif.data.frame <- data.frame(s1 = triad.df$s1, s2 = triad.df$s2, s3 = triad.df$s3,
    s4 = triad.df$s4, s5 = triad.df$s5, d1 = triad.df$d1, d2 = triad.df$d2,
    d3 = triad.df$d3, d4 = triad.df$d4, d5 = triad.df$d5, d6 = triad.df$d6,
    d7 = triad.df$d7, d8 = triad.df$d8)

  rownames(motif.data.frame) <- names(graph.lists)
  return(motif.data.frame)
}

#' Swap with curve ball algorithm 
#'
#' The function makes a swap and return the matrix
#'  
#' @param m a matrix 
#' @details     
#' @references Borrelli, J. J. (2015). Selection against instability: stable subgraphs are most frequent in empirical food webs. Oikos, 124(12), 1583‑1588. https://doi.org/10.1111/oik.02176
#' @references Strona, G. et al. 2014. A fast and unbiased procedure to randomize ecological binary matrices
#' with fixed row and column totals. -Nat. Comm. 5: 4114. doi: 10.1038/ncomms5114
#'
#' @return A matrix 
curve_ball <- function(m) {

  RC <- dim(m)
  R  <- RC[1]
  C  <- RC[2]
  hp <- list()

  for (row in 1:dim(m)[1]) {
    hp[[row]] <- (which(m[row, ] == 1))
  }

  l_hp <- length(hp)
  for (rep in 1:5 * l_hp) {
    AB   <- sample(1:l_hp, 2)
    a    <- hp[[AB[1]]]
    b    <- hp[[AB[2]]]
    ab   <- intersect(a, b)
    l_ab <- length(ab)
    l_a  <- length(a)
    l_b  <- length(b)
    if ( (l_ab %in% c(l_a, l_b)) == F) {
      tot   <- setdiff(c(a, b), ab)
      l_tot <- length(tot)
      tot   <- sample(tot, l_tot, replace <- FALSE, prob <- NULL)
      L     <- l_a - l_ab
      hp[[AB[1]]] <- c(ab, tot[1:L])
      hp[[AB[2]]] <- c(ab, tot[(L + 1):l_tot])
    }
  }

  rm <- matrix(0, R, C)
  for (row in 1:R) {
    rm[row, hp[[row]]] <- 1
  }
  rm
}

#' Motif null model with curve ball algorithm 
#'
#' The function
#'  
#' @param adjmat an adjacency matrix 
#' @param n int number of permutation  
#' @details The function performs a permutation with the curve ball algorithm
#' and count the frequency of the motifs
#' @references Borrelli, J. J. (2015). Selection against instability: stable subgraphs are most frequent in empirical food webs. Oikos, 124(12), 1583‑1588. https://doi.org/10.1111/oik.02176
#'
#' @return A data.frame containing the frequency of motifs for each permutation 
curving <- function(adjmat, n) {
  mot    <- count_motif(list(igraph::graph_from_adjacency_matrix(adjmat)))
  newmat <- adjmat

  for (i in 1:n) {
    newmat <- curve_ball(newmat)
    m <- count_motif(list(igraph::graph_from_adjacency_matrix(newmat)))
    mot <- rbind(mot, m)
  }
  return(mot[-1, ])
}

#' Motif null model with curve ball algorithm 
#'
#' 
#'  
#' @param mat an adjacency matrix 
#' @param iter int number of permutation  
#' @details The function performs a permutation with the curve ball algorithm
#' and count the frequency of the motifs. Different from curving function.  This
#' algorithm maintains the number of single, double, and self links in each
#' matrix to the original curveball algorithm in the function.
#' @references Borrelli, J. J. (2015). Selection against instability: stable subgraphs are most frequent in empirical food webs. Oikos, 124(12), 1583‑1588. https://doi.org/10.1111/oik.02176
#'
#' @return A data.frame containing the frequency of motifs for each permutation 
dblcan.curve <- function(mat, iter) {

  # determines the number of double links
  nd <- function(gl) nrow(gl) - nrow(unique(plyr::aaply(gl, 1, sort)))

  mot <- count_motif(list(igraph::graph_from_adjacency_matrix(mat)))
  el  <- igraph::as_edgelist(igraph::graph_from_adjacency_matrix(mat))
  Ne  <- nrow(el)
  dbl <- nd(el)
  can <- sum(diag(mat))

  for (i in 1:iter) {
    ed  <- TRUE
    dub <- TRUE
    ca  <- TRUE

    while (ed || dub || ca) {

      mat2 <- curve_ball(mat)
      el2  <- igraph::as_edgelist(igraph::graph_from_adjacency_matrix(mat2))
      el3  <- unique(el2)
      Ne2  <- nrow(el3)
      dbl2 <- nd(el3)
      can2 <- sum(diag(mat2))
      ed   <- Ne != Ne2
      dub  <- dbl != dbl2
      ca   <- can != can2
    }
    mat <- mat2
    mot <- rbind(mot, count_motif(list(igraph::graph_from_adjacency_matrix(mat))))
  }
  M <- mot[-1, ]
  return(M)
}

