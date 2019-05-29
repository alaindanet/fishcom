#' Match points to lines
#' 
match_pt_line <- function(point = NULL, line = NULL, start_buffer = 0, inc_buffer = .1, max_buffer = Inf) {

  stopifnot(!is.null(point) | !is.null(line))
  # Start a list of length of the point list  
  line_id <- integer(nrow(point))
  point$id <- seq(1, nrow(point))
  names(line_id) <- point$id
  line$id <- seq(1, nrow(line))

  counter <- 0
  pt_temp <- point
  buff_size <- start_buffer + inc_buffer * counter

  # while
  while (any(line_id == 0) & buff_size < max_buffer) {
    # Begin with the buffer:
    pt_buffer <- st_buffer(pt_temp, dist = buff_size)

    intersect_line <- st_intersects(pt_buffer, line)
    mask_single <- map_lgl(intersect_line,
      function(x) ifelse(length(x) == 1, TRUE, FALSE))
    #message(paste0(sum(mask_single), " pts have matches with one line."))
    # Fill list with those that have one match
    line_id[names(line_id) %in% pt_temp$id[mask_single]] <-
      sapply(intersect_line[mask_single], function(x) x[1])

    # For multiple matches, get the closer
    mask_multi <- map_lgl(intersect_line,
      function(x) ifelse(length(x) > 1, TRUE, FALSE))
    if (any(mask_multi)) {
      message(paste0(sum(mask_multi), " pts have matches with multi."))
      # Fill list with the closer:
      line_id[names(line_id) %in% pt_temp$id[mask_multi]] <- map2_dbl(which(mask_multi), intersect_line[mask_multi],
	function(pts_id, line_id) {
	  dis <- st_distance(pt_temp[pts_id, ], line[line_id, ])
	  line_id[which.min(dis[1, ])]
	})
    }
    # Increase buffer size
    counter <- counter + 1
    buff_size <- start_buffer + inc_buffer * counter
    pt_temp <- filter(point, id %in% names(line_id)[line_id == 0])
    message(paste0("Buffer size = ", buff_size,". ", nrow(pt_temp)," points do not have lines."))

  }
  line_id
}
