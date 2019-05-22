#' Match points to lines
#' 
match_pt_line <- function(point = NULL, line = NULL, start_buffer = 0, inc_buffer = .1) {

  stopifnot(!is.null(point) | !is.null(line))
  # Start a list of length of the point list  
  line_id <- integer(nrow(point))

  # Begin with the buffer:
  pt_buffer <- st_buffer(point, dist = start_buffer)
  intersect_line <- st_intersects(pt_buffer, line)
  mask_single <- map_lgl(intersect_line,
    function(x) ifelse(length(x) == 1, TRUE, FALSE))
  message(paste0(sum(mask_single), " pts have matches with one line."))
  # Fill list with those that have one match
  line_id[mask_single] <-
    sapply(intersect_line[mask_single], function(x) x[1])

  # For multiple matches, get the closer
  mask_multi <- map_lgl(intersect_line,
    function(x) ifelse(length(x) > 1, TRUE, FALSE))
  if (any(mask_multi)) {
    message(paste0(sum(mask_multi), " pts have matches with multi."))
    point[mask_multi]
  st_distance()
     
  }
  # Fill list with the closer:
  # Increase buffer size
  line_id
}
