context("match_pt_line")


pts <- st_sfc(
  st_point(c(0,.5)), # Should work with at the first try
  st_point(c(1.5, 2.12)), # Should have one after at the second it 
  st_point(c(1.75, 1.79))) # Should compute the dist to match
pts <- st_sf(id = 1:3, st_sfc(pts))
pol <- st_multilinestring(
  list(
    rbind(
      c(0,0),
      c(2,0)),
rbind(c(2,0),
      c(2,2)),
rbind(c(2,2),
      c(0,2)),
rbind(c(0,2),
      c(0,0))
    )) %>%
st_sfc %>%
st_cast(.,"LINESTRING")
pol <- st_sf(id = 1:4, st_sfc(pol))
plot(st_geometry(pts))
plot(st_geometry(pol), add = T)
pts_buffer <- st_buffer(pts, .1)
plot(st_geometry(pts_buffer), add = TRUE)

test_that("match works", {
  # Test with start 0, inc = 0.1
  # pt 1 should match the line 4
  # pt 2 and 3 should match the line 3
  source(mypath("R", "geo_methods.R"))
  match_pt_line(pts, pol, start_buffer = 0.001)
  debug(match_pt_line)
  undebug(match_pt_line)

  pt_buffer <- st_buffer(point, dist = .1)
  plot(point)
  plot(pt_buffer, add = TRUE)
  plot(line, add = TRUE)

})
