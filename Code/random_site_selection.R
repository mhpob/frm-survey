library(sf)

wea <- st_read('data/geo/offshore wind layers.gdb',
               query = "select * from BOEM_Wind_Leases_as_of_Aug_3_2021 where State = 'Maryland'") %>% 
  st_transform(26918)



plot(st_geometry(wea))

cntrl_hgt <- (st_bbox(wea)$ymax - st_bbox(wea)$ymin) / 3

control <- st_polygon(
  list(
    cbind(
      c(st_bbox(wea)$xmin, st_bbox(wea)$xmax, st_bbox(wea)$xmax,
        st_bbox(wea)$xmin, st_bbox(wea)$xmin),
      c(st_bbox(wea)$ymin, st_bbox(wea)$ymin, st_bbox(wea)$ymin - cntrl_hgt,
        st_bbox(wea)$ymin - cntrl_hgt, st_bbox(wea)$ymin)
    )
  )
)

control <- st_sfc(control, crs = 26918)


library(ggplot2)
ggplot() +
  geom_sf(data = wea, fill = 'red') +
  geom_sf(data = control, fill = 'green') 



dist_sample <- function(x, n, dist){
  pts <- x %>% 
    st_sample(n) %>% 
    st_sf()
  
  i <- 1 # iterator start
  
  repeat( {
    #  create buffer around i-th point
    buffer <- st_buffer(pts[i,], dist) 
    
    offending <- pts %>%  # start with the intersection of master points... 
      st_intersects(buffer, sparse = F) # ... and the buffer, as a vector
    
    # i-th point is not really offending - it is the origin (not to be excluded)
    offending[i] <- FALSE
    
    # if there are any offending points left - re-assign the master points, 
    # with the offending ones excluded / this is the main pruning part :)
    pts <- pts[!offending,] 
    
    if ( i >= nrow(pts)) {
      if(nrow(pts) < n){
        pts <- rbind(pts, st_sf(geometry = st_sample(x, 1)))
        i <- i + 1 
        # the end was reached; no more points to process
      } else {
        break
      }
    } else {
      # rinse & repeat
      i <- i + 1 
    }
    
  } )
  
  pts
}

exp_pts <- dist_sample(wea, 8, 2000)



ggplot() +
  geom_sf(data = wea, fill = 'red') +
  geom_sf(data = control, fill = 'green') +
  geom_sf(data = exp_pts)


ctrl_pts <- dist_sample(control, 4, 2000)

ggplot() +
  geom_sf(data = wea, fill = 'red') +
  geom_sf(data = control, fill = 'green') +
  geom_sf(data = exp_pts) +
  geom_sf(data = ctrl_pts)


# Transit times (min) @ 7 kt (Sea born)
sb_loc <- st_sf(geometry = st_sfc(st_point(c(-75.10333, 38.32742)), crs = 4326) %>% 
  st_transform(26918))

sites <- rbind(sb_loc, exp_pts, ctrl_pts)

k <- st_distance(sites)

library(TSP)
atsp <- ATSP(as.dist(k))
atsp[, 1] <- 0


initial_tour <- solve_TSP(atsp)

tour <- solve_TSP(atsp, method ="two_opt", control = list(tour = initial_tour))
as.integer(tour)


path <- cut_tour(tour, 1, exclude_cut = FALSE)

ggplot() +
  geom_sf(data = wea, fill = 'red') +
  geom_sf(data = control, fill = 'green') +
  geom_sf(data = exp_pts) +
  geom_sf(data = ctrl_pts) +
  geom_sf(data = st_sfc(st_cast(do.call(c, st_geometry(sites[path,])), 'LINESTRING'), crs = 26918))



tsp <- TSP(as.dist(k))
# atsp[, 1] <- 0


initial_tour <- solve_TSP(tsp)

tour <- solve_TSP(tsp, method ="two_opt", control = list(tour = initial_tour))
as.integer(tour)


path <- cut_tour(tour, 1, exclude_cut = F)

ggplot() +
  geom_sf(data = wea, fill = 'red') +
  geom_sf(data = control, fill = 'green') +
  geom_sf(data = exp_pts) +
  geom_sf(data = ctrl_pts) +
  geom_sf(data = st_sfc(st_cast(do.call(c, st_geometry(sites[path,])), 'LINESTRING'), crs = 26918))
