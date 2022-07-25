library(ggplot2); library(sf); library(TSP)

# Import MD WEA shape
wea <- st_read('data/geo/offshore wind layers.gdb',
               query = "select * from BOEM_Wind_Leases_as_of_Aug_3_2021 where State = 'Maryland'") %>% 
  st_transform(26918)

# plot(st_geometry(wea))

# We want the control area to be ~ 1/3 of the height of the WEA
cntrl_hgt <- (st_bbox(wea)$ymax - st_bbox(wea)$ymin) / 3

# Create a polygon representing the control area
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


# Add Ocean City, MD
ocmd <- c(-75.10333, 38.32742) %>% 
  st_point() %>% 
  st_sfc(crs = 4326) %>% 
  st_sf(geometry = .,
        site = 'ocmd') %>% 
  st_transform(26918)


# Function to sample points in the polygons that are at least some distance apart
dist_sample <- function(x, n, dist){
  pts <- x %>% 
    st_sample(n) %>% 
    st_sf()
  
  i <- 1 # iterator start
  
  repeat({
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
      # Sample new points if some were dropped
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
  })
  
  pts
}


# Experimental points
exp_pts <- dist_sample(wea, 8, 2000)
exp_pts$site <- 'experimental'

# ggplot() +
#   geom_sf(data = wea, fill = 'red') +
#   geom_sf(data = control, fill = 'green') +
#   geom_sf(data = exp_pts)


# Control points
ctrl_pts <- dist_sample(control, 4, 2000)
ctrl_pts$site <- 'control'

# ggplot() +
#   geom_sf(data = wea, fill = 'red') +
#   geom_sf(data = control, fill = 'green') +
#   geom_sf(data = exp_pts) +
#   geom_sf(data = ctrl_pts)




sites <- rbind(ocmd,
               exp_pts,
               ctrl_pts)


# Transit times
tsp <- TSP(as.dist(st_distance(sites)))


tour <- solve_TSP(tsp, method = 'farthest_insertion')

# tour <- solve_TSP(tsp, method ="two_opt", control = list(tour = tour))


path <- c(cut_tour(tour, 1, exclude_cut = FALSE), 1)
tour_length(tour)
tour_length(tour) / 1852 / 7


ggplot() +
  geom_sf(data = wea, fill = 'red') +
  geom_sf(data = control, fill = 'green') +
  geom_sf(data = exp_pts) +
  geom_sf(data = ctrl_pts) +
  geom_sf(data = st_sfc(st_cast(do.call(c, st_geometry(sites[c(path),])), 'LINESTRING'), crs = 26918))

job::job({

times <- replicate(1000, 
  tryCatch({
  # Experimental points
  exp_pts <- dist_sample(wea, 8, 2000)
  exp_pts$site <- 'experimental'
  exp_pts <- exp_pts[1:8,]
  
  # Control points
  ctrl_pts <- dist_sample(control, 4, 2000)
  ctrl_pts$site <- 'control'
  ctrl_pts <- ctrl_pts[1:4,]
  # ctrl_pts
  
  sites <- rbind(ocmd,
                 exp_pts,
                 ctrl_pts)
  
  tsp <- TSP(as.dist(st_distance(sites)))
  
  tour <- solve_TSP(tsp, method = 'farthest_insertion')
  tour <- solve_TSP(tsp, method = 'two_opt', control = list(tour = tour))
  
  path <- c(cut_tour(tour, 1, exclude_cut = FALSE), 1)
  # tour_length(tour)
  tour_length(tour) / 1852 / 7
  },
  error = function(e) NA
))

})

hist(times)
