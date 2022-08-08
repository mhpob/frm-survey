#### June 2022 ----
# set.seed(20220627)
# 
# # NEED: 2 sites control, 2 sites experimental
# n_control <- 2
# n_exp <- 2
# # Consideration: near early stages (far?). split box
# ctrl_area <- st_polygon(
#   list(
#     cbind(
#       c(
#         ctrl_bbox$xmin, ctrl_bbox$xmin + (ctrl_bbox$xmax - ctrl_bbox$xmin)/2,
#         ctrl_bbox$xmin + (ctrl_bbox$xmax - ctrl_bbox$xmin)/2,
#         ctrl_bbox$xmin, ctrl_bbox$xmin
#       ),
#       c(ctrl_bbox$ymax, ctrl_bbox$ymax,
#         ctrl_bbox$ymin + (ctrl_bbox$ymax - ctrl_bbox$ymin)/2,
#         ctrl_bbox$ymin+ (ctrl_bbox$ymax - ctrl_bbox$ymin)/2, ctrl_bbox$ymax
#       )
#     )
#   )
# ) |> 
#   st_sfc(crs = 4326)

#### July 2022 ----
# set.seed(20220718)
#
# #NEED: 2 sites control, 3 sites experimental
# n_control <- 2
# n_exp <- 3

#### August 2022 ----
# set.seed(20220801)

#### September 2022 ----
set.seed(20220829)

#NEED: 2 sites control, 4 sites experimental
n_control <- 2
n_exp <- 4

#### Code ----
library(sf); library(ggplot2); library(concaveman); library(TSP); library(units)
library(data.table); library(targets)

towers <- copy(tar_read(towers))

tower_boundary <- concaveman::concaveman(towers)

# towers$location <- NA
setDT(towers)[c(st_contains(tower_boundary,
                   st_buffer(geometry, units::as_units(10, 'meters')),
                   sparse = F)), location := 'inner']
towers[is.na(location), location := 'outer']


zones <- st_read(tar_read(wea_spatial), layer = 'buildout zones', quiet = T)
ctrl_area <- st_read(tar_read(wea_spatial), layer = 'control area', quiet = T)

ctrl_bbox <- st_bbox(ctrl_area)

ctrl_sites <- ctrl_area |> 
  st_transform(26918) |>
  st_buffer(dist = -units::set_units(550, 'meter')) |>
  st_sample(n_control)|> 
  st_as_sf() |> 
  st_transform(4326) |>
  DT(, station := paste('C0722', 1:n_control, sep = '_'))

setnames(ctrl_sites, 'x', 'geometry')
st_geometry(ctrl_sites) <- 'geometry'

exp_sites <- towers |> 
  DT(grepl('^A-', station) & location == 'inner') |>
  DT(sample(.N, n_exp)) |> 
  DT(, location := NULL) |> 
  st_as_sf()

ocmd <- c(-75.10333, 38.32742) %>% 
  st_point() %>% 
  st_sfc(crs = 4326) %>% 
  st_sf(geometry = .,
        station = 'ocmd')

stations <- rbind(ocmd, ctrl_sites, exp_sites)

ggplot() +
  geom_sf(data = zones) +
  geom_sf(data = ctrl_area) +
  geom_sf(data = ctrl_sites) +
  geom_sf(data = exp_sites)

# Transit times ----
tsp <- TSP(as.dist(st_distance(stations)))

tour <- solve_TSP(tsp, method = 'farthest_insertion')

path <- c(cut_tour(tour, 1, exclude_cut = FALSE), 1)
tour_length(tour)
tour_length(tour) / 1852 / 7.6

round(set_units(st_distance(st_as_sf(stations)), 'nautical_mile'), 2)
round(set_units(st_distance(st_as_sf(stations)), 'mile'), 2)

matrix((as.numeric(set_units(st_distance(st_as_sf(stations)), 'nautical_mile') / 7.6) -
  as.integer((set_units(st_distance(st_as_sf(stations)), 'nautical_mile') / 7.6)))*60, ncol = 7)

ggplot() +
  geom_sf(data = zones) +
  geom_sf(data = towers, aes(geometry = geometry)) +
  geom_sf(data = ctrl_area) +
  geom_sf(data = ctrl_sites) +
  geom_sf(data = exp_sites) +
  geom_sf(data = st_sfc(
    st_cast(
      do.call(c, st_geometry(stations[c(path),])),
      'LINESTRING'
      ), crs = 4326
    ))

bathy <- read_sf('data/geo/bathymetry_midatl.gpkg') |> 
  st_transform(4326)

midatl <- read_sf('data/geo/mid-atlantic/matl_states_land.shp')

ggplot() +
  geom_sf(data = bathy[bathy$elev_m < 100,]) +
  geom_sf(data = midatl) +
  geom_sf(data = ctrl_area, alpha = 0.5) +
  geom_sf(data = zones, alpha = 0.5, show.legend = F) +
  scale_color_viridis_d() +
  geom_sf(data = stations, size = 6, fill = 'red') +
  geom_sf_label(data = stations, aes(label = station), nudge_x = 0.03, nudge_y = 0.01) +
  geom_sf(data = st_sfc(
    st_cast(
      do.call(c, st_geometry(stations[c(path),])),
      'LINESTRING'
      ), crs = 4326
    )) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim  = c(-75.2, -74.55),
           ylim = c(38.1, 38.5), expand = F) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 45)) 


# Convert to decimal minutes ----
setDT(stations)[, ':='(lon = st_coordinates(geometry)[,1],
                       lat = st_coordinates(geometry)[,2])]
dm <- function(x){
  paste(as.integer(x), round((abs(x) %% 1) * 60, 3))
}
stations[, ':='(lon_dm = dm(lon),
                lat_dm = dm(lat))]


# Plot habitat ----
hab <- st_read(file.path('C:/Users/darpa2/Analysis/bottom-trawl-species-dist/Data/Mapping',
                         'benthic_habitats/benhab_mab.shp'),
               query = 'SELECT * FROM benhab_mab LIMIT 0')

crop_box <- st_bbox(c(ymin = 38.1, xmin = -75.2,
                      ymax = 38.5, xmax = -74.55),
                    crs = 4326) |> 
  # turn into a simple features collection
  st_as_sfc()|> 
  st_transform(3857)

hab <- st_read(file.path('C:/Users/darpa2/Analysis/bottom-trawl-species-dist/Data/Mapping',
                         'benthic_habitats/benhab_mab.shp'),
               # turn box into well-known text
               wkt_filter = st_as_text(crop_box))

h2 <- hab |> 
  dplyr::group_by(ECOLOGICAL) |> 
  dplyr::summarise(geometry = st_union(geometry))

sta_hab <- stations[-1,] |> 
  st_as_sf() |> 
  st_buffer(dist = units::set_units(5, mile)) |> 
  st_transform(st_crs(h2)) |> 
  st_intersection(x = h2, y = _) 

h3 <- st_crop(h2, sta_hab)


ggplot() +
  geom_sf(data = h3, aes(fill = ECOLOGICAL)) +
  geom_sf(data = st_as_sf(stations[-1, ]), size = 6, color = 'red', aes(geometry = geometry)) +
  geom_sf_label(data = st_as_sf(stations[-1, ]), aes(label = station, geometry = geometry),
                nudge_x = 2000, nudge_y = 2000) +
  scale_fill_viridis_d(option = 'H') +
  labs(x = '', y = '', fill = 'Bottom') +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 45)) 
