library(sf); library(lwgeom); library(concaveman); library(ggplot2)


# Draft tower location
towers <- st_read('embargo/USWind_PDE-Locations_2022-0111.kml',
        layer = 'WTG_Layout_2022_0111') |> 
  st_zm()

# Development sequence lines
ab_line <- st_linestring(c(
  st_point(c(-74.706408, 38.456837)),
  st_point(c(-74.746202, 38.209318))
))

bc_line <- st_linestring(c(
  st_point(c(-74.762416, 38.512965)),
  st_point(c(-74.809502, 38.218600))
))

dev_lines <-  st_as_sf(st_sfc(ab_line, bc_line, crs = st_crs(towers)))


tower_boundary <- concaveman(towers)

towers[st_contains(tower_boundary,
                   st_buffer(towers, units::as_units(10, 'meters')),
                   sparse = F),]$Description <- 'inner'
towers[towers$Description != 'inner',]$Description <- 'outer'



ggplot() +
  geom_sf(data = towers, aes(color = Description)) +
  geom_sf(data = dev_lines) +
  geom_sf(data = tower_boundary, fill = NA)



wea <- st_read('data/geo/offshore wind layers.gdb',
               query = "select * from BOEM_Wind_Leases_as_of_Aug_3_2021 where State = 'Maryland'")
wea <- st_transform(wea, st_crs(towers))
wea <- st_buffer(wea, 1)
wea_split <- st_union(wea)
wea_split <- st_split(wea_split, st_geometry(dev_lines))
wea_split <- st_collection_extract(wea_split)
wea_split <- st_as_sf(wea_split)
wea_split$zone <- c('A', 'B', 'C')

towers <- st_join(towers, wea_split)

ggplot() +
  geom_sf(data = wea_split, aes(fill = zone)) +
  geom_sf(data = wea, fill = NA) +
  geom_sf(data = towers, aes(color = Description, shape = zone)) +
  scale_color_viridis_d() +
  geom_sf(data = dev_lines) +
  annotate(
    'point', y = 38.23071, x=	-74.75808, shape = 5
  ) +
  geom_sf(data = towers[9,], shape = 5, size = 6) +
  geom_sf(data = st_nearest_points(towers[9,],
                                   st_sfc(st_point(c(-74.75808, 38.23071)),
                                          crs = 4326)),
          color = 'red') +
  labs(x = NULL, y = NULL)

towers$dist_saetia <- st_distance(towers$geometry,
            st_sfc(st_point(c(-74.75808, 38.23071)), crs = 4326))

# closest to Saetia within the wind farm and in zone A
# towers[towers$Description == 'inner' & towers$zone == 'A',][which.min(towers[towers$Description == 'inner' & towers$zone == 'A',]$dist_saetia),]



sites <- data.frame(site = c('ocmd', 'twrecks', 'rtwb'),
                    lat = c(38.32742, 38.23071, 38.303),
                    long = c(-75.10333, -74.75808, -74.645)) |> 
  st_as_sf(coords = c('long', 'lat'), crs = 4326)


ggplot() +
  geom_sf(data = wea_split, aes(fill = zone)) +
  geom_sf(data = wea, fill = NA) +
  # geom_sf(data = towers, aes(color = Description, shape = zone)) +
  scale_color_viridis_d() +
  geom_sf(data = dev_lines) +
  geom_sf(data = sites) +
  geom_sf(data = towers[9,], shape = 5, size = 6) +
  labs(x = NULL, y = NULL)

library(rnaturalearth)

library(dplyr)
coast <- ne_states('United States of America', returnclass = 'sf') |> 
  filter(postal %in% c('MD', 'DE', 'NJ'))
plot(st_geometry(coast))

ggplot() +
  geom_sf(data = wea_split, aes(fill = zone)) +
  geom_sf(data = wea, fill = NA) +
  geom_sf(data = towers, aes(color = Description, shape = zone)) +
  geom_sf(data = coast) +
  scale_color_viridis_d() +
  geom_sf(data = dev_lines) +
  geom_sf(data = twrecks, shape = 5) +
  geom_sf(data = towers[9,], shape = 5, size = 6) +
  geom_sf(data = st_nearest_points(towers[9,],
                                   st_sfc(st_point(c(-74.75808, 38.23071)),
                                          crs = 4326)),
          color = 'red') +
  geom_sf(data = ocmd) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim  = c(-75.2, -74.55),
           ylim = c(38.2, 38.5), expand = F)


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
) |> 
  st_sfc(crs = 4326)





bathy <- read_sf('data/geo/bathymetry_midatl.gpkg') |> 
  st_transform(4326)


ggplot() +
  geom_sf(data = bathy[bathy$elev_m < 100,]) +
  geom_sf(data = control, alpha = 0.5) +
  geom_sf(data = wea_split, aes(fill = zone), alpha = 0.5, show.legend = F) +
  geom_sf(data = wea, fill = NA) +
  geom_sf(data = towers) +
  geom_sf(data = coast) +
  scale_color_viridis_d() +
  geom_sf(data = dev_lines) +
  geom_sf(data = sites, size = 6, fill = 'red') +
  geom_sf(data = towers[9,], size = 6, color = 'red') +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim  = c(-75.2, -74.55),
           ylim = c(38.1, 38.5), expand = F) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 45))

coast <- st_read('data/geo/mid-atlantic/matl_states_land.shp') |> 
  st_transform(4326)

ggplot() +
  geom_sf(data = bathy) +
  scale_color_viridis_c() +
  geom_sf(data = coast) +
  geom_sf(data = sites, size = 6) +
  geom_sf(data = towers[9,], size = 6) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim  = c(-75.2, -74.55),
           ylim = c(38.175, 38.45), expand = F) +
  theme_minimal()


