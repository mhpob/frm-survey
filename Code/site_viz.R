library(readxl); library(sf); library(data.table); library(ggplot2)

pot <- read_excel('embargo/pot_data/pot survey recorded data.xlsx')
setDT(pot)

pot <- unique(pot[type == 'deploy'], by = c('station', 'datetime'))
pot <- pot |> 
  st_as_sf(coords = c('lon_start', 'lat_start'),
           crs = 4326,
           remove = F)

tbs <- turbines[c(st_contains(zones[zones$zone == 'A',], geom,
                   sparse = F))]
tbs[, geom := st_as_sfc(st_as_text(geom))]

ggplot() +
  geom_sf(data = ctrl_area, fill = NA) +
  geom_sf(data = zones, fill = NA) +
  geom_sf(data = tbs, aes(geometry = geom,
                          size = location),
          color = 'black',
          show.legend = F) +
  scale_size_manual(values = c(1, 0)) +
  geom_sf(data = pot, color = 'red') +
  geom_sf(data = stations[stations$station != 'OCMD',], shape = 21, size = 3) +
  labs(title = 'May 2023') +
  theme_minimal()
