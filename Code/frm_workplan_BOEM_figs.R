library(sf); library(ggplot2); library(concaveman); library(units)
library(data.table); library(targets)


turbines <- usw_locations |> 
  tar_read() |> 
  st_read(layer = 'WTG_Layout_2022_0111', quiet = TRUE)

turbine_boundary <- concaveman::concaveman(turbines)

setDT(turbines)[c(st_contains(turbine_boundary,
                              st_buffer(geom, units::as_units(10, 'meters')),
                              sparse = F)), location := 'inner']
turbines[is.na(location), location := 'outer']


zones <- st_read(tar_read(usw_buildout),
                 quiet = T)
ctrl_area <- st_read(tar_read(frm_control),
                     quiet = T)

coast <- st_read('data/geo/matl_states.gpkg')

pot <- ggplot() +
  geom_sf(data = zones, aes(fill = zone), show.legend = F) +
  geom_sf(data = ctrl_area, alpha = 0.5, fill = 'pink') +
  geom_sf(data = coast) +
  geom_sf(data = turbines[grepl('^[JKLMNOPQR]', ID) & location == 'inner'],
          aes(geometry = geom)) +
  geom_sf(data = st_sample(ctrl_area, 25), size = 0) +
  coord_sf(xlim = c(-75.15, -74.6), ylim = c(38.1, 38.55)) +
  scale_fill_manual(values = c('lightblue', NA, NA), na.value = NA) +
  theme_minimal() +
  theme(axis.text.y = element_blank())


rec_ctrl_sites <- data.table(
  station = c('Twin Wrecks', 'Great Eastern Reef'),
  lat = c(38.23071, 38.43049),
  long = c(-74.75808, -74.76770)
) |> 
  st_as_sf(coords = c('long', 'lat'),
           crs = 4326)

rec <- ggplot() +
  geom_sf(data = zones, aes(fill = zone), show.legend = F) +
  geom_sf(data = coast) +
  geom_sf(data = turbines[grepl('^[JKLMNOPQR]', ID) & location == 'inner'],
          aes(geometry = geom)) +
  geom_sf(data = rec_ctrl_sites, size = 2, color = 'darkgreen') +
  coord_sf(xlim = c(-75.15, -74.6), ylim = c(38.1, 38.55)) +
  scale_fill_manual(values = c('lightblue', NA, NA), na.value = NA) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 35))

library(patchwork)
rec + pot + plot_annotation(tag_levels = 'A')


mmm_vr <- data.table(
  site = c('A-5C', 'T-1C', 'T-2C', 'T-3C', 'V4', 'V5'),
  lat = c('38 20.166', '38 20.165', '38 20.161',
          '38 20.635', '38 20.103', '38 20.213'),
  long = c('74 43.355', '74 56.905', '74 30.270',
           '74 25.441', '74 40.609', '74 46.100')
)
mmm_vr[, ':='(lat = as.numeric(gsub(' .*', '', lat)) +
                as.numeric(gsub('.* ', '', lat))/60,
              long = -as.numeric(gsub(' .*', '', long)) -
                as.numeric(gsub('.* ', '', long))/60)]

mmm_vr <- st_as_sf(mmm_vr, crs = 4326, coords = c('long', 'lat'))

ggplot() +
  geom_sf(data = zones, aes(fill = zone), show.legend = F) +
  geom_sf(data = coast) +
  geom_sf(data = turbines[grepl('^[JKLMNOPQR]', ID) & location == 'inner'],
          aes(geometry = geom), size = 0) +
  geom_sf(data = mmm_vr, size = 2, color = 'darkgreen') +
  coord_sf(xlim = c(-75.15, -74.40), ylim = c(38.2, 38.5)) +
  scale_fill_manual(values = c('lightblue', NA, NA), na.value = NA) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 35))
