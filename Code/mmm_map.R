library(sf)
library(dplyr)

# Read in development zones
zones <- st_read('data/geo/usw_buildout_zones.gpkg',
                 quiet = T)

# Read in MMM sites
mmm <- read.csv('data/geo/MD_PAM_AllStations_BOEMFinal_D1_2.csv',
                na.strings = '')|> 
  # convert to sf
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) |> 
  filter(
    # Pick MARU, CPOD, FPOD sites
    Device %in% c('MARU', 'C-POD', 'FPOD'),
    # Drop T-3 and A-8
    !Site %in% c('T-3', 'A-8')
  )

# Add VR2AR sites 4km to the W and E of A-5C
vr2ar <- mmm |> 
  # Pull out A5C
  filter(StationName == 'A-5C') |> 
  # Convert to meter-based projection
  st_transform(6487) |> 
  # Add and subtract 4000m from the easting
  st_geometry() + list(c(4000, 0), c(-4000, 0)) 
# re-apply projecction
st_crs(vr2ar) <- 6487
# convert back to to WGS84
vr2ar <- st_transform(vr2ar, 4326)
vr2ar <- st_as_sf(vr2ar)
# Add names
vr2ar$StationName <- c('V4', 'V5')
vr2ar$Device <- 'VR2AR'
vr2ar$geometry <- vr2ar$x
vr2ar$x <- NULL

# Merge sites
mmm <- bind_rows(mmm, vr2ar)


# Add extra FPOD site similar to the above.
#  This is hacky and being called CPOD to get the colors right
fpod <- mmm |> 
  filter(StationName == 'T-3*M') |> 
  st_transform(6487) |> 
  st_geometry() + list(c(-2500, 0)) 
st_crs(fpod) <- 6487
fpod <- st_transform(fpod, 4326)
fpod <- st_as_sf(fpod)
fpod$StationName <- 'T-3*C'
fpod$Device <- 'C-POD'
fpod$geometry <- fpod$x
fpod$x <- NULL

mmm <- bind_rows(mmm, fpod)

# Rename devices
mmm <- mmm |> 
  mutate(Device = case_when(Device == 'MARU' ~ 'Rockhopper',
                            Device == 'C-POD' ~ 'FPOD + VR2AR',
                            T ~ Device)
  )

# load in spatial files and convert to WGS84
md_coast <- read_sf('data/geo/matl_states.gpkg') %>%
  filter(!is.na(STATE_NAME)) %>%
  st_transform(4326)

atl_coast <- read_sf('data/geo/ne_10m_coastline.gpkg')%>%
  st_transform(4326)

bathy <- read_sf('data/geo/bathymetry_midatl.gpkg',
                 # don't keep all of the unneeded countours
                 query = 'select * from bathymetry_midatl where elev_m > -50') %>%
  st_transform(4326) %>%
  filter(as.numeric(st_length(.))> 30000) %>%
  st_crop(xmin = -75.15, xmax = -74.3, ymin = 38.2, ymax = 38.5)

library(ggspatial)
library(ragg)
library(ggplot2)

main <-
  ggplot() +
  geom_sf(data = bathy, color = 'gray', linewidth = 0.1) +
  # add bathymetry labels
  geom_label(data = data.frame(
    lab = c('10', '20', '30', '40'),
    x = c(-75.1, -74.838, -74.695, -74.59),
    y = c(38.27, 38.425, 38.425, 38.25)
  ),
  aes(x = x, y = y, label = lab),
  label.size = 0, label.padding = unit(0.1, "lines"), size = 1) +
  geom_sf(data = md_coast, size = 0.1) +
  geom_sf(data = zones, aes(fill = zone), alpha = 0.8,
          show.legend = F) +
  geom_sf(data = mmm, aes(color = Device),
          size = 3, alpha = 0.8) +
  scale_fill_manual(values = c('pink', NA, NA),
                    na.value = NA) +
  scale_color_manual(values = c('green', 'red', 'blue', 'black')) +
  coord_sf(xlim = c(-75.15, -74.3),
           ylim = c(38.2, 38.5),
           expand = F) +
  # add scalebar via ggspatial::annotation_scale
  annotation_scale(text_cex = 0.5, height = unit(1, 'mm')) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), 'mm'),
        panel.background = element_rect(fill = 'lightblue'),
        legend.position = c(0.85, 0.8),
        legend.key = element_blank(),
        legend.key.size = unit(0.1, 'mm'),
        legend.text = element_text(size = 12 / .pt),
        legend.background = element_blank(),
        legend.box.background = element_blank())

# Add inset
inset <- ggplotGrob(
  ggplot() +
    geom_sf(data = atl_coast, linewidth = 0.1) +
    coord_sf(xlim = c(-77.5, -69.5),
             ylim = c(35.5, 43)) +
    annotate('rect', xmin = -75.15, xmax = -74.3, ymin = 38.2, ymax = 38.5,
             fill = NA, color = 'red', size  = 0.1)+
    theme_void() +
    theme(panel.background = element_rect(fill = 'white'))
)


# save map
agg_tiff("map.png",
         width = 85, height = 39, units = 'mm',
         compression = 'lzw', res = 600)


main +
  annotation_custom(inset, xmin = -74.4, xmax = -74.3,
                    ymin = 38.2, ymax = 38.35)


dev.off()

