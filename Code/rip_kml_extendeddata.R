library(rvest)
library(data.table)
library(sf)


# WTG fix ----
values <- read_html('wtg_layout_2022_0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_text()

key <- read_html('wtg_layout_2022_0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_element(xpath = '@name') |> 
  html_text() 

pin_coords <- read_html('wtg_layout_2022_0111.kml') |> 
  html_elements(xpath = 'body/kml/document/folder/placemark/point')|> 
  html_text()

ext_data <- data.table(key, values)

ext_data[, grp := rleid(key == 'ID')]
ext_data[, grp := fifelse(key == 'ID', grp + 1, grp)]

ext_data <- dcast(ext_data, grp ~ key, value.var = 'values')
ext_data[, names(ext_data) := lapply(.SD, type.convert, as.is = T),
          .SDcols = names(ext_data)]
ext_data[, grp := NULL]

ext_data[, ':='(pin_lon = tstrsplit(pin_coords, ',')[[1]],
                pin_lat = tstrsplit(pin_coords, ',')[[2]])]

ext_data_sf <- ext_data |> 
  st_as_sf(coords = c('pin_lon', 'pin_lat'),
           crs = 4326)

dir.create('WTG_Layout_2022_0111')

write_sf(ext_data_sf, 'WTG_Layout_2022_0111/WTG_Layout_2022_0111.shp')
write_sf(ext_data_sf, 'USWind_PDE-Locations_2022-0111.gpkg',
         layer = 'WTG_Layout_2022_0111')

# OSS fix ----
values <- read_html('oss_locations_2022_0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_text()

key <- read_html('oss_locations_2022_0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_element(xpath = '@name') |> 
  html_text() 

pin_coords <- read_html('oss_locations_2022_0111.kml') |> 
  html_elements(xpath = 'body/kml/document/folder/placemark/point')|> 
  html_text()

ext_data <- data.table(key, values)

ext_data[, grp := rleid(key == 'ID')]
ext_data[, grp := fifelse(key == 'ID', grp + 1, grp)]

ext_data <- dcast(ext_data, grp ~ key, value.var = 'values')
ext_data[, names(ext_data) := lapply(.SD, type.convert, as.is = T),
         .SDcols = names(ext_data)]
ext_data[, grp := NULL]

ext_data[, ':='(pin_lon = tstrsplit(pin_coords, ',')[[1]],
                pin_lat = tstrsplit(pin_coords, ',')[[2]])]

ext_data_sf <- ext_data |> 
  st_as_sf(coords = c('pin_lon', 'pin_lat'),
           crs = 4326)

dir.create('OSS_Locations_2022_0111')
write_sf(ext_data_sf, 'OSS_Locations_2022_0111/OSS_Locations_2022_0111.shp')
write_sf(ext_data_sf, 'USWind_PDE-Locations_2022-0111.gpkg',
         layer = 'OSS_Locations_2022_0111')

# MetTower fix ----
values <- read_html('mettower_locations_2022-0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_text()

key <- read_html('mettower_locations_2022-0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_element(xpath = '@name') |> 
  html_text() 

pin_coords <- read_html('mettower_locations_2022-0111.kml') |> 
  html_elements(xpath = 'body/kml/document/folder/placemark/point')|> 
  html_text()
pin_names <- read_html('mettower_locations_2022-0111.kml') |>
  html_elements(xpath = 'body/kml/document/folder/placemark/name') |> 
  html_text()

ext_data <- data.table(key, values)

ext_data[, grp := rleid(key == 'id')]
ext_data[, grp := fifelse(key == 'id', grp + 1, grp)]

ext_data <- dcast(ext_data, grp ~ key, value.var = 'values')
ext_data[, names(ext_data) := lapply(.SD, type.convert, as.is = T),
         .SDcols = names(ext_data)]
ext_data[, grp := NULL]

ext_data[, ':='(pin_lon = tstrsplit(pin_coords, ',')[[1]],
                pin_lat = tstrsplit(pin_coords, ',')[[2]],
                name = pin_names)]

ext_data_sf <- ext_data |> 
  st_as_sf(coords = c('pin_lon', 'pin_lat'),
           crs = 4326)

dir.create('MetTower_locations_2022-0111')
write_sf(ext_data_sf, 'MetTower_locations_2022-0111/MetTower_locations_2022-0111.shp')
write_sf(ext_data_sf, 'USWind_PDE-Locations_2022-0111.gpkg',
         layer = 'MetTower_locations_2022-0111')
