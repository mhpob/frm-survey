library(rvest)
library(data.table)
library(sf)

values <- read_html('c:/users/darpa2/downloads/wtg_layout_2022_0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_text()

key <- read_html('c:/users/darpa2/downloads/wtg_layout_2022_0111.kml') |> 
  html_elements(xpath = '//simpledata') |> 
  html_element(xpath = '@name') |> 
  html_text() 

ext_data <- data.table(key, values)

ext_data[, grp := rleid(key == 'ID')]
ext_data[, grp := fifelse(key == 'ID', grp + 1, grp)]

ext_data <- dcast(ext_data, grp ~ key, value.var = 'values')
ext_data[, names(ext_data) := lapply(.SD, type.convert, as.is = T),
          .SDcols = names(ext_data)]
ext_data[, grp := NULL]

ext_data[, c('Lat', 'Long') := NULL]

ext_data_sf <- ext_data |> 
  st_as_sf(coords = c('Lon2', 'Lat2'),
           crs = 4326)

write_sf(ext_data_sf, 'WTG_layout_2022_0111.shp')
