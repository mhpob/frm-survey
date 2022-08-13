# 
add_buildout_zones <- function(wea_spatial){
  library(lwgeom)
  
  # Development sequence lines
  ab_line <- st_linestring(c(
    st_point(c(-74.706408, 38.456837)),
    st_point(c(-74.746202, 38.209318))
  ))
  
  bc_line <- st_linestring(c(
    st_point(c(-74.762416, 38.512965)),
    st_point(c(-74.809502, 38.218600))
  ))
  dev_lines <-  st_as_sf(st_sfc(ab_line, bc_line, crs = 4326))
  

  wea <- st_read(wea_spatial,
                 query = "select * from BOEMWindLeases_6_30_2022 where State = 'Maryland'",
                 quiet = T)
  # wea <- st_transform(wea, 4326)
  wea <- st_buffer(wea, 1)
  wea_split <- st_union(wea)
  wea_split <- st_split(wea_split, st_geometry(dev_lines))
  wea_split <- st_collection_extract(wea_split)
  wea_split <- st_as_sf(wea_split)
  wea_split$zone <- c('A', 'B', 'C')
  
  st_write(wea,
    'data/geo/wea.gpkg',
    layer = 'lease blocks',
    append = F)
  st_write(wea_split,
           'data/geo/wea.gpkg',
           layer = 'buildout zones',
           append = F)
  
  'data/geo/wea.gpkg'
}

add_control_area <- function(wea){
  
  wea <- st_read(wea, layer = 'lease blocks')
  
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
  
  st_write(control,
           'data/geo/wea.gpkg',
           layer = 'control area',
           append = F)
  
  'data/geo/wea.gpkg'
  
}


tower_stations <- function(){
  towers <- st_read('embargo/USWind_PDE-Locations_2022-0111.kml',
                    layer = 'WTG_Layout_2022_0111', 
                    fid_column_name = 'fid',
                    quiet = T) |> 
    st_zm()
  
  codes <- read.csv('data/geo/station_coding_key.csv') |> 
    mutate(fid = as.character(fid))
  
  towers <- left_join(towers, codes) |> 
    select(station)
  
  towers
}
