# 
add_buildout_zones <- function(boem_weas){
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
  

  wea <- st_read(boem_weas,
                 query = "select * from BOEMWindLeases_6_30_2022 where State = 'Maryland'",
                 quiet = T)
  # wea <- st_transform(wea, 4326)
  wea <- st_buffer(wea, 1)
  wea_split <- st_union(wea)
  wea_split <- st_split(wea_split, st_geometry(dev_lines))
  wea_split <- st_collection_extract(wea_split)
  wea_split <- st_as_sf(wea_split)
  wea_split$zone <- c('C', 'B', 'A')
  
  st_write(wea_split,
           'data/geo/usw_buildout_zones.gpkg',
           delete_layer = TRUE)
  
  'data/geo/usw_buildout_zones.gpkg'
}



add_control_area <- function(boem_weas){
  
  wea <- st_read(boem_weas,
                 query = "select* from BOEMWindLeases_6_30_2022 where State = 'Maryland'")
  
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
  
  # Create a polygon representing the "cable pile"; a "fish haven" in the NOAA charts
  cable_pile <- st_polygon(
    list(
      matrix(
        c(
          -74.741652100,38.200013200,
          -74.741699700,38.216638800,
          -74.721753200,38.216601400,
          -74.721753200,38.200013201,
          -74.741652100,38.200013200
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  ) |> 
    st_sfc(crs = 4326)
  
  # Remove cable pile from the control area
  control <- st_difference(control, cable_pile)
  
  st_write(control,
           'data/geo/frm_control.gpkg',
           delete_layer = TRUE)
  
  'data/geo/frm_control.gpkg'
  
}


wtg_update_202211 <- function(update_202211, turbines){
  # library(pdftools)
  old_turbines <- st_read(
    turbines,
    layer = 'WTG_Layout_2022_0111', quiet = TRUE
    )

  txt <- pdf_text(update_202211)
  
  locations <- lapply(txt, fread) |>
    lapply(function(.) .[, 1:5]) |> 
    lapply(function(.) .[complete.cases(.)]) |> 
    rbindlist(use.names = FALSE)
  
  setnames(locations,
           c('name', 'former_name', 'easting', 'northing', 'depth'))
  
  locations <- merge(locations, old_turbines,
                     by.x = 'former_name', by.y = 'ID',
                     suffixes = c('.202211'))
  
  st_write(locations,
           'data/geo/wtg_locations_202211.gpkg',
           delete_layer = TRUE)
  
  'data/geo/wtg_locations_202211.gpkg'
}


