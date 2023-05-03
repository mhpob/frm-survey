

gpx_ext_fix <- function(x){
  base <- x |> 
    as.character() |> 
    # Tell R that this is XML
    # Side-effect of dropping the prepended "gpxtrkx:" and making everything lowercase
    read_html() |>  
    # Pull out the main nodes
    xml_contents() |> 
    # Pull out the children of the nodes
    xml_children()
  
  conts <- base |> 
    # Pull out the values
    xml_text() |> 
    # Make columns instead of rows
    t() |> 
    # Convert to data frame
    data.frame() 
  
  # Rename columns
  names(conts) <- xml_name(base)
  
  conts
}

gpx_read_tracks <- function(gps_dirs){
  # library(sf)
  tracks <- list.files(gps_dirs, full.names = T, pattern = 'Track.*gpx')
  
  tracks <- lapply(
    tracks,
    st_read,
    layer = 'tracks', quiet = T) |>
    bind_rows() |>
    # pull out "fit" information
    data.frame() |> 
    rowwise() |>
    mutate(
      # targets doesn't play nicely with sf data, and likely never will.
      #   https://github.com/ropensci/targets/discussions/713
      #   convert to df above and geom column to text; will need to convert to spatial later
      geometry = st_as_text(geometry),
      across(
        # select columns that have XML (as indicated by tags)
        where(
          function(.) any(grepl('<.*>', .))
        ),
        ~ gpx_ext_fix(.x),
        .unpack = '{inner}'
      )
    ) |>
    # remove the XML columns
    select(
      where(
        ~ !any(grepl('<.*>', .x))
      )
    ) |> 
    ungroup()
  
  # One cruise used GAIA GPS: fix the color name
  if('color' %in% names(tracks)){
    names(tracks)[names(tracks) == 'color'] <- 'displaycolor'
    tracks[, c('distance', 'timertime', 'totalelapsedtime', 'movingspeed',
               'maxelevation', 'minelevation', 'ascent',
               'descent', 'calories')] <- NA
  }
  
  tracks
  
}


gpx_read_pts <- function(gps_dirs){
  pts <- list.files(gps_dirs, pattern = '^Waypoints.*gpx', full.names = T)
  
  pts <- lapply(
    pts,
    read_sf,
    layer = 'waypoints'
  ) |>
    bind_rows()
}

gpx_make_pt_key <- function(){
  write.csv(data.frame(original_name = pts$name,
                       new_name = '',
                       original_cmt = pts$cmt,
                       new_cmt = '',
                       fid = row.names(pts)),
            'embargo/gps/2304/repaired/pot/_key_pot_2304.csv',
            row.names = FALSE, na = '')
}

# mapview::mapview(pts) + mapview::mapview(tracks)

gpx_repair_pts <- function(){
  # pt_key <- read.csv('embargo/gps/2304/repaired/pot/key_pot_2304.csv',
  #                    na.strings = '') 
  # pts <- pts |> 
  #   left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  #   mutate(name = new_name,
  #          cmt = new_cmt) |> 
  #   select_at(vars(-starts_with('new'))) |> 
  #   select(-fid)
  # 
  # st_write(pts, 'embargo/gps/2304/repaired/pot/waypoints_repaired_20230412.gpx',
  #          delete_dsn = T)
}

