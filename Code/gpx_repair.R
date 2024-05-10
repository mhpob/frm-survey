library(sf); library(dplyr); library(xml2); library(mapview)

# https://gdal.org/drivers/vector/gpx.html
# GPX extension parser
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

load_tracks <- function(cruise_id, survey_type){
  lapply(
    list.files(
      paste('embargo/gps',
            cruise_id,
            'raw',
            survey_type,
            sep = '/'),
      full.names = T, pattern = '^Track.*gpx'),
    st_read,
    layer = 'tracks', quiet = T) |>
    bind_rows() |>
    
    # pull out "fit" information
    rowwise() |>
    mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
           gpx_ext_fix(gpxx_TrackExtension)) |> 
    select_at(vars(-starts_with('gpx')))
}

load_points <- function(cruise_id, survey_type){
  lapply(
    list.files(
      paste('embargo/gps',
            cruise_id,
            'raw',
            survey_type,
            sep = '/'),
      pattern = '^Waypoints.*gpx', full.names = T),
    read_sf,
    layer = 'waypoints'
  ) |> 
    bind_rows()
}

create_key <- function(cruise_id, survey_type){
  path <- paste('embargo/gps',
                cruise_id,
                'repaired',
                survey_type,
                paste('key', survey_type, cruise_id, sep = '_'),
                sep = '/')
  path <- paste0(path, '.csv')
  
  if(!file.exists(path)){
    write.csv(data.frame(original_name = pts$name,
                         new_name = '',
                         original_cmt = pts$cmt,
                         new_cmt = '',
                         fid = row.names(pts)),
              path,
              row.names = FALSE, na = '')
    path
  }else{
    cat('File already exists at', path)
    path
  }
}

key_points <- function(key_path){
  pt_key <- read.csv(key_path,
                     na.strings = '') |> 
    mutate(original_name = as.character(original_name))
  
  pts <- pts |> 
    left_join(pt_key, by = c('name' = 'original_name',
                             'cmt' = 'original_cmt'))|> 
    mutate(name = new_name,
           cmt = new_cmt) |> 
    select_at(vars(-starts_with('new'))) |> 
    select(-fid)
  
  st_write(pts, 
           paste(
             dirname(key_path),
             paste0(
               'waypoints_repaired_',
               format(min(pts$time) - 24*60*60, '%Y%m%d'),
               '.gpx'
             ),
             sep = '/'
           ),
           delete_dsn = T
  )
  
  pts
}


# 2404 pot cruise ----
tracks <- load_tracks('2404', 'pot')
pts <- load_points('2404', 'pot')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key('2404', 'pot')
mapview(pts) + mapview(tracks)

## Combine points and key
new_pts <- key_points(key_path)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- new_pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(10, deploy_sites)

recover_sites <- new_pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times (note change in TZ)
deploy_times <- new_pts |> 
  filter(grepl('R1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -05:00'))
writeClipboard(deploy_times$time)

recover_times <- new_pts |> 
  filter(grepl('R1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -05:00'))
writeClipboard(recover_times$time)




# 2311 pot cruise ----
tracks <- load_tracks('2311', 'pot')
pts <- load_points('2311', 'pot')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key('2311', 'pot')
mapview(pts) + mapview(tracks)

## Combine points and key
new_pts <- key_points(key_path)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- new_pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(10, deploy_sites)

recover_sites <- new_pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times (note change in TZ)
deploy_times <- new_pts |> 
  filter(grepl('[B]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -05:00'))
writeClipboard(deploy_times$time)

recover_times <- new_pts |> 
  filter(grepl('[B]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -05:00'))
writeClipboard(recover_times$time)




# 2310-2 pot cruise ----
tracks <- load_tracks('2310_2', 'pot')
pts <- load_points('2310_2', 'pot')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key('2310_2', 'pot')
library(mapview)
mapview(pts) + mapview(tracks)

## Combine points and key
new_pts <- key_points(key_path)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- new_pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(11, deploy_sites)

recover_sites <- new_pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times
deploy_times <- new_pts |> 
  filter(grepl('[B]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- new_pts |> 
  filter(grepl('[B]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time)



# 2310 pot cruise ----
tracks <- load_tracks(2310, 'pot')
pts <- load_points(2310, 'pot')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key(2310, 'pot')
library(mapview)
mapview(pts) + mapview(tracks)

## Combine points and key
new_pts <- key_points(key_path)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- new_pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(11, deploy_sites)

recover_sites <- new_pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times
deploy_times <- new_pts |> 
  filter(grepl('[B]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- new_pts |> 
  filter(grepl('[B]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time)




# 2309 rec cruise ----
# dir.create('embargo/gps/2309/raw/rec', recursive = T)
# dir.create('embargo/gps/2309/repaired/rec', recursive = T)

tracks <- load_tracks(2309, 'rec')
pts <- load_points(2309, 'rec')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key(2309, 'rec')
library(mapview)
mapview(pts) + mapview(tracks)

## Combine points and key
new_pts <- key_points(key_path)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
jigs <- new_pts |> 
  filter(grepl('jig', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(7, jigs)

drops <- new_pts |> 
  filter(grepl('drop', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(3, drops)

## Update times
deploy_times <- new_pts |> 
  filter(grepl('[BR]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- new_pts |> 
  filter(grepl('[BR]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time)





# 2308 pot cruise ----
tracks <- load_tracks(2308, 'pot')
pts <- load_points(2308, 'pot')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key(2308, 'pot')
library(mapview)
mapview(pts) + mapview(tracks)

## Combine points and key
new_pts <- key_points(key_path)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- new_pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(1, deploy_sites)

recover_sites <- new_pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times
deploy_times <- new_pts |> 
  filter(grepl('[BR]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- new_pts |> 
  filter(grepl('[BR]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time)

# 2308 rec cruise ----
tracks <- load_tracks(2308, 'rec')
pts <- load_points(2308, 'rec')

## Create key to rename waypoints then open up the created file
## and edit by hand
key_path <- create_key(2308, 'rec')
library(mapview)
mapview(pts) + mapview(tracks)

## Combine points and key
key_points(key_path)




# 2307 rec cruise ----
## GPS ran out of battery ~1/4 into the first day

# 2307 pot cruise ----
tracks <- load_tracks(2307, 'pot')
pts <- load_points(2307, 'pot')

## Create key to rename waypoints
# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2307/repaired/pot/key_pot_2307.csv',
#           row.names = FALSE, na = '')

## Open up the created file and edit by hand
library(mapview)
mapview(pts)

pt_key <- read.csv('embargo/gps/2307/repaired/pot/key_pot_2307.csv',
                   na.strings = '') |> 
  mutate(original_name = as.character(original_name))

pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new'))) |> 
  select(-fid)

st_write(pts, 'embargo/gps/2307/repaired/pot/waypoints_repaired_20230724.gpx',
         delete_dsn = T)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(11, deploy_sites)

recover_sites <- pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times
deploy_times <- pts |> 
  filter(grepl('[BR]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- pts |> 
  filter(grepl('[BR]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time)

# 2306-2 pot cruise ----
tracks <- load_tracks('2306_2', 'pot')
pts <- load_points('2306_2', 'pot')

## Create key to rename waypoints
# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2306_2/repaired/pot/key_pot_2306_2.csv',
#           row.names = FALSE, na = '')

## Open up the created file and edit by hand
# library(mapview)
# mapview(pts)


pt_key <- read.csv('embargo/gps/2306_2/repaired/pot/key_pot_2306_2.csv',
                   na.strings = '') 
pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new'))) |> 
  select(-fid)

st_write(pts, 'embargo/gps/2306_2/repaired/pot/waypoints_repaired_20230626.gpx',
         delete_dsn = T)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(11, deploy_sites)

recover_sites <- pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(22, recover_sites)

## Update times
deploy_times <- pts |> 
  filter(grepl('[BR]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- pts |> 
  filter(grepl('[BR]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time[c(1, 3:7)])


# 2208 rec cruise ----
tracks <- lapply(
  list.files('embargo/gps/2306/raw/rec', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2306/raw/rec', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = ''),
#           'embargo/gps/2306/repaired/rec/_key_rec_2306.csv',
#           row.names = FALSE, na = '')
# mapview::mapview(pts) + mapview::mapview(tracks)

pt_key <- read.csv('embargo/gps/2306/repaired/rec/key_rec_2306.csv',
                   na.strings = '')

pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new')))

st_write(pts, 'embargo/gps/2306/repaired/rec/waypoints_repaired_20230626.gpx',
         delete_dsn = T)



## update raw data gdrive spreadsheet
track_points <- lapply(
  list.files('embargo/gps/2306/raw/rec', full.names = T, pattern = '^Track.*gpx'), 
  st_read,
  layer = 'track_points',
  quiet = T
)
concat_coords_rec <- function(.) {
  hold <- st_coordinates(
    track_points[[.]][c(1, nrow(track_points[[.]])),]
    )
  writeClipboard(rep(paste(c(hold[1,],'','','', hold[2,]), collapse = ','),
                     times = 3))
}


# 2306 pot cruise ----
tracks <- lapply(
  list.files('embargo/gps/2306/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2306/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

## Create key to rename waypoints
# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2306/repaired/pot/key_pot_2306.csv',
#           row.names = FALSE, na = '')

pt_key <- read.csv('embargo/gps/2306/repaired/pot/key_pot_2306.csv',
                   na.strings = '') 
pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new'))) |> 
  select(-fid)

st_write(pts, 'embargo/gps/2306/repaired/pot/waypoints_repaired_20230605.gpx',
         delete_dsn = T)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- pts |> 
  filter(grepl('[BR][12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(1, deploy_sites)

recover_sites <- pts |> 
  filter(grepl('[BR][12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(11, recover_sites)

## Update times
deploy_times <- pts |> 
  filter(grepl('[BR]1 deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- pts |> 
  filter(grepl('[BR]1 recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time[c(1:2, 4:7)])


# 2305 pot cruise ----
tracks <- lapply(
  list.files('embargo/gps/2305/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2305/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

## Create key to rename waypoints
# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2305/repaired/pot/key_pot_2305.csv',
#           row.names = FALSE, na = '')

pt_key <- read.csv('embargo/gps/2305/repaired/pot/key_pot_2305.csv',
                   na.strings = '') 
pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new'))) |> 
  select(-fid)

st_write(pts, 'embargo/gps/2305/repaired/pot/waypoints_repaired_20230502.gpx',
         delete_dsn = T)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
deploy_sites <- pts |> 
  filter(grepl('B[12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords <- function(ind, data) {
  writeClipboard(paste(c(data[ind,]$coord, data[ind + 1,]$coord),
                       collapse = ','))
}
concat_coords(11, deploy_sites)

recover_sites <- pts |> 
  filter(grepl('B[12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
concat_coords(1, recover_sites)

## Update times
deploy_times <- pts |> 
  filter(grepl('B[1] deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(deploy_times$time)

recover_times <- pts |> 
  filter(grepl('B[2] recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(recover_times$time)

# 2304 pot cruise ----
tracks <- lapply(
  list.files('embargo/gps/2304/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2304/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()


# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2304/repaired/pot/_key_pot_2304.csv',
#           row.names = FALSE, na = '')
# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2304/repaired/pot/key_pot_2304.csv',
#           row.names = FALSE, na = '')
mapview::mapview(pts) + mapview::mapview(tracks)


pt_key <- read.csv('embargo/gps/2304/repaired/pot/key_pot_2304.csv',
                   na.strings = '') 
pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new'))) |> 
  select(-fid)

st_write(pts, 'embargo/gps/2304/repaired/pot/waypoints_repaired_20230412.gpx',
         delete_dsn = T)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
kk <- pts |> 
  filter(grepl('B[12] deploy', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
k <- function(.) {
  writeClipboard(paste(c(kk[.,]$coord, kk[.+1,]$coord), collapse = ','))
}
k(11)

kk <- pts |> 
  filter(grepl('B[12] recover', name)) |> 
  arrange(time, name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)

k(1)

j <- pts |> 
  filter(grepl('B[1] deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(j$time)

j <- pts |> 
  filter(grepl('B[2] recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(j$time)


# 2209 pot cruise ----
tracks <- lapply(
  list.files('embargo/gps/2209/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2209/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()


# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = '',
#                      fid = row.names(pts)),
#           'embargo/gps/2209/repaired/pot/_key_pot_2209.csv',
#           row.names = FALSE, na = '')
mapview::mapview(pts) + mapview::mapview(tracks)

pt_key <- read.csv('embargo/gps/2209/repaired/pot/key_pot_2209.csv',
                   na.strings = '') 
pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new')))

st_write(pts, 'embargo/gps/2209/repaired/pot/waypoints_repaired_20220829.gpx',
         delete_dsn = T)

## update raw data gdrive spreadsheet
##  This copies block 1 and 2 coords to the clipboard, use odd indices
kk <- pts |> 
  filter(grepl('B[12] deploy', name)) |> 
  arrange(name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)
k <- function(.) {
  writeClipboard(paste(c(kk[.,]$coord, kk[.+1,]$coord), collapse = ','))
}

kk <- pts |> 
  filter(grepl('B[12] recover', name)) |> 
  arrange(name) |>
  mutate(coord = st_coordinates(geometry)) |> 
  select(name, coord)

j <- pts |> 
  filter(grepl('B[1] deploy', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(j$time)

j <- pts |> 
  filter(grepl('B[2] recover', name)) |> 
  arrange(time) |>
  select(name, time) |> 
  mutate(time = format(time, '%Y-%m-%dT%H:%M:%S -04:00'))
writeClipboard(j$time)

# 2208 rec cruise ----
tracks <- lapply(
  list.files('embargo/gps/2208/raw/rec', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2208/raw/rec', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = ''),
#           'embargo/gps/2208/repaired/rec/_key_rec_2208.csv',
#           row.names = FALSE, na = '')
# mapview::mapview(pts) + mapview::mapview(tracks)

pt_key <- read.csv('embargo/gps/2208/repaired/rec/key_rec_2208.csv',
                   na.strings = '') |> 
  filter(new_name != 'ERR')

pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new')))

st_write(pts, 'embargo/gps/2208/repaired/rec/waypoints_repaired_20220815.gpx',
         delete_dsn = T)



## update raw data gdrive spreadsheet
kk <- lapply(list.files('embargo/gps/2208/raw/rec', full.names = T, pattern = '^Track.*gpx'), 
             st_read, layer = 'track_points', quiet = T)
k <- function(.) {
  hold <- st_coordinates(kk[[.]][c(1, nrow(kk[[.]])),])
  writeClipboard(rep(paste(c(hold[1,],'','','', hold[2,]), collapse = ','),
                     times = 3))
}







# 2208 pot cruise ----
tracks <- lapply(
  list.files('embargo/gps/2208/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('embargo/gps/2208/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

# write.csv(data.frame(original_name = pts$name,
#                      new_name = '',
#                      original_cmt = pts$cmt,
#                      new_cmt = ''),
#           'data/gps/2208/repaired/pot/_key_pot_2208.csv',
#           row.names = FALSE, na = '')
# mapview::mapview(pts) + mapview::mapview(tracks)

pt_key <- read.csv('embargo/gps/2208/repaired/pot/key_pot_2208.csv',
                   na.strings = '') |> 
  filter(new_name != 'ERR')

pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new')))

st_write(pts, 'embargo/gps/2208/repaired/pot/waypoints_repaired_20220801.gpx',
         delete_dsn = T)







kk <- lapply(list.files('data/gps/2208/raw/pot', full.names = T, pattern = '^Track.*gpx'), 
             st_read, layer = 'track_points', quiet = T)


# 2207 Pot cruise ----
tracks <- lapply(
  list.files('data/gps/2207/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))


pts <- lapply(
  list.files('data/gps/2207/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

pt_key <- read.csv('data/gps/2207/repaired/pot/key_pot_2207.csv',
                   na.strings = '')

pts <- pts |> 
  left_join(pt_key, by = c('name' = 'original_name', 'cmt' = 'original_cmt'))|> 
  mutate(name = new_name,
         cmt = new_cmt) |> 
  select_at(vars(-starts_with('new')))

st_write(pts, 'data/gps/2207/repaired/pot/waypoints_repaired_20220718.gpx',
         delete_dsn = T)




### Find transit speed
library(sf); library(data.table)
track_pts <- st_read(list.files('data/gps/2207/raw/pot', full.names = T, pattern = '^Track.*gpx')[1],
                     layer = 'track_points', quiet = T)

setDT(track_pts)


track_pts[, lag_pts := shift(st_as_text(geometry))]
track_pts[, lag_dt := shift(time)]


track_pts <- track_pts[!is.na(lag_pts)]
track_pts[, lag_pts := st_as_sfc(lag_pts, crs = 4326)]
track_pts[, dist := st_distance(geometry, lag_pts, by_element = T)]
track_pts[, dt := difftime(time, lag_dt, 'secs')]

track_pts[, speed := dist/units::set_units(as.numeric(dt), 'seconds')]

track_pts[, spd_kts := units::set_units(speed, 'knots')]


k <- lapply(list.files('data/gps/2207/raw/pot', full.names = T, pattern = '^Track.*gpx'), 
            st_read, layer = 'track_points', quiet = T)



# 0622 Pot cruise ----
pts <- lapply(
  list.files('data/gps 2206/raw/pot', pattern = '^Waypoints.*gpx', full.names = T),
  read_sf,
  layer = 'waypoints'
) |> 
  bind_rows()

pts <- pts |> 
  mutate(name = c('C0622_1',
                  'C0622_2',
                  'A-7-6',
                  'A-9-7',
                  'C0622_1 block1 deploy',
                  'C0622_1 first pot',
                  'C0622_1 last pot',
                  'C0622_1 block2 deploy',
                  'C0622_2 block1 deploy',
                  'C0622_2 first pot', #10
                  'C0622_2 last pot',
                  'ERR',
                  'C0622_2 block2 deploy',
                  'E0622_1 block1 deploy',
                  'E0622_1 first pot',
                  'E0622_1 last pot',
                  'E0622_1 block2 deploy',
                  'E0622_2 first pot',
                  'E0622_2 last pot',
                  'E0622_2 block2 deploy', #20
                  'Integrity pots',
                  'C0622_1 block2 recover',
                  'C0622_1 block1 recover',
                  'C0622_2 block2 recover',
                  'C0622_2 block1 recover',
                  'E0622_1 block2 recover',
                  'E0622_1 block1 recover',
                  'E0622_2 block2 recover'),
         cmt = c('Target',
                 'Target',
                 'Target. E0622_1.',
                 'Target. E0622_2.',
                 rep(NA, 9),
                 rep('A-7-6', 4),
                 rep('A-9-7', 3),
                 rep(NA, 5),
                 rep('A-7-6', 2),
                 'A-9-6'))

# st_write(pts, 'data/gps 2206/repaired/pot/waypoints_repaired_20220627.gpx')


tracks <- lapply(
  list.files('data/gps 2206/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'tracks', quiet = T) |>
  bind_rows() |>
  
  # pull out "fit" information
  rowwise() |>
  mutate(gpx_ext_fix(gpxtrkx_TrackStatsExtension),
         gpx_ext_fix(gpxx_TrackExtension)) |> 
  select_at(vars(-starts_with('gpx')))

st_write(tracks, 'data/gps 2206/repaired/pot/tracks_repaired_20220627_4.gpx',
         layer = 'tracks',
         dataset_options =
           c('GPX_USE_EXTENSIONS=YES',
             'GPX_EXTENSIONS_NS="gpxtpx"',
             'GPX_EXTENSIONS_NS_URL="http://www.garmin.com/xmlschemas/TrackPointExtensionv1.xsd"'))

k <- st_read('data/gps/2206/repaired/pot/tracks_repaired_20220627_3.gpx',
             layer = 'tracks')

# data.frame(tracks)[1, 'gpxx_TrackExtension'] |> 
#   read_html() |> 
#   xml_contents() |> 
#   xml_children() |> 



k <- gsub('gpxx:', '', k) |> 
  read_xml() |>
  xml_contents() |> 
  as.character()

track_pts <- lapply(
  list.files('data/gps 2206/raw/pot', full.names = T, pattern = '^Track.*gpx'),
  st_read,
  layer = 'track_points', quiet = T) |>
  bind_rows() 



### Find transit speed
library(sf); library(data.table)
tracks <- st_read(list.files('data/gps 2206/raw/pot', full.names = T, pattern = '^Track.*gpx')[1],
                  layer = 'track_points', quiet = T)

setDT(tracks)


tracks[, lag_pts := shift(st_as_text(geometry))]
tracks[, lag_dt := shift(time)]


tracks <- tracks[!is.na(lag_pts)]
tracks[, lag_pts := st_as_sfc(lag_pts, crs = 4326)]
tracks[, dist := st_distance(geometry, lag_pts, by_element = T)]
tracks[, dt := difftime(time, lag_dt, 'secs')]

tracks[, speed := dist/units::set_units(as.numeric(dt), 'seconds')]

tracks[, spd_kts := units::set_units(speed, 'knots')]





