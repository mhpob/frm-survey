# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c('sf', 'dplyr', 'googledrive', 'readxl', 'purrr', 'tidyr',
               'xml2', 'units', 'data.table', 'pdftools', 'ggplot2'), 
  # default storage format
  format = "qs" 
  # Set other options as needed.
)

# tar_make_future() configuration:
plan(multisession,
     workers = availableCores(logical = F))

# Load the R scripts with your custom functions:
for (file in list.files("code/_targets", full.names = TRUE)) source(file)
rm(file)


# Replace the target list below with your own:
list(
  
  # read in rec survey, pot survey, HOBO, or Castaway data from Google Drive
  # https://github.com/ropensci/tarchetypes/discussions/28
  tar_map(
    list(data_source = c('rec', 'pot', 'hobo', 'rec_castaway', 'pot_castaway')),
    
    # Cues will always appear outdated so that they check every time
    # We want the cue to be T if we want to skip it, F if we want to run it
    tar_target(gdrive_cue, find_gdrive_cue(data_source),
               cue = tar_cue('always')),
    
    # After checking, pot/rec downloads will be cancelled if the target
    #   metadata is older than the last time the sheet was edited. HOBO and Castaway
    #   downloads will be cancelled if the file names in data/hobo/ or data/castaway
    #   match those in the USW/FRM/pot/raw data/HOBO or /pot/raw data/castaway or
    #   /rec/raw data/castaway shared drive folders
    tar_skip(raw_data,
             gdrive_download(data_source),
             skip = gdrive_cue,
             cue = tar_cue(mode = "always"),
             format = 'qs'
    )
  ),
  
  # Note for debugging -- you may need to delete a file in order to get it to run
  #   This is especially true if empty objects start being returned.
  tar_target(hobo,
             hobo_clean(
               raw_data_hobo,
               raw_data_pot
             )
  ),
  
  tar_target(castaway,
             castaway_clean(
               raw_data_pot_castaway,
               raw_data_rec_castaway
             )
  ),
  
  tar_target(depth_profiles_pot,
             castaway_viz(
               castaway,
               survey_id = 'pot'
             )
  ),
  # # GPS inputs
  # tar_files_input(gps_dirs,
  #                 grep(
  #                   'RAW/(Pot|Rec)',
  #                   list.dirs('embargo/gps', full.names = T),
  #                   value = T
  #                 )
  # ),
  # 
  # ## tracks
  # tar_target(track_data,
  #            gpx_read_tracks(gps_dirs),
  #            pattern = map(gps_dirs)
  # ),
  # 
  # ## points
  # tar_target(pt_data,
  #            gpx_read_pts(gps_dirs),
  #            pattern = map(gps_dirs)
  # ),
  # 
  # tar_file(pt_key,
  #          gpx_make_pt_key(pt_data)
  #          ),
  # 
  # tar_file(pt_repaired, gpx_repair_pts(pt_data, pt_key)),
  
  
  
  # Geospatial inputs
  tar_file(
    usw_locations,
    'embargo/USWind_PDE-Locations_2022-0111.gpkg'
  ),
  
  tar_file(boem_weas,
           'data/geo/boemwindlayers_4download.gpkg'
  ),
  
  tar_file(update_202211,
           'data/geo/app-i-f-wtg-oss-locations.pdf'
  ),
  
  tar_file(
    usw_buildout,
    add_buildout_zones(boem_weas)
  ),
  
  tar_file(
    frm_control,
    add_control_area(boem_weas)
  ),
  
  tar_file(
    usw_locations_202211,
    wtg_update_202211(update_202211, usw_locations)
  )
  # 
  # tar_quarto(point_process, "reports/point_process.qmd",
  #            cue = tar_cue('never'))
  
  
)
