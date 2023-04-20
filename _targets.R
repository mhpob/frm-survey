# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c('sf', 'dplyr', 'googledrive', 'readxl', 'purrr', 'tidyr'), 
  # default storage format
  format = "feather" 
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
  
  # read in rec survey, pot survey, or HOBO data from Google Drive
  # https://github.com/ropensci/tarchetypes/discussions/28
  tar_map(
    list(data_source = c('rec', 'pot', 'hobo')),
    
    # Cues will always appear outdated so that they check every time
    # We want the cue to be T if we want to skip it, F if we want to run it
    tar_target(gdrive_cue, find_gdrive_cue(data_source),
               format = 'qs',
               cue = tar_cue('always')),
    
    # After checking, pot/rec downloads will be cancelled if the target
    #   metadata is older than the last time the sheet was edited. HOBO
    #   downloads will be cancelled if the file names in data/hobo/ match those
    #   in the USW/FRM/pot/raw data/HOBO shared drive folder
    tar_skip(raw_data,
             gdrive_download(data_source),
             skip = gdrive_cue,
             format = 'file',
             cue = tar_cue(mode = "always")
    )
  ),
  
  tar_target(hobo,
             hobo_clean(
               tar_read(raw_data_hobo),
               tar_read(raw_data_pot)
             ),
  ),

  
  # Geospatial inputs
  tar_file(
    usw_locations,
    'embargo/USWind_PDE-Locations_2022-0111.gpkg'
  ),
  
  tar_file(boem_weas,
    'data/geo/boemwindlayers_4download.gpkg'
  ),
  
  tar_file(
    usw_buildout,
    add_buildout_zones(boem_weas)
  ),
  
  tar_file(
    frm_control,
    add_control_area(boem_weas)
  ),
  
  
  tar_quarto(point_process, "reports/point_process.qmd",
             cue = tar_cue('never'))

  
)
