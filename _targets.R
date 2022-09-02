# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c('sf', 'dplyr', 'googledrive'), 
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
  
  # read in pot and rec survey data from Google Drive
  tar_map(
    list(surveys = c('rec', 'pot')),
    
    # Cues will always appear outdated so that they check every time
    tar_target(gdrive_cue, find_gdrive_cue(surveys),
               format = 'qs',
               cue = tar_cue('always')),
    
    # After checking, the download will be cancelled if the target
    #   metadata is older than the last time the sheet was edited.
    tar_skip(raw_data,
             gdrive_download(surveys),
             skip = gdrive_cue,
             format = 'file',
             cue = tar_cue(mode = "always")
    )
  ),
  # https://github.com/ropensci/tarchetypes/discussions/28
  # tar_target(gdrive_cue, find_gdrive_cue('rec'),
  #            format = 'qs'),
  # tar_skip(rec_survey,
  #          gdrive_download('rec'),
  #          skip = gdrive_cue,
  #          format = 'file'
  # ),
  
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
