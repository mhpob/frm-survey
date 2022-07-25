# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(future)

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c('sf', 'dplyr'), 
  # default storage format
  format = "feather" 
  # Set other options as needed.
)

# tar_make_future() configuration:
plan(multisession,
     workers = availableCores(logical = F))

# Load the R scripts with your custom functions:
for (file in list.files("_targets_code", full.names = TRUE)) source(file)
rm(file)


# Replace the target list below with your own:
list(
  tar_target(wea_raw,
    'data/geo/offshore wind layers.gdb',
    format = 'file'
  ),
  
  tar_target(
    wea_buildout,
    add_buildout_zones(wea_raw),
    format = "file" 
  ),
  
  tar_target(
    wea_spatial,
    add_control_area(wea_buildout),
    format = "file" 
  ),
  
  
  tar_target(
    towers,
    tower_stations(),
    format = 'qs'
  )
)
