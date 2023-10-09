castaway_clean <- function(raw_data_rec_castaway, raw_data_pot_castaway){
  # library(data.table)
  # targets::tar_load(raw_data_rec_castaway)
  # targets::tar_load(raw_data_pot_castaway)
  
  ctd_import <- function(x) {
    ctd <- sapply(
      x,
      fread,
      col.names = function(.) tolower(gsub(' \\(.*', '', .)),
      simplify = F, USE.NAMES = T)
    ctd <- rbindlist(ctd, idcol = 'file')
    setnames(ctd, function(.) gsub(' ', '_', .))
    ctd[, deploy_date_time := sub('^.*_(.*_.*?)\\.csv$', '\\1', file)]
    ctd[, deploy_date_time := as.POSIXct(deploy_date_time,
                                         format = '%Y%m%d_%H%M%S',
                                         tz = 'UTC')]
    ctd[, ':='(survey = sub('(?:.*/){2}(.*?)/.*$', '\\1', file),
               cruise = sub('.*_20(\\d{4}?).*csv', '\\1', file),
               file = NULL)]
    ctd[, ':='(pressure = set_units(pressure, dbar),
               depth = set_units(depth, m),
               temperature = set_units(temperature, degree_Celsius),
               conductivity = set_units(conductivity, uS/cm),
               specific_conductance = set_units(specific_conductance, uS/cm),
               salinity = set_units(salinity, ppt),
               sound_velocity = set_units(sound_velocity, m/s),
               density = set_units(density, kg/m^3))]
    
    setcolorder(ctd, c('survey', 'cruise', 'deploy_date_time'))
  }
  
  rbind(
    ctd_import(raw_data_pot_castaway),
    ctd_import(raw_data_rec_castaway)
  )
  
  
}
