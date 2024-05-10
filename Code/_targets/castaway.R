castaway_clean <- function(pot_data, rec_data){
# library(data.table)
# targets::tar_load(raw_data_rec_castaway)
# targets::tar_load(raw_data_pot_castaway)


  ctd_import <- function(x) {
    
    ctd <- sapply(
      x,
      fread,
      col.names = function(.) tolower(gsub(' \\(.*', '', .)),
      simplify = F, USE.NAMES = T,
      skip = "Pressure") |>
      rbindlist(idcol = 'file_name')

    setnames(ctd, function(.) gsub(' ', '_', .))

    ctd[, deploy_date_time := sub('^.*_(.*_.*?)\\.csv$', '\\1', file_name)]
    ctd[, deploy_date_time := as.POSIXct(deploy_date_time,
                                         format = '%Y%m%d_%H%M%S',
                                         tz = 'UTC')]
    ctd[, ':='(survey = sub('(?:.*/){2}(.*?)/.*$', '\\1', file_name),
               cruise = sub('.*_20(\\d{4}?).*csv', '\\1', file_name))]
    ctd[, ':='(pressure = set_units(pressure, dbar),
               depth = set_units(depth, m),
               temperature = set_units(temperature, degree_Celsius),
               conductivity = set_units(conductivity, uS/cm),
               specific_conductance = set_units(specific_conductance, uS/cm),
               salinity = set_units(salinity, ppt),
               sound_velocity = set_units(sound_velocity, m/s),
               density = set_units(density, kg/m^3),
               file_name = NULL)]

    setcolorder(ctd, c('survey', 'cruise', 'deploy_date_time'))

    ctd
  }
  
  rbind(
    ctd_import(pot_data),
    ctd_import(rec_data)
  )


}
