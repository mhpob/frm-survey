
castaway_viz <- function(castaway_data,
                         survey_id = c('pot', 'rec'),
                         variables = c('temperature', 'salinity',
                                       'sound_velocity')){
  cruise_data <- castaway_data[survey %in% survey_id]
  cruise_data <- melt(
    cruise_data, 
    id.vars = c('survey', 'cruise', 'deploy_date_time', 'depth')
  )
  cruise_data <- cruise_data[
    variable %in% variables
  ]
  
  ggplot(data = cruise_data) +
    geom_path(aes(x = as.numeric(value), y = as.numeric(depth),
                  group = factor(deploy_date_time),
                  color = cruise)) +
    labs(x = 'Value', y = 'Depth (m)', color = 'Survey') +
    facet_wrap(~variable, scales = 'free_x') +
    scale_color_viridis_d(option = 'H') +
    scale_y_reverse() +
    theme_minimal()
}

#' tar_load(hobo)
#' setDT(hobo)
#' hobo[, cruise := ifelse(month(datetime_edt) == 6 & datetime_edt > '2023-06-15',
#'                       '2023-06 (2)', cruise)]
#' hobo[, cruise := factor(cruise, ordered = T, 
#'                         levels = c('2023-04', '2023-05', '2023-06',
#'                                    '2023-06 (2)', '2023-07'))]
#' 
#' 
#' ggplot(data = hobo) +
#'   geom_line(aes(x = datetime_edt, y = bwt_c, group = station)) +
#'   facet_wrap(~cruise, scales = 'free_x') +
#'   scale_x_datetime(breaks = '6 hours', date_labels = '%H:%M') +
#'   labs(x = '', y = 'BWT (C)') +
#'   theme_minimal()
#' 
#' vue <- fread('embargo/mmm_act/vue_export.csv')
#' vue <- vue[Description %in% c('Temperature'#, 
#'                               #'Average noise'
#'                               ) &
#'              `Date and Time (UTC)` %between% c('2023-04-20', '2023-10-11')]
#' vue[, Data := as.numeric(Data)]
#' 
#' 
#' ggplot(data = vue) +
#'   geom_vline(xintercept = as.POSIXct(c('2023-05-02', '2023-06-06', '2023-06-27',
#'                                        '2023-07-25', '2023-08-15', '2023-08-22',
#'                                        '2023-09-06', '2023-10-03'))) +
#'   geom_vline(xintercept = as.POSIXct('2023-09-23'), color = 'red', lty = 'dashed') +
#'   geom_line(aes(x = `Date and Time (UTC)`, y = Data, color = Receiver),
#'             show.legend = F) +
#'   labs(x = '', y = 'BWT (C)') +
#'   theme_minimal()
