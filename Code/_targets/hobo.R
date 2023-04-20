hobo_clean <- function(raw_data_hobo, raw_data_pot){
  # library(readxl); library(dplyr); library(purrr); library(tidyr)
  # targets::tar_load('raw_data_hobo'); targets::tar_load('raw_data_pot')
  raw_hobo <- sapply(
    raw_data_hobo,
    read_xlsx,
    sheet = 'Data',
    simplify = F
  )
  
  raw_hobo <- bind_rows(raw_hobo, .id = 'file')
  
  colnames(raw_hobo) <- c('file', 'obs_num', 'datetime_edt', 'bwt_c')
  
  hobo_clean <- raw_hobo |>
    mutate(
      hobo_id = as.numeric(gsub('.*/| 202.*', '', `file`)),
      cruise = gsub('^.*\\d{8} |-\\d{2} .*$', '', `file`),
      datetime_edt = as.POSIXct(as.character(datetime_edt),
                                origin = as.POSIXct('1970-01-01',
                                                    tz = 'America/New_York'),
                                tz = 'America/New_York'))

  pot_station <- raw_data_pot |>
    read_xlsx(sheet = 'Station') |>
    select(-c(notes, old_station)) |>
    mutate(cruise = substr(datetime, 1, 7),
           datetime = as.POSIXct(gsub(':00$', '00', datetime),
                                 format = '%Y-%m-%dT%T %z')) |>

    # "pivot" data to a wider form, renaming columns to refer to
    #   deployment or recovery
    nest(.by = type) |>
    mutate(
      data = map2(
        .x = data,
        .y = c('deploy', 'recover'),
        .f = function(d = .x, nms = .y) {
          # need to make a different function with different variables as
          #     too many anonymous functions confuse purrr
          rename_with(.data = d,
                      #note that this .x refers to d, not data
                      .fn = ~ paste(nms, .x, sep = '_'),
                      .cols = -c(station, hobo_id, cruise))
        }
      )
    )

  pot_station <- pot_station$data |>
    purrr::reduce(full_join, by = c('station', 'hobo_id', 'cruise'))


  hobo_clean <- hobo_clean |>
    inner_join(pot_station,
              by = join_by(
                hobo_id,
                cruise,
                between(datetime_edt, deploy_datetime, recover_datetime))
    )

  hobo_clean |>
    select(cruise, station, hobo_id, datetime_edt, bwt_c, depth = deploy_depth, file)
}
