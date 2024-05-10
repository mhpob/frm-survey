find_gdrive_cue <- function(data_source){
  
  id <- switch(data_source,
               rec = '1OsvJwdYRc8Wf9Z-nkF4t3SCQQ89CEWe9a1CCtPRMp7w',
               pot = '1I3zFvMrVjOnMxZ21BSTRVqW7hKbt1qOpDARenosknUM',
               hobo = '1-DOmuSt-ErQeD2H463dFWV5KvtaClHNx',
               rec_castaway = '1_Y_VmelDBG0KY9F9ECMibPNCglaAp-2a',
               pot_castaway = '1n4Ed7XE8K9e9Nyiuk4jVrzv3JCQUznQp')
  
  if(Sys.info()['nodename'] == 'acadnet-delphinus'){
    googledrive::drive_auth(token = readRDS(".secrets/MOBtoken"))
  }else{
    drive_auth(email = 'obrien@umces.edu')
  }
  
  if(data_source %in% c('rec', 'pot')){
    gd_update <- as.POSIXct(
      drive_get(
        as_id(id)
      )$drive_resource[[1]]$modifiedTime,
      tz = 'UTC', format = '%Y-%m-%dT%H:%M:%OSZ'
    )
    
    survey_dir <- ifelse(data_source == 'rec', 'recreational', data_source)
    targets_update <- file.mtime(
      paste0('embargo/', survey_dir, '_data/',
             data_source,' survey recorded data.xlsx')
    )
    
    # targets_update <- tar_meta()[tar_meta()$name == paste('gdrive_cue', data_source, sep = '_'),]$time
    
    # if not a branch:
    # targets_update <- tar_meta()[tar_meta()$name == 'gdrive_cue',]$time
    
    
    if(length(targets_update) == 0 | is.na(targets_update)){
      targets_update <- 1
    }else{
      attr(targets_update, 'tzone') <- 'UTC'
    }
    cue <- gd_update < targets_update
  }
  
  if(data_source == 'hobo'){
    gdrive_hobo <- drive_ls(as_id(id))
    local_hobo <- list.files('data/hobo')
    
    cue <- all(gdrive_hobo$name %in% local_hobo)
  }
  
  if(grepl('castaway', data_source)){
    gdrive_castaway <- drive_ls(as_id(id), type = 'csv', recursive = T)
    local_castaway <- list.files(
      paste('data/castaway',
             gsub('_.*', '', data_source),
             sep = '/'
      )
    )
    
    cue <- all(gdrive_castaway$name %in% local_castaway)
  }
  
  cue
}



gdrive_download <- function(data_source){
  
  id <- switch(data_source,
               rec = '1OsvJwdYRc8Wf9Z-nkF4t3SCQQ89CEWe9a1CCtPRMp7w',
               pot = '1I3zFvMrVjOnMxZ21BSTRVqW7hKbt1qOpDARenosknUM',
               hobo = '1-DOmuSt-ErQeD2H463dFWV5KvtaClHNx',
               rec_castaway = '1_Y_VmelDBG0KY9F9ECMibPNCglaAp-2a',
               pot_castaway = '1n4Ed7XE8K9e9Nyiuk4jVrzv3JCQUznQp')
  
  drive_auth(email = 'obrien@umces.edu')
  
  if(data_source %in% c('rec', 'pot')){
    survey_dir <- ifelse(data_source == 'rec', 'recreational', data_source)
    
    dest_path <- paste0('embargo/', survey_dir, '_data/',
                        data_source,' survey recorded data.xlsx')
    
    drive_download(
      as_id(id),
      dest_path,
      overwrite = TRUE
    )
    
   return(dest_path)
  }
  
  if(data_source == 'hobo'){
    gdrive_hobo <- drive_ls(as_id(id))
    local_hobo <- list.files('data/hobo')
    
    to_download <- gdrive_hobo[!gdrive_hobo$name %in% local_hobo,]
    
    for(i in 1:nrow(to_download)){
      drive_download(as_id(to_download$id[i]),
                     paste0('data/hobo/',
                            to_download$name[i]))
    }
    
    list.files('data/hobo', full.names = T)
  }
  
  if(grepl('castaway', data_source)){
    gdrive_castaway <- drive_ls(as_id(id), type = 'csv', recursive = T)
    local_castaway <- list.files(
      paste(
        'data/castaway',
        gsub('_.*', '', data_source),
        sep = '/'
      )
    )
    
    to_download <- gdrive_castaway[!gdrive_castaway$name %in% local_castaway,]
    
    for(i in 1:nrow(to_download)){
      drive_download(
        as_id(to_download$id[i]),
        paste(
          'data/castaway',
          gsub('_.*', '', data_source),
          to_download$name[i],
          sep = '/'
        )
      )
    }
    
    list.files(paste0('data/castaway/', gsub('_.*', '', data_source)),
               full.names = T)
  }
  
  ### GPS files
  ## Need to create the following directory structure:
  # |-- EMBARGO
  #   |-- GPS
  #     |-- YYMM <-new for each month
  #       |-- RAW
  #         |-- pot
  #         |-- rec
  #       |-- repaired
  #         |-- pot
  #         |-- rec
}
