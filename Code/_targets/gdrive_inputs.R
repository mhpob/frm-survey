find_gdrive_cue <- function(survey){
  
  id <- switch(survey,
               rec = '1OsvJwdYRc8Wf9Z-nkF4t3SCQQ89CEWe9a1CCtPRMp7w',
               pot = '1I3zFvMrVjOnMxZ21BSTRVqW7hKbt1qOpDARenosknUM')
  
  drive_auth(email = 'obrien@umces.edu')
  
  gd_update <- as.POSIXct(
    drive_get(
      as_id(id)
    )$drive_resource[[1]]$modifiedTime,
    tz = 'UTC', format = '%Y-%m-%dT%H:%M:%OSZ'
  )
  
  survey_dir <- ifelse(survey == 'rec', 'recreational', survey)
  targets_update <- file.mtime(
    paste0('embargo/', survey_dir, '_data/', survey,' survey recorded data.xlsx')
  )

  # targets_update <- tar_meta()[tar_meta()$name == paste('gdrive_cue', survey, sep = '_'),]$time
  
  # if not a branch:
  # targets_update <- tar_meta()[tar_meta()$name == 'gdrive_cue',]$time
  
  if(length(targets_update) == 0){
    targets_update <- 1
  }else{
    attr(targets_update, 'tzone') <- 'UTC'
  }
  gd_update < targets_update
}



gdrive_download <- function(survey){
  
  id <- switch(survey,
               rec = '1OsvJwdYRc8Wf9Z-nkF4t3SCQQ89CEWe9a1CCtPRMp7w',
               pot = '1I3zFvMrVjOnMxZ21BSTRVqW7hKbt1qOpDARenosknUM')
  
  drive_auth(email = 'obrien@umces.edu')
  
  survey_dir <- ifelse(survey == 'rec', 'recreational', survey)
  
  dest_path <- paste0('embargo/', survey_dir, '_data/', survey,' survey recorded data.xlsx')
  
  drive_download(as_id(id),
                 dest_path,
                 overwrite = TRUE)
  
  dest_path
}
