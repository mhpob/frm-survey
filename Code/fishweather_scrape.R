library(httr)
library(rvest)
library(jsonlite)
library(qs)

fw_del <- GET('https://api.weatherflow.com/wxengine/rest/model/getModelDataBySpot?callback=jQuery1720512415836475669_1680697747280&units_wind=mph&units_temp=f&units_distance=mi&units_precip=in&spot_id=732&model_id=-1&wf_token=3a4a6dea792d08d03da7c993cd24604e&_=1680697758195')


fw_del_text <- content(fw_del, as = 'text', encoding = 'UTF-8')

fw_del_text <- gsub('^.*\\(', '', fw_del_text)
fw_del_text <- gsub(')$', '', fw_del_text)

forecast <- fromJSON(fw_del_text)$model_data |> 
  type.convert(as.is = TRUE)

forecast$model_time_local <- as.POSIXct(forecast$model_time_local)

qsave(forecast, 'fw_forecast.qs', preset = 'archive')
