library('move2')
library('ctmm')
library("lubridate")
library("sf")
library("dplyr")
# input: move2_loc - output: telemetry.list
# data <- readRDS("./data/raw/input1.rds")
rFunction = function(data) {
  # needed columns: individual.local.identifier (or tag.local.identifier), timestamp, location.long and location.lat
  data <- mt_as_event_attribute(data, names(mt_track_data(data)))
  data <- dplyr::mutate(data, location.long=sf::st_coordinates(data)[,1],
                        location.lat=sf::st_coordinates(data)[,2])
  
  mv2df <- data.frame(data)
  ## as.telemetry expects the track id to be called "individual.local.identifier" this is a quick fix, it might need some more thought to it to make it nicer. HOPE THIS IS FIXED ONCE ctmm INTEGRATES READING IN move2
  # fix: idtrack colum gets the prefix "track_id:", individual.local.identifier gets the sufix "_original" to manain this original infomation
  colnames(mv2df)[colnames(mv2df)%in%make.names(mt_track_id_column(data))] <- paste0("track_id:",make.names(mt_track_id_column(data)))
  colnames(mv2df)[colnames(mv2df)%in%c("individual.local.identifier","individual_local_identifier","individual-local-identifier")] <- paste0(colnames(mv2df)[colnames(mv2df)%in%c("individual.local.identifier","individual_local_identifier","individual-local-identifier")],"_original")
  mv2df$individual_local_identifier <-mt_track_id(data)
  mv2df$timestamp <- mt_time(data) # ensuring used timestamps are in the column "timestamp" as expected by as.telemetry()
  telem <- as.telemetry(mv2df,
                        timezone=tz(mt_time(data)),
                        projection= if(st_is_longlat(data)){NULL}else{projection(data)},
                        na.rm= "col",
                        keep=T,
                        drop=F
  ) # retains all columns, and is always a list
  return(telem)
}