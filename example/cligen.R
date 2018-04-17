# Create cligen-like rain intervals from hourly meteorological data

library(mesowest)
library(data.table)

# get climate data from mesowest
  bw <- mw('metadata', stid='BDGER', complete=1, sensorvars=1, json=T)
  bwdat <- mw('timeseries', start = 201001010000, end =201804150000, obtimezone='local', stid = 'BDGER')
  obs <- bwdat$STATION$OBSERVATIONS
  prec <- obs$precip_accum_one_hour_set_1

# extract and format data
  dt <- obs$date_time[[1]]
  dt <- as.POSIXct(dt, format = '%Y-%m-%dT%H:%M:%S')
  D <- data.table(dt = dt, ppt = prec[[1]])
  D$day <- mday(D$dt)
  D$month <- month(D$dt)
  D$year <- year(D$dt)
  D$ymd <- paste0(D$year, D$month, D$day)

# find days with rain

  D[, hasrain:=list(sum(ppt)> 0), by = list(ymd)]
  R <- D[hasrain==TRUE,]
  # find beginning of storm
  R[,starttime := min(dt[which(ppt>0)]), by = ymd]
  # find end of storm
  R[, endtime := max(dt[which(ppt>0)])+60*60, by = ymd]
  # get duration of storm 
  R[,duration :=  endtime-starttime, by = ymd]

  # find hour of maximum intensity
  R[,peak := dt[which.max(ppt)], by = ymd]
  # find max intensity
  R[,ip := ppt[which.max(ppt)], by = ymd]
  # find midpoint of hour of maximum intensity, find fractional time to midpoint of the storm
  R[,tp := as.numeric(((peak + 30*60)-starttime), units= 'secs')/as.numeric(duration, units = 'secs'), by = ymd]

  out <- R[, list(rain = sum(ppt)), by = list( day, month, year, duration, tp, ip)]
  out$id <- 1:nrow(out)
  out <- out[, list(id, day, month, year, rain, as.numeric(duration, units= 'hours'), tp, ip)] 
  
  write.csv(out, 'bw_cligen_format.csv', row.names=F)

