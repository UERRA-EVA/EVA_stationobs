#' @title Compute the distance on a sphere
#' @description \code{getNearest} computes the distance on the surface of a sphere;
#'   the idea was taken from (Wikipedia, Orthodrome) and coded by L. bierdel and S.
#'   Bentzien in 2011 of Uni Bonn.
#' @param b1 latitude of station
#' @param b2 latitude of gridpoints
#' @param l1 longitude of station
#' @param l2 latitude of gridpoints
#' @return s which is the distance in [km]
getNearest <- function(b1,b2,l1,l2){

  f<-1/298.257223563                # Abplattung Erde
  a<-6378137/1000                    # Aequatorradius Erde

  F<-(b1+b2)/2
  G<-(b1-b2)/2
  l<-(l1-l2)/2
  F<-pi/180*F
  G<-pi/180*G
  l<-pi/180*l

  S<-(sin(G))^2 *cos(l)^2 + cos(F)^2 *sin(l)^2
  C<-(cos(G))^2 * cos(l)^2 + sin(F)^2 *sin(l)^2
  w<-atan(sqrt(S/C))
  D<-2*w*a
  R<-sqrt(S*C)/w
  H1<-(3*R -1)/(2*C)
  H2<-(3*R+1)/(2*C)

  s<-D*(1 + f*H1*(sin(F))^2*(cos(G))^2 - f*H2*(cos(F)^2*(sin(G))^2))

  return(s)
}

#-----------------------------------------------------------------------------------

#' @title Extract and align wind station data to reanalysis data.
#'   \code{ExtractStationData} reads wind speed values and dates from station data
#'   and creates a gap filled (NA), hence continuous, extended time series. Monthly
#'   or daily mean time series are calculated which span the time period of the
#'   longest stretching reanalysis data set. The time steps of the monthly time
#'   series is given with precision of months, those of daily time series with
#'   precision of days. Parts taken from get.timeseries.R of GetPlotsFromFtp and off
#'   website http://bocoup.com/weblog/padding-time-series-with-r/
#' @note needs to add functionality to calculate daily or monthly values only if
#'   there are enough hourly values available; it needsd to be decided what enough
#'   is.
#' @param station.data is an extended time series holding monthly mean station wind
#'   data
#' @param era20c.tsstart is a charcter string of the start date of the ERA20C data
#'   of the format c(YYYY,MM)
#' @param era20c.tsend is the same as above for the end date
#' @param eraI.tsstart is the start date of ERA-Interim data
#' @param eraI.tsend is the end date of ERA-Interim data
#' @param herz.tsstart is the start date of HErZ data
#' @param herz.tsend is the end date of the HErZ data
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param station.daily boolean parameter to specify whether station data is
#'   aggregated in hourly (F) or daily (T) time steps.
#' @return MM.station[timestr] is the potentially gab filled monthly mean station
#'   data spanning the time period of the longest ranging reanalysis time series.
ExtractStationData <- function(station.data, era20c.tsstart, era20c.tsend,
                               eraI.tsstart, eraI.tsend, herz.tsstart, herz.tsend,
                               ana.time.res, station.daily) {

  # extract station values and times
  data.vals = station.data$WINDGESCHWINDIGKEIT
  idx = which(data.vals <= 0.0) #!! exclude all these 0s (and correct missing values)
  data.vals[idx] = NA
  if (!station.daily){
    time.vals <- as.POSIXct(strptime(station.data$MESS_DATUM,
                                     format="%Y-%m-%d %H:%M:%S"),
                            format="%Y-%m-%d %H:%M:%S", tz="UTC")
  } else if (station.daily) {
    time.vals <- as.POSIXct(strptime(station.data$MESS_DATUM, format="%Y-%m-%d"),
                            format="%Y-%m-%d %H:%M:%S", tz="UTC")
  }
  time.series.frame = data.frame(time.vals, data.vals)

  # create a gap free time series from start to end of station measurements
  if (!station.daily){
    full <- seq.POSIXt(time.vals[1], time.vals[length(time.vals)], by='hour')
  } else if (station.daily) {
    full <- seq.POSIXt(time.vals[1], time.vals[length(time.vals)], by='day')
  }
  all.dates.frame <- data.frame(list(time.vals=full))

  # merge time and values into one gap free data frame
  merged.data <- merge(all.dates.frame, time.series.frame, all=T)
  StatXTS = xts(merged.data$data.vals, order.by=merged.data$time.vals)

  # calculate monmean of the station time series
  stat.tsstart = c(as.numeric(substr(as.character(merged.data$time.vals[1]),1,4)),
                   as.numeric(substr(as.character(merged.data$time.vals[1]),6,7)))
  if(ana.time.res$time.res == ana.time.res$monthly) {
    MM.station = as.xts( ts( as.numeric(lapply(split(StatXTS, "months"), mean,
                                               na.rm=TRUE)), # exclude all NANs
                             start=stat.tsstart, frequency=12 ) )
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    daily.xts = period.apply(StatXTS, endpoints(StatXTS, "days"), mean, na.rm=TRUE)
    daily.vals = as.POSIXct(strptime(index(daily.xts), format="%Y-%m-%d"),
                            format="%Y-%m-%d", tz = "UTC")
    MM.station = xts(daily.xts, order.by=daily.vals)
  } else if (ana.time.res$time.res == ana.time.res$hourly) {
    MM.station = StatXTS
  }

  # cut station time series to length of longest dataset (if shorter than station ts)
  if (era20c.tsstart[1] == eraI.tsstart[1] & era20c.tsstart[1] == herz.tsstart[1] &
        era20c.tsend[1] == eraI.tsend[1] & era20c.tsend[1] == herz.tsend[1]) {
    stat.tsstart = era20c.tsstart
    stat.tsend = era20c.tsend
  } else {
    if (ana.time.res$time.res == ana.time.res$monthly) {
      year.start.stat = as.numeric(substr(toString(index(MM.station[1])),5,8))
      year.end.stat = as.numeric(substr(toString(index(tail(MM.station,1))),5,8))
    } else {
      year.start.stat = as.numeric(substr(toString(index(MM.station[1])),1,4))
      year.end.stat = as.numeric(substr(toString(index(tail(MM.station,1))),1,4))
    }
    # set start to January (station data may start in a different month)
    stat.tsstart = c(max(year.start.stat, era20c.tsstart[1]), 1)
    stat.tsend = c(min(year.end.stat, era20c.tsend[1]), 12) # same for December here
  }
  timestr = paste0(toString(stat.tsstart[1]), toString(stat.tsstart[2]), '/',
                   toString(stat.tsend[1]), toString(stat.tsend[2]))

  return(MM.station[timestr])
}

#-----------------------------------------------------------------------------------

#' @title Extract an extended time series off a 2D ERA data (ERA20C, ERA-I)
#' corresponding to a provided point (lon, lat) location.
#' @description \code{ExtractERAxts} extracts the nearest pixel of a provided data
#'   set (reanalysis) given the station location (lon, lat).
#' @param data (reanalysis=) data set from which to extract the time series at the
#'   provided station location
#' @param time.vals time period covered by the data set
#' @param lon longitude grid points of the data set
#' @param lat latitude grid points of the data set
#' @param tsstart character string of the format (YYYY,M) for the newly to be set
#'   start date of the data set
#' @param tsend character string as above for the end date
#' @param stat.lon station longitude
#' @param stat.lat station latitude
#' @return era.xts is the time series extracted off the data set at the
#'   pixel corresponding to the provided station location.
ExtractERAxts <- function(data, time.vals,
                          lon, lat, tsstart, tsend, stat.lon, stat.lat) {

  # set time.vals to class yearmon
  time.vals = as.yearmon(time.vals)

  # need to get pixel from ERA20C corresponding to lonlat of station
  latidx = GetNearestIdx(lat, stat.lat)
  lonidx = GetNearestIdx(lon, stat.lon)
  data.vals = data[lonidx,latidx,]

  time.series.frame = data.frame(time.vals, data.vals)
  era.xts = xts(time.series.frame$data.vals, order.by=time.series.frame$time.vals)

  # adjust to time period as provided by tsstart and tsend
  timestr = paste0(toString(tsstart[1]), toString(tsstart[2]), '/',
                   toString(tsend[1]), toString(tsend[2]))

  return(era.xts[timestr])
}

#-----------------------------------------------------------------------------------

#' @title Extract an extended time series off 2D HErZ data corresponding to a
#' provided point (lon, lat) location.
#' @description \code{ExtractHErZxts} extracts the time series of the HErZ pixel
#'   corresponding to the station location. HErZ data come in an irregular polar
#'   projection which calls for a function to extract the correct pixel off that grid.
#' @param herz.data is the HErZ data set from which to extract the time series at
#'   the station location
#' @param time.vals time period covered by the HErZ data set
#' @param herz.lon longitude grid points of the HErZ data set
#' @param herz.lat latitude grid points of the HErZ data set
#' @param tsstart character string of the format (YYYY,M) of the start date of the
#'   HErZ data set time series
#' @param tsend character string as above of the end date of the HErZ data set
#' @param stat.lon station longitude
#' @param stat.lat station latitude
#' @return herz.xts[timestr]is the time series extracted off the HErZ reanalysis at
#'   the pixel corresponding to the provided station location
#'   Extract nearest HErZ pixel corresponding to station lon, lat calling Uni Bonn
#'   function getNearest.
ExtractHErZxts <- function(herz.data, time.vals, herz.lon, herz.lat,
                           tsstart, tsend, stat.lon, stat.lat) {

  # set time.vals to class yearmon
  time.vals = as.yearmon(time.vals)

  # need to get pixel from HErZ corresponding to lon, lat of station
  dist.to.point = getNearest(stat.lat, herz.lat, stat.lon, herz.lon)
  index.at.point = arrayInd(which.min(dist.to.point), dim(dist.to.point))
  data.vals = herz.data[index.at.point[1],index.at.point[2],]

  time.series.frame = data.frame(time.vals, data.vals)
  herz.xts = xts(time.series.frame$data.vals, order.by=time.series.frame$time.vals)

  # adjust to time period as provided by tsstart and tsend
  timestr = SetToDate(tsstart, tsend)

  return(herz.xts[timestr])
}

#-----------------------------------------------------------------------------------

#' @title Extract tower measurement data.
#' @description This function reads one or more variable name(s) off a netCDF file
#'   and formats the time axis. A list named by the variable names is returned
#'   which holds extended time series for the complete time series of the variable
#'   name specified (or all).
#' @param file.name character string holding the file name of the file to be read
#' @param para.name character string holding the parameter name(s) to be read
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @return The return value is a named list with the names of the parameter names
#'   holding extended time series of that parameter.
ExtractTowerData <- function(file.name, para.name, ana.time.res) {
  CheckFile(file.name)
  data.lst = list()
  for (cnt in seq(para.name)) {
    dat = ReadNetcdf(para.name[cnt], file.name)
    if (ana.time.res$time.res == ana.time.res$monthly) {
      time.vals = as.yearmon(dat$time)
    } else if (ana.time.res$time.res == ana.time.res$daily) {
      time.vals = as.POSIXct(strptime(dat$time, format="%Y-%m-%d"),
                             format="%Y-%m-%d", tz = "UTC")
    } else if (ana.time.res$time.res == ana.time.res$hourly) {
      time.vals = as.POSIXct(strptime(dat$time, format="%Y-%m-%d %H:%M:%S"),
                             format="%Y-%m-%d %H:%M:%S", tz = "UTC")
    }
    data.xts = xts(dat$data, order.by=time.vals)
    data.lst[[para.name[cnt]]] = data.xts
  }
  return(data.lst)
}

#-----------------------------------------------------------------------------------
