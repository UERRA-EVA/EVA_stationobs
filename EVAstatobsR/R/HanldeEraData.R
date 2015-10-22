#' @title Read monthly or daily HErZ data and convert to xts
#' @description This function reads HErZ reanalysis data at the point of the station
#'   location of monthly or daily data and for the complete profile consisting of
#'   six model levels (which are hard coded into variable names here because they
#'   are assumed to stay the way they are). After reading and temporarily storing
#'   into a data frame, the return values of this function are extended time series
#'   holding the complete time series (time) at each model level (windspeed). Only
#'   here it is decided whether to return the complete profile ore only 10m and 100m
#'   data. The time steps of the monthly time series is given with precision of
#'   months, those of daily time series with precision of days.
#' @param herz.param string of length n holding the parameters of the n different
#'   model levels.
#' @param herz.fname string of length n holding the file names of the n different
#'   daily or one monthly file(s).
#' @param herz.tsstart string of the start date of the HErZ data of the format
#'   c(YYYY,MM).
#' @param herz.tsend string of the end date of the HErZ data.
#' @param lonidx numeric value of the longitude station location which to extract
#'   off the HErZ grid.
#' @param latidx numeric value of the latitude station location.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param verb.dat optional boolean to determine whether to print out what's going
#'   on (T). The default is to suppress printing (verb.dat = FALSE).
#' @return A named list holding the extended time series of HErZ data at the
#'   different model levels: (herz10=,herz35=,herz69=,herz116=,herz178,herz258). For
#'   unspecified model levels (e.g., if(!herz.profile), a NULL value will be
#'   returned.
ReadHerzNetcdfMonthlyDaily2Xts <- function(herz.param, herz.fname,
                                           herz.tsstart, herz.tsend,
                                           lonidx, latidx,
                                           ana.time.res, herz.profile,
                                           verb.dat=FALSE) {

  # read HErZ data into a data.frame
  if (ana.time.res$time.res == ana.time.res$monthly) {
    if (herz.profile) {
      dat10 = ReadNetcdf(herz.param[1], herz.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat35 = ReadNetcdf(herz.param[2], herz.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat69 = ReadNetcdf(herz.param[3], herz.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat116 = ReadNetcdf(herz.param[4], herz.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat178 = ReadNetcdf(herz.param[5], herz.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat258 = ReadNetcdf(herz.param[6], herz.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)

      ndf = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                       dat116$data, dat178$data, dat258$data)

    } else {
      dat10 = ReadNetcdf(herz.param[1], herz.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat116 = ReadNetcdf(herz.param[2], herz.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)

      ndf = data.frame(dat10$time, dat10$data, dat116$data)

    }
  } else if (ana.time.res$time.res == ana.time.res$daily) {

    ndf = data.frame()
    if (herz.profile) {
      for (step in seq(herz.fname)) {
        dat10 = ReadNetcdf(herz.param[1], herz.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat35 = ReadNetcdf(herz.param[2], herz.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat69 = ReadNetcdf(herz.param[3], herz.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat116 = ReadNetcdf(herz.param[4], herz.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat178 = ReadNetcdf(herz.param[5], herz.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat258 = ReadNetcdf(herz.param[6], herz.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)

        df = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                        dat116$data, dat178$data, dat258$data)
        ndf = rbind(ndf, df)
      }
    } else {
      for (step in seq(herz.fname)) {
        dat10 = ReadNetcdf(herz.param[1], herz.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat116 = ReadNetcdf(herz.param[2], herz.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)

        df = data.frame(dat10$time, dat10$data, dat116$data)
        ndf = rbind(ndf, df)
      }
    }
  }

  # convert data.frame of HErZ data into an extended time series
  if (ana.time.res$time.res == ana.time.res$monthly) {
    ndf$dat10.time = as.yearmon(ndf$dat10.time)
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    ndf$dat10.time = as.POSIXct(strptime(ndf$dat10.time, format="%Y-%m-%d"),
                                format="%Y-%m-%d", tz = "UTC")
  }
  timestr = SetToDate(herz.tsstart, herz.tsend)
  herz10.xts = xts(ndf$dat10.data, order.by=ndf$dat10.time)
  herz10.xts = herz10.xts[timestr]
  herz116.xts = xts(ndf$dat116.data, order.by=ndf$dat10.time)
  herz116.xts = herz116.xts[timestr]
  if (herz.profile) {
    herz35.xts = xts(ndf$dat35.data, order.by=ndf$dat10.time)
    herz35.xts = herz35.xts[timestr]
    herz69.xts = xts(ndf$dat69.data, order.by=ndf$dat10.time)
    herz69.xts = herz69.xts[timestr]
    herz178.xts = xts(ndf$dat178.data, order.by=ndf$dat10.time)
    herz178.xts = herz178.xts[timestr]
    herz258.xts = xts(ndf$dat258.data, order.by=ndf$dat10.time)
    herz258.xts = herz258.xts[timestr]
  } else {
    herz35.xts = NULL
    herz69.xts = NULL
    herz178.xts = NULL
    herz258.xts = NULL
  }

  return(list(herz10=herz10.xts, herz35=herz35.xts, herz69=herz69.xts,
              herz116=herz116.xts, herz178=herz178.xts, herz258=herz258.xts))
}

#-----------------------------------------------------------------------------------

#' @title
#' @description
#' @param
#' @return
ReadHerzNetcdfHourly2Xts <- function(herz.param, herz.fname,
                                     herz.tsstart, herz.tsend,
                                     herz.profile) {

  # read HErZ data into a data.frame
  if (herz.profile) {
    dat10 = ReadNetcdf(herz.param, herz.fname[1])
    dat35 = ReadNetcdf(herz.param, herz.fname[2])
    dat69 = ReadNetcdf(herz.param, herz.fname[3])
    dat116 = ReadNetcdf(herz.param, herz.fname[4])
    dat178 = ReadNetcdf(herz.param, herz.fname[5])
    dat258 = ReadNetcdf(herz.param, herz.fname[6])

    ndf = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                     dat116$data, dat178$data, dat258$data)
  } else {
    dat10 = ReadNetcdf(herz.param, herz.fname[1])
    dat116 = ReadNetcdf(herz.param, herz.fname[4])

    ndf = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                     dat116$data, dat178$data, dat258$data)
  }

  # convert data.frame of HErZ data into an extended time series
  ndf$dat10.time = as.POSIXct(strptime(ndf$dat10.time, format="%Y-%m-%d %H:%M:%S"),
                              format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  timestr = SetToDate(herz.tsstart, herz.tsend)
  herz10.xts = xts(ndf$dat10.data, order.by=ndf$dat10.time)
  herz10.xts = herz10.xts[timestr]
  herz116.xts = xts(ndf$dat116.data, order.by=ndf$dat10.time)
  herz116.xts = herz116.xts[timestr]
  if (herz.profile) {
    herz35.xts = xts(ndf$dat35.data, order.by=ndf$dat10.time)
    herz35.xts = herz35.xts[timestr]
    herz69.xts = xts(ndf$dat69.data, order.by=ndf$dat10.time)
    herz69.xts = herz69.xts[timestr]
    herz178.xts = xts(ndf$dat178.data, order.by=ndf$dat10.time)
    herz178.xts = herz178.xts[timestr]
    herz258.xts = xts(ndf$dat258.data, order.by=ndf$dat10.time)
    herz258.xts = herz258.xts[timestr]
  } else {
    herz35.xts = NULL
    herz69.xts = NULL
    herz178.xts = NULL
    herz258.xts = NULL
  }

  return(list(herz10=herz10.xts, herz35=herz35.xts, herz69=herz69.xts,
              herz116=herz116.xts, herz178=herz178.xts, herz258=herz258.xts))
}

#-----------------------------------------------------------------------------------

#' @title Read monthly or daily ERA20C or ERA-Interim data and convert to xts
#' @description This function reads monthly or daily Era data at the location of the
#'   station location. The time steps of the monthly time series is given with
#'   precision of months, those of daily time series with precision of days. The
#'   return values are Era 10m windspeed and in case of ERA20C data windspeed in
#'   100m height.
#' @param era.param string of length n holding the parameters of the n different
#'   model levels (one for ERA-I, two for ERA20C).
#' @param era.fname string holding the file name of the ERA data.
#' @param era.tsstart string of the start date of the ERA data of the format
#'   c(YYYY,MM).
#' @param era.tsend same as above for the end date.
#' @param lonidx numeric value of the longitude station location which to extract
#'   off the ERA grid.
#' @param latidx same as above for latitude.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param era20c boolean to determine whether the data is ERA20C (T) or
#'   ERA-Interim (F).
#' @param verb.dat optional boolean to determine whether to print out what's going
#'   on (T). The default is to suppress printing (verb.dat = FALSE).
#' @return A named list holding the extended time series of the ERA data at 10m
#'   height for ERA-Interim and 10m and 100m for ERA20C.: (era10=,era20c100=). If
#'   ERA-Interim data is read, a NULL value will be returned for era20c100.
ReadEraNetcdf2Xts <- function(era.param, era.fname,
                              era.tsstart, era.tsend,
                              lonidx, latidx, ana.time.res,
                              era20c=TRUE, verb.dat=FALSE) {

  # Read ERA-I or ERA20C monthly or daily data
  era10m = ReadNetcdf(era.param[1], era.fname, count=c(1,1,-1),
                      start=c(lonidx, latidx, 1), verb.dat=verb.dat)
  if (era20c) {
    era20c100m = ReadNetcdf(era.param[2], era.fname, count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)
    df = data.frame(era10m$time, era10m$data, era20c100m$data)
  } else {
    df = data.frame(era10m$time, era10m$data)
  }

  # convert ERA data and time values into an extended time series
  # and apply start and end date
  if (ana.time.res$time.res == ana.time.res$monthly) {
    df$era10m.time = as.yearmon(df$era10m.time)
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    df$era10m.time = as.POSIXct(strptime(df$era10m.time, format="%Y-%m-%d"),
                                format="%Y-%m-%d", tz = "UTC")
  }
  timestr = SetToDate(era.tsstart, era.tsend)
  era.xts = xts(df$era10m.data, order.by=df$era10m.time)
  era.xts = era.xts[timestr]
  if (era20c) {
    era20c100.xts = xts(df$era20c100m.data, order.by=df$era10m.time)
    era20c100.xts = era20c100.xts[timestr]
  } else {
    era20c100.xts = NULL
  }

  return(list(era10=era.xts, era20c100=era20c100.xts))
}

#-----------------------------------------------------------------------------------

#' @title Produce a data frame holding the wind speed profiles of the tower
#'   measurements, from the HErZ reanalysis, and the available values of the
#'   global reanalyses.
#' @description First the end point to the time series is set to the shortest
#'   available time period. The beginning is set by the tower measurements because
#'   they are always shorter than the available reanalysis data.
#'   A data frame is created holding the available reanalysis and tower measurement
#'   data. The columns are named by the data source and height. One data frame is
#'   returned.
#' @param tower.xts extended time series of available tower data; for FINO1,2 there
#'   is only one height level of data available
#' @param tower2.xts,tower3.xts,tower4.xts,tower5.xts,tower6.xts optional extended
#'   time series holding more available height levels of tower measurement data
#' @param herz10.xts,herz35.xts,herz69.xts,herz116.xts,herz178.xts,herz258.xts all
#'   available height levels of HErZ data as extended time series
#' @param era20c10.xts,era20c100.xts the two available height levels of 10m and 100m
#'   data of ERA20C as extended time series
#' @param tower.tsstart,tower.tsend,herz.tsend,era20c.tsend start and end time in
#'   the format c(yyyy,mm) of the tower measurements, HErZ and ERA20C reanalyses
#' @param tower.name string holding the tower name; if an unexpected name is passed
#'   the function will terminate execution
#' @return The return value is a data frame which holds the available data of the
#'   wind speed profile at different height levels which are named columns of
#'   tower measurements and reanalysis data
GetTowerProfileTS <- function(tower.xts, tower2.xts=NULL, tower3.xts=NULL,
                              tower4.xts=NULL, tower5.xts=NULL, tower6.xts=NULL,
                              herz10.xts, herz35.xts, herz69.xts, herz116.xts,
                              herz178.xts, herz258.xts, era20c10.xts, era20c100.xts,
                              tower.tsstart, tower.tsend, herz.tsend, era20c.tsend,
                              tower.name=NULL) {

  if (!is.null(tower.name) & tower.name != "Fino1" & tower.name != "Fino2" &
      tower.name != "Lindenberg" & tower.name != "Cabauw") {
    CallStop(paste0("Unexpected tower.name: ", tower.name, " "))
  }

  #XXXXXXXXXXXXXXXXXXXXXXXXX
  # tsend(max aus allen)
  # dann die daten reihen mit NA auffüllen, die kürzer als das max tsend sind
  #XXXXXXXXXXXXXXXXXXXXXXXXX

  tsend = c(min(tower.tsend[1], herz.tsend[1], era20c.tsend[1]), 12)
  timestr = paste0(toString(tower.tsstart[1]), toString(tower.tsstart[2]), '/',
                   toString(tsend[1]), toString(tsend[2]))

  tower.xts = tower.xts[timestr]
  if (tower.name == "Lindenberg" | tower.name == "Cabauw") {
    tower2.xts = tower2.xts[timestr]
    tower3.xts = tower3.xts[timestr]
    tower4.xts = tower4.xts[timestr]
    tower5.xts = tower5.xts[timestr]
    tower6.xts = tower6.xts[timestr]
  }
  herz10.xts = herz10.xts[timestr]
  herz35.xts = herz35.xts[timestr]
  herz69.xts = herz69.xts[timestr]
  herz116.xts = herz116.xts[timestr]
  herz178.xts = herz178.xts[timestr]
  herz258.xts = herz258.xts[timestr]
  era20c10.xts = era20c10.xts[timestr]
  era20c100.xts = era20c100.xts[timestr]

  if (tower.name == "Fino1") {
    df = data.frame(date=index(tower.xts), Fino1=coredata(tower.xts))
  } else if (tower.name == "Fino2") {
    df = data.frame(date=index(tower.xts), Fino2=coredata(tower.xts))
  } else if (tower.name == "Lindenberg") {
    df = data.frame(date=index(tower.xts), Lind10=coredata(tower.xts))
    df$Lind20 = coredata(tower2.xts)
    df$Lind40 = coredata(tower3.xts)
    df$Lind60 = coredata(tower4.xts)
    df$Lind80 = coredata(tower5.xts)
    df$Lind98 = coredata(tower6.xts)
  } else if (tower.name == "Cabauw") {
    df = data.frame(date=index(tower.xts), Cabauw10=coredata(tower.xts))
    df$Cabauw20 = coredata(tower2.xts)
    df$Cabauw40 = coredata(tower3.xts)
    df$Cabauw80 = coredata(tower4.xts)
    df$Cabauw140 = coredata(tower5.xts)
    df$Cabauw200 = coredata(tower6.xts)
  }
  df$herz10 = coredata(herz10.xts)
  df$herz35 = coredata(herz35.xts)
  df$herz69 = coredata(herz69.xts)
  df$herz116 = coredata(herz116.xts)
  df$herz178 = coredata(herz178.xts)
  df$herz258 = coredata(herz258.xts)
  df$era20c10 = coredata(era20c10.xts)
  df$era20c100 = coredata(era20c100.xts)

  return(df)
}

#-----------------------------------------------------------------------------------

#' @title
#' @description
#' @param
#' @return
GetTowerObject <- function(tower.xts, tower2.xts=NULL, tower3.xts=NULL,
                           tower4.xts=NULL, tower5.xts=NULL, tower6.xts=NULL,
                           herz10.xts, herz35.xts, herz69.xts, herz116.xts,
                           herz178.xts, herz258.xts, era20c10.xts, era20c100.xts,
                           tower.tsstart, tower.tsend, herz.tsend, era20c.tsend,
                           tower.name="", tower.lon, tower.lat,
                           tower.param, era20c.param) {

  if (nchar(tower.name) == 0 | tower.name != "Fino1" & tower.name != "Fino2" &
      tower.name != "Lindenberg" & tower.name != "Cabauw") {
    CallStop(paste0("Unexpected tower.name: ", tower.name, " "))
  }

  tsstart = c(max(tower.tsstart[1]),1) # this can be extended if necessary
  tsend = c(min(tower.tsend[1], herz.tsend[1], era20c.tsend[1]), 12)
  timestr = paste0(toString(tower.tsstart[1]), toString(tower.tsstart[2]), '/',
                   toString(tsend[1]), toString(tsend[2]))

  tower.xts = tower.xts[timestr]
  if (!is.null(tower2.xts)) tower2.xts = tower2.xts[timestr]
  if (!is.null(tower3.xts)) tower3.xts = tower3.xts[timestr]
  if (!is.null(tower4.xts)) tower4.xts = tower4.xts[timestr]
  if (!is.null(tower5.xts)) tower5.xts = tower5.xts[timestr]
  if (!is.null(tower6.xts)) tower6.xts = tower6.xts[timestr]
  herz10.xts = herz10.xts[timestr]
  herz35.xts = herz35.xts[timestr]
  herz69.xts = herz69.xts[timestr]
  herz116.xts = herz116.xts[timestr]
  herz178.xts = herz178.xts[timestr]
  herz258.xts = herz258.xts[timestr]
  era20c10.xts = era20c10.xts[timestr]
  era20c100.xts = era20c100.xts[timestr]


  tower.df = data.frame(date=index(tower.xts),
                        ReanaName="", StationName=tower.name,
                        latitude=tower.lat, longitude=tower.lon,
                        wind_speed=coredata(tower.xts),
                        height=strsplit(tower.param, '_')[[1]][[2]])
  if (!is.null(tower2.xts)) {
    tower2.df = data.frame(date=index(tower2.xts),
                           ReanaName="", StationName=tower.name,
                           latitude=tower.lat, longitude=tower.lon,
                           wind_speed=coredata(tower2.xts),
                           height=strsplit(tower.param, '_')[[2]][[2]])
  }
  if (!is.null(tower3.xts)) {
    tower3.df = data.frame(date=index(tower3.xts),
                           ReanaName="", StationName=tower.name,
                           latitude=tower.lat, longitude=tower.lon,
                           wind_speed=coredata(tower3.xts),
                           height=strsplit(tower.param, '_')[[3]][[2]])
  }
  if (!is.null(tower4.xts)) {
    tower4.df = data.frame(date=index(tower4.xts),
                           ReanaName="", StationName=tower.name,
                           latitude=tower.lat, longitude=tower.lon,
                           wind_speed=coredata(tower4.xts),
                           height=strsplit(tower.param, '_')[[4]][[2]])
  }
  if (!is.null(tower5.xts)) {
    tower5.df = data.frame(date=index(tower5.xts),
                           ReanaName="", StationName=tower.name,
                           latitude=tower.lat, longitude=tower.lon,
                           wind_speed=coredata(tower5.xts),
                           height=strsplit(tower.param, '_')[[5]][[2]])
  }
  if (!is.null(tower6.xts)) {
    tower6.df = data.frame(date=index(tower6.xts),
                           ReanaName="", StationName=tower.name,
                           latitude=tower.lat, longitude=tower.lon,
                           wind_speed=coredata(tower6.xts),
                           height=strsplit(tower.param, '_')[[6]][[2]])
  }
  herz10.df = data.frame(date=index(herz10.xts),
                         ReanaName="HErZ", StationName=tower.name,
                         latitude=tower.lat, longitude=tower.lon,
                         wind_speed=coredata(herz10.xts),
                         height="10m")
  herz35.df = data.frame(date=index(herz35.xts),
                         ReanaName="HErZ", StationName=tower.name,
                         latitude=tower.lat, longitude=tower.lon,
                         wind_speed=coredata(herz35.xts),
                         height="35m")
  herz69.df = data.frame(date=index(herz69.xts),
                         ReanaName="HErZ", StationName=tower.name,
                         latitude=tower.lat, longitude=tower.lon,
                         wind_speed=coredata(herz69.xts),
                         height="69m")
  herz116.df = data.frame(date=index(herz116.xts),
                          ReanaName="HErZ", StationName=tower.name,
                          latitude=tower.lat, longitude=tower.lon,
                          wind_speed=coredata(herz116.xts),
                          height="116m")
  herz178.df = data.frame(date=index(herz178.xts),
                          ReanaName="HErZ", StationName=tower.name,
                          latitude=tower.lat, longitude=tower.lon,
                          wind_speed=coredata(herz178.xts),
                          height="178m")
  herz258.df = data.frame(date=index(herz258.xts),
                          ReanaName="HErZ", StationName=tower.name,
                          latitude=tower.lat, longitude=tower.lon,
                          wind_speed=coredata(herz258.xts),
                          height="258m")
  if (!is.null(era20c10.xts)) {
    era20c10.df = data.frame(date=index(era20c10.xts),
                             ReanaName="ERA20C", StationName=tower.name,
                             latitude=tower.lat, longitude=tower.lon,
                             wind_speed=coredata(era20c10.xts),
                             height=strsplit(era20c.param, '_')[[1]][[2]])
  }
  if (!is.null(era20c100.xts)) {
    era20c100.df = data.frame(date=index(era20c100.xts),
                              ReanaName="ERA20C", StationName=tower.name,
                              latitude=tower.lat, longitude=tower.lon,
                              wind_speed=coredata(era20c100.xts),
                              height=strsplit(era20c.param, '_')[[2]][[2]])
  }

  if (!is.null(tower6.xts)) {
    if (!is.null(era20c10.xts) & !is.null(era20c100.xts)) {
      climate.tower.object = climate(data_tables=
                                       list(tower=tower.df, tower2=tower2.df,
                                            tower3=tower3.df, tower4=tower4.df,
                                            tower5=tower5.df, tower6=tower6.df,
                                            herz10=herz10.df, herz35=herz35.df,
                                            herz69=herz69.df, herz116=herz116.df,
                                            herz178=herz178.df, herz258=herz258.df,
                                            era20c10=era20c10.df,
                                            era20c100=era20c100.df))
    }  else {
      climate.tower.object = climate(data_tables=
                                       list(tower=tower.df, tower2=tower2.df,
                                            tower3=tower3.df, tower4=tower4.df,
                                            tower5=tower5.df, tower6=tower6.df,
                                            herz10=herz10.df, herz35=herz35.df,
                                            herz69=herz69.df, herz116=herz116.df,
                                            herz178=herz178.df, herz258=herz258.df))
    }
  } else {
    if (!is.null(era20c10.xts) & !is.null(era20c100.xts)) {
      climate.tower.object = climate(data_tables=
                                       list(tower=tower.df, herz10=herz10.df,
                                            herz35=herz35.df, herz69=herz69.df,
                                            herz116=herz116.df, herz178=herz178.df,
                                            herz258=herz258.df, era20c10=era20c10.df,
                                            era20c100=era20c100.df))
    } else {
      climate.tower.object = climate(data_tables=
                                       list(tower=tower.df, herz10=herz10.df,
                                            herz35=herz35.df, herz69=herz69.df,
                                            herz116=herz116.df, herz178=herz178.df,
                                            herz258=herz258.df))
    }
  }

  return(list(tower.object = climate.tower.object))

}

#-----------------------------------------------------------------------------------
