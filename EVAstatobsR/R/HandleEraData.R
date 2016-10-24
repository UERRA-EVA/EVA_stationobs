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
#' @param rra.para string of length n holding the parameters of the n different
#'   model levels.
#' @param rra.fname string of length n holding the file names of the n different
#'   daily or one monthly file(s).
#' @param rra.tsstart string of the start date of the HErZ data of the format
#'   c(YYYY,MM).
#' @param rra.tsend string of the end date of the HErZ data.
#' @param lonidx numeric value of the longitude station location which to extract
#'   off the HErZ grid.
#' @param latidx numeric value of the latitude station location.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param herz.profile is a boolean which indicated whether to read the complete
#'   HErZ profile (T) (six height levels) or only 10m and 116m (F).
#' @param only.10m is a boolean which indicates whether to read only 10m wind speed
#'   data (T) or not (F). This has only an effect if herz.profile=F. Default value
#'   is only.10m=F.
#' @param verb.dat optional boolean to determine whether to print out what's going
#'   on (T). The default is to suppress printing (verb.dat = FALSE).
#' @return A named list holding the extended time series of HErZ data at the
#'   different model levels: (herz10=,herz35=,herz69=,herz116=,herz178,herz258). For
#'   unspecified model levels (e.g., if(!herz.profile), a NULL value will be
#'   returned.
#' @importFrom zoo as.yearmon
#' @importFrom xts xts
#' @export
ReadHerzNetcdfMonthlyDaily2Xts <- function(rra.para, rra.fname,
                                           rra.tsstart, rra.tsend,
                                           lonidx, latidx,
                                           ana.time.res, herz.profile,
                                           only.10m=FALSE, verb.dat=FALSE) {

  # read HErZ data into a data.frame
  if (ana.time.res$time.res == ana.time.res$monthly) {
    if (herz.profile) {
      dat10 = ReadNetcdf(rra.para[6], rra.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat35 = ReadNetcdf(rra.para[5], rra.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat69 = ReadNetcdf(rra.para[4], rra.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat116 = ReadNetcdf(rra.para[3], rra.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat178 = ReadNetcdf(rra.para[2], rra.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat258 = ReadNetcdf(rra.para[1], rra.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)

      ndf = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                       dat116$data, dat178$data, dat258$data)

    } else {
      if (only.10m) {
        dat10 = ReadNetcdf(rra.para, rra.fname, count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        ndf = data.frame(dat10$time, dat10$data)
      } else {
        dat10 = ReadNetcdf(rra.para[2], rra.fname, count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat116 = ReadNetcdf(rra.para[1], rra.fname, count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        ndf = data.frame(dat10$time, dat10$data, dat116$data)
      }

    }
  } else if (ana.time.res$time.res == ana.time.res$daily) {

    ndf = data.frame()
    if (herz.profile) {
      for (step in seq(rra.fname)) {
        dat10 = ReadNetcdf(rra.para[6], rra.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat35 = ReadNetcdf(rra.para[5], rra.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat69 = ReadNetcdf(rra.para[4], rra.fname[step], count=c(1,1,-1),
                           start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat116 = ReadNetcdf(rra.para[3], rra.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat178 = ReadNetcdf(rra.para[2], rra.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)
        dat258 = ReadNetcdf(rra.para[1], rra.fname[step], count=c(1,1,-1),
                            start=c(lonidx, latidx, 1), verb.dat=verb.dat)

        df = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                        dat116$data, dat178$data, dat258$data)
        ndf = rbind(ndf, df)
      }
    } else {
      for (step in seq(rra.fname)) {
        if (only.10m) {
          dat10 = ReadNetcdf(rra.para, rra.fname[step], count=c(1,1,-1),
                             start=c(lonidx, latidx, 1), verb.dat=verb.dat)
          df = data.frame(dat10$time, dat10$data)
          ndf = rbind(ndf, df)
        } else {
          dat10 = ReadNetcdf(rra.para[2], rra.fname[step], count=c(1,1,-1),
                             start=c(lonidx, latidx, 1), verb.dat=verb.dat)
          dat116 = ReadNetcdf(rra.para[1], rra.fname[step], count=c(1,1,-1),
                              start=c(lonidx, latidx, 1), verb.dat=verb.dat)
          df = data.frame(dat10$time, dat10$data, dat116$data)
          ndf = rbind(ndf, df)
        }
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
  timestr = SetToDate(rra.tsstart, rra.tsend)
  herz10.xts = xts(ndf$dat10.data, order.by=ndf$dat10.time)
  herz10.xts = herz10.xts[timestr]
  herz116.xts = NULL
  if (!only.10m) {
    herz116.xts = xts(ndf$dat116.data, order.by=ndf$dat10.time)
    herz116.xts = herz116.xts[timestr]
  }
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

#' @title Read hourly HErZ wind speed at six height levels.
#' @description Read hourly HErZ netCDF files of wind speed at up to six height
#'   levels (herz.profile=T). If herz.profile=F, then only 10m and 116m wind speeds
#'   are read, and if herz.profile=F and only.10m=T, then only the 10m wind speeds
#'   are read.
#' @param rra.para is a string holding the variable name to be read off the netCDF
#'   file.
#' @param rra.fname is a string holding the netCDF file name to be read from.
#' @param rra.tsstart is a string of the start date of the RRA data of the format
#'   c(YYYY,MM).
#' @param rra.tsend same as above for the end date.
#' @param herz.profile is a boolean which indicated whether to read the complete
#'   HErZ profile (T) (six height levels) or only 10m and 116m (F).
#' @param only.10m is a boolean which indicates whether to read only 10m wind speed
#'   data (T) or not (F). This has only an effect if herz.profile=F. Default value
#'   is only.10m=F.
#' @return a list holding the wind speed values in extended time series for each
#'   height level. If no data had been read, NULL is returned.
#' @importFrom xts xts
#' @export
ReadHerzNetcdfHourly2Xts <- function(rra.para, rra.fname,
                                     rra.tsstart, rra.tsend,
                                     herz.profile, only.10m=FALSE) {

  # read HErZ data into a data.frame
  if (herz.profile) {
    dat10 = ReadNetcdf(rra.para, rra.fname[6])
    dat35 = ReadNetcdf(rra.para, rra.fname[5])
    dat69 = ReadNetcdf(rra.para, rra.fname[4])
    dat116 = ReadNetcdf(rra.para, rra.fname[3])
    dat178 = ReadNetcdf(rra.para, rra.fname[2])
    dat258 = ReadNetcdf(rra.para, rra.fname[1])

    ndf = data.frame(dat10$time, dat10$data, dat35$data, dat69$data,
                     dat116$data, dat178$data, dat258$data)
  } else {
    if (only.10m) {
      dat10 = ReadNetcdf(rra.para, rra.fname)
      ndf = data.frame(dat10$time, dat10$data)
    } else {
      dat10 = ReadNetcdf(rra.para, rra.fname[2])
      dat116 = ReadNetcdf(rra.para, rra.fname[1])
      ndf = data.frame(dat10$time, dat10$data, dat116$data)
    }
  }

  # convert data.frame of HErZ data into an extended time series
  ndf$dat10.time = as.POSIXct(strptime(ndf$dat10.time, format="%Y-%m-%d %H:%M:%S"),
                              format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  timestr = SetToDate(rra.tsstart, rra.tsend)
  herz10.xts = xts(ndf$dat10.data, order.by=ndf$dat10.time)
  herz10.xts = herz10.xts[timestr]
  herz116.xts = NULL
  if (!only.10m) {
    herz116.xts = xts(ndf$dat116.data, order.by=ndf$dat10.time)
    herz116.xts = herz116.xts[timestr]
  }
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

#' @title Read 6-hourly ERA-Interim and ERA20C wind speed data.
#' @description Read 6-hourly ERA-Interim and ERA20C netCDF files of wind speed at
#'   10m and 100m height and fill the six hourly data with NA to an hourly time
#'   series.
#' @param era.para10m,era.para100m strings of length n holding the parameters of the
#'   n different model levels (one for ERA-I, two for ERA20C).
#' @param era.fname10m,era.fname100m strings holding the file names of the ERA data.
#' @param read.10m,read.100m optional boolean parameters setting whether to read
#'   data at only 10m, only 100m, or at both heights. The default setting is to
#'   read 10m and not to read 100m data.
#' @return a list holding the wind speed values in extended time series for each
#'   height and global reanalysis.
#' @importFrom xts xts
#' @export
ReadERANetcdfHourly2Xts <- function(era.para10m, era.para100m,
                                    era.fname10m, era.fname100m,
                                    era.tsstart, era.tsend,
                                    read.10m=TRUE, read.100m=FALSE) {

  # sanity check
  if (!(read.10m) & !(read.100m)) {
    CallStop(paste0("You need to read at least one of 10m or 100m data! ",
                    "Now both are set to FALSE "))
  }

  # read ERA data into a data.frame
  if (read.10m) {
    dat10 = ReadNetcdf(era.para10m, era.fname10m)
    if (!read.100m) {
      ndf = data.frame(dat10$time, dat10$data)
    }
  }
  if (read.100m) {
    dat100 = ReadNetcdf(era.para100m, era.fname100m)
    if (!read.10m) {
      ndf = data.frame(dat100$time, dat100$data)
    }
  }
  if (read.10m & read.100m) {
    ndf = data.frame(dat10$time, dat10$data, dat100$data)
  }

  # convert data.frame of ERA data into an extended time series
  if (read.10m) {
    ndf$dat10.time = as.POSIXct(strptime(ndf$dat10.time, format="%Y-%m-%d %H:%M:%S"),
                              format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    ndf$dat100.time = as.POSIXct(strptime(ndf$dat100.time, format="%Y-%m-%d %H:%M:%S"),
                                format="%Y-%m-%d %H:%M:%S", tz = "UTC")
  }
  timestr = SetToDate(era.tsstart, era.tsend)
  if (read.10m) {
    era10.xts = xts(ndf$dat10.data, order.by=ndf$dat10.time)
    era10.xts = era10.xts[timestr]
    if (read.100m) {
      era100.xts = xts(ndf$dat100.data, order.by=ndf$dat10.time)
      era100.xts = era100.xts[timestr]
    } else {
      era100.xts = NULL
    }
  } else {
    era100.xts = xts(ndf$dat100.data, order.by=ndf$dat100.time)
    era10.xts = NULL
  }

  # fill era xts to hourly time series *** HARDCODED***
  time.vals = seq.POSIXt(from=as.POSIXct("200001010000", format="%Y%m%d%H%M", tz="UTC"),
                         by=3600, to=as.POSIXct("201012312300", format="%Y%m%d%H%M", tz="UTC"))
  ts.na = xts(numeric(length=length(time.vals))*NA, order.by=time.vals)

  if (!is.null(era10.xts)) {
    era10.xts = merge.xts(ts.na, era10.xts, join="left")[,2]
  }
  if (!is.null(era100.xts)) {
    era100.xts = merge.xts(ts.na, era100.xts, join="left")[,2]
  }

  return(list(era10=era10.xts, era100=era100.xts))
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
#' @param read.10m,read.100m optional boolean parameters setting whether to read
#'   data at only 10m, only 100m, or at both heights. The default setting is to
#'   read 10m and not to read 100m data.
#' @param verb.dat optional boolean to determine whether to print out what's going
#'   on (T). The default is to suppress printing (verb.dat = FALSE).
#' @return A named list holding the extended time series of the ERA data at 10m
#'   height for ERA-Interim and 10m and 100m for ERA20C.: (era10=,era20c100=). If
#'   ERA-Interim data is read, a NULL value will be returned for era20c100.
#' @importFrom zoo as.yearmon
#' @importFrom xts xts
#' @export
ReadEraNetcdf2Xts <- function(era.param, era.fname,
                              era.tsstart, era.tsend,
                              lonidx, latidx, ana.time.res,
                              read.10m=FALSE, read.100m=FALSE, verb.dat=FALSE) {

  # sanity check
  if (!(read.10m) & !(read.100m)) {
    CallStop(paste0("You need to read at least one of 10m or 100m data! ",
                    "Now both are set to FALSE "))
  }

  # Read ERA-I or ERA20C monthly or daily data
  if (read.10m) {
    era10m = ReadNetcdf(era.param[1], era.fname[1], count=c(1,1,-1),
                        start=c(lonidx, latidx, 1), verb.dat=verb.dat)
  }
  if (read.100m) {
    era100m = ReadNetcdf(era.param[2], era.fname[2], count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
  }
  if (read.10m & !(read.100m)) {
    df = data.frame(era10m$time, era10m$data)
  } else if (read.100m & !(read.10m)) {
    df = data.frame(era100m$time, era100m$data)
  } else if (read.10m & read.100m) {
    df = data.frame(era10m$time, era10m$data, era100m$data)
  }

  # convert ERA data and time values into an extended time series
  # and apply start and end date
  if (ana.time.res$time.res == ana.time.res$monthly) {
    if (read.100m & !(read.10m)) {
      df$era100m.time = as.yearmon(df$era100m.time)
    } else {
      df$era10m.time = as.yearmon(df$era10m.time)
    }
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    if (read.100m & !(read.10m)) {
      df$era100m.time = as.POSIXct(strptime(df$era100m.time, format="%Y-%m-%d"),
                                   format="%Y-%m-%d", tz = "UTC")
    } else {
      df$era10m.time = as.POSIXct(strptime(df$era10m.time, format="%Y-%m-%d"),
                                  format="%Y-%m-%d", tz = "UTC")
    }
  }
  timestr = SetToDate(era.tsstart, era.tsend)
  era10m.xts = NULL
  era100m.xts = NULL
  if (read.10m) {
    era10m.xts = xts(df$era10m.data, order.by=df$era10m.time)
    era10m.xts = era10m.xts[timestr]
  }
  if (read.100m) {
    if (read.100m & !(read.10m)) {
      era100m.xts = xts(df$era100m.data, order.by=df$era100m.time)
    } else {
      era100m.xts = xts(df$era100m.data, order.by=df$era10m.time)
    }
    era100m.xts = era100m.xts[timestr]
  }

  return(list(era10=era10m.xts, era100=era100m.xts))
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
#'   the format c(yyyy,mm) of the tower measurements, HErZ, and ERA20C reanalyses
#' @param tower.name string holding the tower name; if an unexpected name is passed
#'   the function will terminate execution
#' @return The return value is a data frame which holds the available data of the
#'   wind speed profile at different height levels which are named columns of
#'   tower measurements and reanalysis data
#' @importFrom zoo index coredata
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

#' @title Create climate objects of passed wind speed data of up to all six levels.
#' @description Wind speed observation data of up to six height levels (tower) or
#'   only one (station data) is passed together with HErZ data at up to six height
#'   levels. Optionally, it is possible to pass ERA20C at 10m and 100m and
#'   ERA-Interim at 10m. All this data is eventually copied into one climate data
#'   object with additional information like lat, lon, etc.
#' @param obs.xts,obs2.xts,obs3.xts,obs4.xts,obs5.xts,obs6.xts extended time series
#'   of observation data as availabel; if not available it is set to NULL.
#' @param herz10.xts,herz35.xts,herz69.xts,herz116.xts,herz178.xts,herz258.xts
#'   extended time series of HErZ data at different height levels if available; if
#'   not available it is set to NULL.
#' @param eraI10.xts,eraI100.xts extended time series of ERA-Interim data at 10m and
#'   100m, respectively.
#' @param era20c10.xts,era20c100.xts extended time series of ERA20C data at 10m and
#'   100m, respectively.
#' @param obs.tsstart start time in the format c(yyyy,mm) of the observation data
#' @param obs.tsend,herz.tsend,eraI.tsend,era20c.tsend end time in the format
#'   c(yyyy,mm) of the observations, HErZ, ERA_Interim, and ERA20C reanalyses,
#'   resectively.
#' @param obs.name a string holding the name of the tower observation.
#' @param obs.lon,obs.lat lon, lat information of the tower observation.
#' @param obs.param,eraI.param,era20c.param a string holding the parameter name of
#'   the tower measurements, ERA-Interim, and ERA20C, respectively.
#' @param herz.profile is a boolean which indicated whether to read the complete
#'   HErZ profile (T) (six height levels) or only 10m and 116m (F).
#' @param only.10m is a boolean which indicates whether to read only 10m wind speed
#'   data (T) or not (F). This has only an effect if herz.profile=F. Default value
#'   is only.10m=F.
#' @return a list which holds the climate data object.
#' @importFrom zoo index coredata
#' @export
GetObsObject <- function(obs.xts, obs2.xts=NULL, obs3.xts=NULL,
                         obs4.xts=NULL, obs5.xts=NULL, obs6.xts=NULL,
                         herz10.xts, herz35.xts=NULL, herz69.xts=NULL, herz116.xts,
                         herz178.xts=NULL, herz258.xts=NULL,
                         eraI10.xts=NULL, eraI100.xts=NULL,
                         era20c10.xts=NULL, era20c100.xts=NULL,
                         obs.tsstart, obs.tsend, herz.tsend,
                         eraI.tsend=NULL, era20c.tsend=NULL,
                         obs.name="", obs.lon, obs.lat,
                         obs.param, eraI.param=NULL, era20c.param=NULL,
                         herz.profile, only.10m=FALSE) {

  # -- checks on input variables
  if (nchar(obs.name) == 0 | obs.name != "Fino1" & obs.name != "Fino2" &
      obs.name != "Lindenberg" & obs.name != "Cabauw") {
    cat("\n   *** The observation name is: ", obs.name, "  ***\n\n")
  }
  if (is.null(eraI.tsend) & is.null(era20c.tsend)) {
    tsend = c(min(obs.tsend[1], herz.tsend[1]), 12)
  } else if (is.null(eraI.tsend) & !is.null(era20c.tsend)) {
    tsend = c(min(obs.tsend[1], herz.tsend[1], era20c.tsend[1]), 12)
  } else if (!is.null(eraI.tsend) & is.null(era20c.tsend)) {
    tsend = c(min(obs.tsend[1], herz.tsend[1], eraI.tsend[1]), 12)
  } else if (!is.null(eraI.tsend) & !is.null(era20c.tsend)) {
    tsend = c(min(obs.tsend[1], herz.tsend[1], era20c.tsend[1], eraI.tsend[1]), 12)
  }
  timestr = paste0(toString(obs.tsstart[1]), toString(obs.tsstart[2]), '/',
                   toString(tsend[1]), toString(tsend[2]))
  if (herz.profile) only.10m = FALSE

  # -- assigne time series in time period provided
  obs.xts = obs.xts[timestr]
  if (!is.null(obs2.xts)) obs2.xts = obs2.xts[timestr]
  if (!is.null(obs3.xts)) obs3.xts = obs3.xts[timestr]
  if (!is.null(obs4.xts)) obs4.xts = obs4.xts[timestr]
  if (!is.null(obs5.xts)) obs5.xts = obs5.xts[timestr]
  if (!is.null(obs6.xts)) obs6.xts = obs6.xts[timestr]
  herz10.xts = herz10.xts[timestr]
  if (!only.10m) {
    herz116.xts = herz116.xts[timestr]
  }
  if (herz.profile) {
    herz35.xts = herz35.xts[timestr]
    herz69.xts = herz69.xts[timestr]
    herz178.xts = herz178.xts[timestr]
    herz258.xts = herz258.xts[timestr]
  }
  if (!is.null(eraI10.xts)) eraI10.xts = eraI10.xts[timestr]
  if (!is.null(eraI100.xts)) eraI100.xts = eraI100.xts[timestr]
  if (!is.null(era20c10.xts)) era20c10.xts = era20c10.xts[timestr]
  if (!is.null(era20c100.xts)) era20c100.xts = era20c100.xts[timestr]

  # -- create data frames to be saved into clim objects
  obs.df = data.frame(date=index(obs.xts),
                      ReanaName="", StationName=obs.name,
                      latitude=obs.lat, longitude=obs.lon,
                      wind_speed=coredata(obs.xts),
                      height=strsplit(obs.param, '_')[[1]][[2]])
  if (!is.null(obs2.xts)) {
    obs2.df = data.frame(date=index(obs2.xts),
                         ReanaName="", StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(obs2.xts),
                         height=strsplit(obs.param, '_')[[2]][[2]])
  }
  if (!is.null(obs3.xts)) {
    obs3.df = data.frame(date=index(obs3.xts),
                         ReanaName="", StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(obs3.xts),
                         height=strsplit(obs.param, '_')[[3]][[2]])
  }
  if (!is.null(obs4.xts)) {
    obs4.df = data.frame(date=index(obs4.xts),
                         ReanaName="", StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(obs4.xts),
                         height=strsplit(obs.param, '_')[[4]][[2]])
  }
  if (!is.null(obs5.xts)) {
    obs5.df = data.frame(date=index(obs5.xts),
                         ReanaName="", StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(obs5.xts),
                         height=strsplit(obs.param, '_')[[5]][[2]])
  }
  if (!is.null(obs6.xts)) {
    obs6.df = data.frame(date=index(obs6.xts),
                         ReanaName="", StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(obs6.xts),
                         height=strsplit(obs.param, '_')[[6]][[2]])
  }

  herz10.df = data.frame(date=index(herz10.xts),
                         ReanaName="HErZ", StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(herz10.xts),
                         height="10m")
  if (!only.10m) {
    herz116.df = data.frame(date=index(herz116.xts),
                            ReanaName="HErZ", StationName=obs.name,
                            latitude=obs.lat, longitude=obs.lon,
                            wind_speed=coredata(herz116.xts),
                            height="116m")
  }
  if (herz.profile) {
    herz35.df = data.frame(date=index(herz35.xts),
                           ReanaName="HErZ", StationName=obs.name,
                           latitude=obs.lat, longitude=obs.lon,
                           wind_speed=coredata(herz35.xts),
                           height="35m")
    herz69.df = data.frame(date=index(herz69.xts),
                           ReanaName="HErZ", StationName=obs.name,
                           latitude=obs.lat, longitude=obs.lon,
                           wind_speed=coredata(herz69.xts),
                           height="69m")
    herz178.df = data.frame(date=index(herz178.xts),
                            ReanaName="HErZ", StationName=obs.name,
                            latitude=obs.lat, longitude=obs.lon,
                            wind_speed=coredata(herz178.xts),
                            height="178m")
    herz258.df = data.frame(date=index(herz258.xts),
                            ReanaName="HErZ", StationName=obs.name,
                            latitude=obs.lat, longitude=obs.lon,
                            wind_speed=coredata(herz258.xts),
                            height="258m")
  }

  if (!is.null(eraI10.xts)) {
    eraI10.df = data.frame(date=index(eraI10.xts),
                           ReanaName="ERA-I", StationName=obs.name,
                           latitude=obs.lat, longitude=obs.lon,
                           wind_speed=coredata(eraI10.xts),
                           height=strsplit(eraI.param, '_')[[1]][[2]])
    colnames(eraI10.df)[6] = "wind_speed"
  }
  if (!is.null(era20c10.xts)) {
    era20c10.df = data.frame(date=index(era20c10.xts),
                             ReanaName="ERA20C", StationName=obs.name,
                             latitude=obs.lat, longitude=obs.lon,
                             wind_speed=coredata(era20c10.xts),
                             height=strsplit(era20c.param, '_')[[1]][[2]])
    colnames(era20c10.df)[6] = "wind_speed"
  }
  if (!is.null(eraI100.xts)) {
    eraI100.df = data.frame(date=index(eraI100.xts),
                            ReanaName="ERA-I", StationName=obs.name,
                            latitude=obs.lat, longitude=obs.lon,
                            wind_speed=coredata(eraI100.xts),
                            height=strsplit(eraI.param, '_')[[2]][[2]])
    colnames(eraI100.df)[6] = "wind_speed"
  }
  if (!is.null(era20c100.xts)) {
    era20c100.df = data.frame(date=index(era20c100.xts),
                              ReanaName="ERA20C", StationName=obs.name,
                              latitude=obs.lat, longitude=obs.lon,
                              wind_speed=coredata(era20c100.xts),
                              height=strsplit(era20c.param, '_')[[2]][[2]])
    colnames(era20c100.df)[6] = "wind_speed"
  }

  # -- create clim objects
  if (!is.null(obs6.xts)) {
    if (!is.null(era20c10.xts) & !is.null(era20c100.xts) & !is.null(eraI10.xts) &
        !is.null(eraI100.xts)) {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, obs2=obs2.df,
                                          obs3=obs3.df, obs4=obs4.df,
                                          obs5=obs5.df, obs6=obs6.df,
                                          herz10=herz10.df, herz35=herz35.df,
                                          herz69=herz69.df, herz116=herz116.df,
                                          herz178=herz178.df, herz258=herz258.df,
                                          eraI10=eraI10.df,
                                          eraI100=eraI100.df,
                                          era20c10=era20c10.df,
                                          era20c100=era20c100.df))
    }  else {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, obs2=obs2.df,
                                          obs3=obs3.df, obs4=obs4.df,
                                          obs5=obs5.df, obs6=obs6.df,
                                          herz10=herz10.df, herz35=herz35.df,
                                          herz69=herz69.df, herz116=herz116.df,
                                          herz178=herz178.df, herz258=herz258.df))
    }
  } else {
    if (!is.null(era20c10.xts) & !is.null(era20c100.xts) & !is.null(eraI10.xts) &
        !is.null(eraI100.xts)) {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, herz10=herz10.df,
                                          herz35=herz35.df, herz69=herz69.df,
                                          herz116=herz116.df, herz178=herz178.df,
                                          herz258=herz258.df, eraI10=eraI10.df,
                                          eraI100=eraI100.df,
                                          era20c10=era20c10.df,
                                          era20c100=era20c100.df))
    } else {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, herz10=herz10.df,
                                          herz35=herz35.df, herz69=herz69.df,
                                          herz116=herz116.df, herz178=herz178.df,
                                          herz258=herz258.df))
    }
  }

  return(list(obs.object = climate.obs.object))

}

#-----------------------------------------------------------------------------------

#' @title Create climate objects of passed 10m wind speed data.
#' @description Same as function \code{\link{GetObsObject}} but for 10m RRA data only.
#' @param obs.xts extended time series of observation data.
#' @param rra10.xts,rra10.hourly.xts,stats.atrra10.xts extended time series of
#'   regional reanalysis at 10m, the same for hourly data, and observation data at
#'   10m but only at time steps of the RRA, respectively.
#' @param eraI10.xts extended time series of ERA-Interim data at 10m.
#' @param obs.tsstart start time in the format c(yyyy,mm) of the observation data
#' @param obs.tsend,rra.tsend,eraI.tsend end time in the format
#'   c(yyyy,mm) of the observations, regional reanalysis, and ERA_Interim reanalyses,
#'   resectively.
#' @param rra.name,eraI.name,obs.name are strings holding the names of the regional
#'   reanalysis, ERA-Interim, and the observation source, respectively.
#' @param obs.lon,obs.lat numeric values holding the longitude and latitude values of
#'   observation location, respectively.
#' @param obs.param,eraI.param are strings holding the parameter names of the
#'   observations and ERA-Interim, respectively.
#' @return a list which holds the climate data object.
#' @importFrom zoo index coredata
#' @export
Get10mRRAObsObject <- function(obs.xts, rra10.xts, rra10.hourly.xts=NULL,
                               stats.atrra10.xts=NULL, eraI10.xts=NULL,
                               obs.tsstart, obs.tsend,
                               rra.tsend, eraI.tsend=NULL,
                               rra.name="", eraI.name="", obs.name="",
                               obs.lon, obs.lat,
                               obs.param, eraI.param=""){

  tsend = c(min(obs.tsend[1], rra.tsend[1], eraI.tsend[1]), 12)
  timestr = paste0(toString(obs.tsstart[1]), toString(obs.tsstart[2]), '/',
                   toString(tsend[1]), toString(tsend[2]))

  # -- assigne time series in time period provided
  obs.xts = obs.xts[timestr]
  rra10.xts = rra10.xts[timestr]
  if (!is.null(rra10.hourly.xts)) rra10.hourly.xts = rra10.hourly.xts[timestr]
  if (!is.null(stats.atrra10.xts)) stats.atrra10.xts = stats.atrra10.xts[timestr]
  if (!is.null(eraI10.xts)) eraI10.xts = eraI10.xts[timestr]

  # -- create data frames to be saved into clim objects
  obs.df = data.frame(date=index(obs.xts),
                      ReanaName=rra.name, StationName=obs.name,
                      latitude=obs.lat, longitude=obs.lon,
                      wind_speed=coredata(obs.xts),
                      height="10m")

  rra10.df = data.frame(date=index(rra10.xts),
                        ReanaName=rra.name, StationName=obs.name,
                        latitude=obs.lat, longitude=obs.lon,
                        wind_speed=coredata(rra10.xts),
                        height="10m")

  if (!is.null(rra10.hourly.xts)) {
    rra10.hourly.df = data.frame(date=index(rra10.hourly.xts),
                                 ReanaName=rra.name, StationName=obs.name,
                                 latitude=obs.lat, longitude=obs.lon,
                                 wind_speed=coredata(rra10.hourly.xts),
                                 height="10m")
    col.names = c("date", "ReanaName", "StationName", "latitude", "longitude",
                  "wind_speed", "height")
    colnames(rra10.hourly.df) = col.names
  }

  if (!is.null(stats.atrra10.xts)) {
    stats.atrra10.df = data.frame(date=index(stats.atrra10.xts),
                                  ReanaName=rra.name, StationName=obs.name,
                                  latitude=obs.lat, longitude=obs.lon,
                                  wind_speed=coredata(stats.atrra10.xts),
                                  height="10m")
  }

  if (!is.null(eraI10.xts)) {
    eraI10.df = data.frame(date=index(eraI10.xts),
                         ReanaName=eraI.name, StationName=obs.name,
                         latitude=obs.lat, longitude=obs.lon,
                         wind_speed=coredata(eraI10.xts),
                         height="10m")
  }

  # -- create clim objects
  if (!is.null(eraI10.xts)) {
    if (!is.null(rra10.hourly.xts) & !is.null(stats.atrra10.xts)) {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, rra10=rra10.df,
                                          rra10.hourly=rra10.hourly.df,
                                          stats.atrra10=stats.atrra10.df,
                                          eraI10=eraI10.df))
    } else {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, rra10=rra10.df,
                                          eraI10=eraI10.df))
    }
  } else {
    if (!is.null(rra10.hourly.xts) & !is.null(stats.atrra10.xts)) {
      climate.obs.object = climate(data_tables=
                                   list(obs=obs.df, rra10=rra10.df,
                                        rra10.hourly=rra10.hourly.df,
                                        stats.atrra10=stats.atrra10.df))
    } else {
      climate.obs.object = climate(data_tables=
                                     list(obs=obs.df, rra10=rra10.df))
    }
  }

  return(list(obs.object = climate.obs.object))
}

#-----------------------------------------------------------------------------------

#' @title Create a climate data object for random data.
#' @param obs.xts,forec.xts observation and forecast data holding random data.
#' @return a list which holds the climate data object.
#' @importFrom zoo index coredata
#' @export
GetRandomClimObject <- function(obs.xts, forec.xts) {

  obs.df = data.frame(date=index(obs.xts),
                      StationName="random data",
                      wind_speed=coredata(obs.xts))
  forec.df = data.frame(date=index(forec.xts),
                        StationName="random data",
                        wind_speed=coredata(forec.xts))
  climate.random.object = climate(data_tables=
                                    list(obs=obs.df, forec=forec.df))

  return(list(random.object = climate.random.object))
}

#-----------------------------------------------------------------------------------

#' @title Create hourly RRA time series with NA fill values.
#' @description The newly aligned RRA data is filled with NA values at time steps
#'   at which there is no observations data available (and also set to NA).
#' @param stat.xts extended time series of station data.
#' @param rra.xts extended time series of RRA data.
#' @return an extended time series holding the aligned RRA data.
#' @importFrom xts merge.xts
#' @export
AligneRRA2Obsxts <- function(stat.xts, rra.xts) {

  merged.xts = merge.xts(rra.xts, stat.xts)
  aligned.rra.xts = merged.xts$rra.xts

  return(aligned.rra.xts)
}

#-----------------------------------------------------------------------------------

#' @title Get station values only at RRA time steps.
#' @description The newly aligned station data spans the same time period as the
#'   RRA data.
#' @param stat.xts extended time series of station data.
#' @param rra.xts extended time series of RRA data.
#' @return an extended time series holding the newly aligned station values.
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
AligneObs2RRAxts <- function(stat.xts, rra.xts) {

  new.vector = vector(mode="numeric", length=length(rra.xts))*NA
  cnt = 1
  for (n.step in seq(stat.xts)) {
    # this is especially needed for SMHI which does not span all of 2008 to 2009
    if (cnt > length(rra.xts)) break
    if (index(stat.xts)[[n.step]] == index(rra.xts)[[cnt]]) {
      new.vector[cnt] = stat.xts[[n.step]]
      cnt = cnt + 1
    }
  }
  new.xts = xts(new.vector, order.by=index(rra.xts))
  return(new.xts)
}

#-----------------------------------------------------------------------------------
