#' @title Read monthly or daily HErZ data and convert to xts
#' @description This function reads HErZ reanalysis data at the point of the station
#'   location of monthly or daily data and for the complete profile consisting of
#'   six model levels (which are hard coded into variable names here because they
#'   are assumed to stay the way they are) or only the 10m and 116m model level.
#'   After reading and temporarily storing into a data frame, the return values of
#'   this function are extended time series holding the complete time series (time)
#'   at each model level (windspeed). The time steps of the monthly time series is
#'   given with precision of months, those of daily time series with precision of
#'   days.
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
#' @param era.monthly boolean to determine whether to read daily or mothly HErZ data.
#' @param herz.profile boolean to determine whether to read HErZ profile (six model
#'   levels) or only the two specified levels (10m and 116m).
#' @param verb.dat optional boolean to determine whether to print out what's going
#'   on (T). The default is to suppress printing (verb.dat = FALSE).
#' @return A named list holding the extended time series of HErZ data at the
#'   different model levels: (herz10=,herz35=,herz69=,herz116=,herz178,herz258). For
#'   unspecified model levels (e.g., if(!herz.profile), a NULL value will be
#'   returned.
ReadHerzNetcdfMonthlyDaily2Xts <- function(herz.param, herz.fname,
                                           herz.tsstart, herz.tsend,
                                           lonidx, latidx,
                                           era.monthly, herz.profile,
                                           verb.dat=FALSE) {

  # read HErZ data into a data.frame
  if (era.monthly) { # monthly
    if (herz.profile) { # monthly profile
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

    } else { # monthly 10m, 116m

      dat10 = ReadNetcdf(herz.param[1], herz.fname, count=c(1,1,-1),
                         start=c(lonidx, latidx, 1), verb.dat=verb.dat)
      dat116 = ReadNetcdf(herz.param[2], herz.fname, count=c(1,1,-1),
                          start=c(lonidx, latidx, 1), verb.dat=verb.dat)

      ndf = data.frame(dat10$time, dat10$data, dat116$data)
    }

  } else { # daily

    if (herz.profile) { # daily profile
      ndf = data.frame()
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

    } else { # daily 10m, 116m

      ndf = data.frame()
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
  if (era.monthly) {
    ndf$dat10.time = as.yearmon(ndf$dat10.time)
  } else {
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
#' @param era.monthly boolean to determine whether to read daily or mothly HErZ data.
#' @param era20c boolean to determine whether the data is ERA20C (T) or
#'   ERA-Interim (F).
#' @param verb.dat optional boolean to determine whether to print out what's going
#'   on (T). The default is to suppress printing (verb.dat = FALSE).
#' @return A named list holding the extended time series of the ERA data at 10m
#'   height for ERA-Interim and 10m and 100m for ERA20C.: (era10=,era20c100=). If
#'   ERA-Interim data is read, a NULL value will be returned for era20c100.
ReadEraNetcdf2Xts <- function(era.param, era.fname,
                              era.tsstart, era.tsend,
                              lonidx, latidx, era.monthly,
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
  if (era.monthly) {
    df$era10m.time = as.yearmon(df$era10m.time)
  } else {
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

#' @title
#' @description
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @param
#' @return
GetTowerProfileTS <- function(tower.xts, tower2.xts=NULL, tower3.xts=NULL,
                              tower4.xts=NULL, tower5.xts=NULL, tower6.xts=NULL,
                              herz10.xts, herz35.xts, herz69.xts, herz116.xts,
                              herz178.xts, herz258.xts, era20c100.xts,
                              tower.tsstart, tower.tsend, herz.tsend, era20c.tsend,
                              tower.name=NULL) {

  if (!is.null(tower.name) & tower.name != "Fino1" & tower.name != "Fino2" &
        tower.name != "Lindenberg") {
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
  if (tower.name == "Lindenberg") {
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
  }
  df$herz10 = coredata(herz10.xts)
  df$herz35 = coredata(herz35.xts)
  df$herz69 = coredata(herz69.xts)
  df$herz116 = coredata(herz116.xts)
  df$herz178 = coredata(herz178.xts)
  df$herz258 = coredata(herz258.xts)
  df$era20c100 = coredata(era20c100.xts)

  return(df)
}

#-----------------------------------------------------------------------------------
