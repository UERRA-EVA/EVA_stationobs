#' @title Stop on error and print error message.
#' @description This function stops and prints an error message which has been
#'   passed to it. It does not have a return value.
#' @param errstr is a string which hold the error message to be printed before
#'   stopping execution
CallStop <- function(errstr) {
  err = simpleError(paste0("\n   ***\n   ", errstr, "\n   ***\n"))
  tryCatch(stop(err))
}

#-----------------------------------------------------------------------------------

#' @title Check for existence of file.
#' @description \code{CheckFile} checks for the existence of the file(s) passed to
#' it. If one file doese not exist it stops with an error message.
#' @param infile Character string or concatenated character strings holding the file
#' name(s).
CheckFile <- function(infile) {
  if (!class(infile) == "character") {
    CallStop(paste0("Unexpected type of infile, ABORTING!\n",
                    "   ", class(infile), "\n   should be character"))
  }
  if (any(!file.exists(infile))) {
    idx = which(!file.exists(infile))
    missing.file = infile[idx]
    CallStop(paste0("Missing File: ", infile[idx], "ABORTING!\n"))
  }
}

#-----------------------------------------------------------------------------------

#' @title Check HErZ parameter names.
#' @description HErZ parameter names are checked which is especially important if
#'   the complete profile is passed. Within the package the names are expected to
#'   follow a certain structure: "windspeed_xxxm", whereas xxx equals the height of
#'   the model level and is expected to be increasing, i.e., 10m, 35m, 69m, 116m,
#'   178m, 258m. This function does not have a return value.
#' @param herz.param a string of length n which holds n parameter names.
#' @param herz.profile boolean which determines whether to check for the complete
#'   HErZ profile or only the two levels 10m and 116m.
CheckHerzParams <- function(herz.param, herz.profile) {
  if (herz.profile) {
    if (!herz.param[1] == "windspeed_10m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[1],
                      "\n    Should be: windspeed_10m"))
    }
    if (!herz.param[2] == "windspeed_35m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[2],
                      "\n    Should be: windspeed_35m"))
    }
    if (!herz.param[3] == "windspeed_69m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[3],
                      "\n    Should be: windspeed_69m"))
    }
    if (!herz.param[4] == "windspeed_116m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[4],
                      "\n    Should be: windspeed_116m"))
    }
    if (!herz.param[5] == "windspeed_178m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[5],
                      "\n    Should be: windspeed_178m"))
    }
    if (!herz.param[6] == "windspeed_258m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[6],
                      "\n    Should be: windspeed_258m"))
    }
  } else {
    if (!herz.param[1] == "windspeed_10m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[1],
                      "\n    Should be: windspeed_10m"))
    }
    if (!herz.param[2] == "windspeed_116m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[2],
                      "\n    Should be: windspeed_116m"))
    }
  }
}

#-----------------------------------------------------------------------------------

#' @title Get nearest neighbor index of a point within a vector.
#' @description \code{get.lonidx} calculates the nearest neighbor of a point within
#' a vector. Here, it is used to find the index within a regular lon, lat grid of a
#' specified lon, lat point. This function needs to be executed separately for each
#' index. The value of the point needs to be within the range of the vector.
#' @param vec vector of regularly spaced values
#' @param num numeric to find the nearest index within the provided vector.
#' @return idx the nearest index of \code{num} within \code{vec}.
get.nearest.idx <- function(vec, num) {
  idx = which( abs(vec - num) == min(abs(vec - num)) )
  return(idx)
}

#-----------------------------------------------------------------------------------

#' @title Extract longitude and latitude index off a lon, lat regular grid.
#' @description \code{get.lon.lat.idx} extracts the longitude and latitude index of
#' a point off a netCDF file which holds data on a regular grid with longitude and
#' latitude values (as vectors) stored.
#' @param fname string of the file name to read
#' @param point.lon longitude point which to match and extract off the grid
#' @param point.lat latitude point as above
#' @param grid.lon optional parameter of the grid longitude values. If the grid#
#'   ongitude is a vector and can be read from nc file this is done and the grid.lon
#'   parameter needs to be set to NULL. The default is to set grid.lon to NULL.
#' @param grid.lat same as above for the grid latitude values.
#' @return Return a named lilst of (lonidx=,latidx=) longitude and latitude index.
get.lon.lat.idx <- function(fname, point.lon, point.lat,
                            grid.lon=NULL, grid.lat=NULL) {
  CheckFile(fname)
  if (is.null(grid.lon)) { # ERA20C, ERA-Interim data
    dat = ReadNetcdfLonLat(fname)
    grid.lon = dat$lon
    grid.lat = dat$lat
    latidx = get.nearest.idx(grid.lat, point.lat)
    lonidx = get.nearest.idx(grid.lon, point.lon)
    return(list(lonidx=lonidx, latidx=latidx))
  } else { # HErZ data
    dist.to.point = getNearest(point.lat, grid.lat, point.lon, grid.lon)
    index.at.point = arrayInd(which.min(dist.to.point), dim(dist.to.point))
    return(list(lonidx=index.at.point[1], latidx=index.at.point[2]))
  }
}

#-----------------------------------------------------------------------------------

#' @title Determine start/end string to set extended time series.
#' @description A start and end date in the format of c(YYYY,M) are put together in
#'   a string of the format YYYYM/YYYYM.
#' @param tsstart numeric of the format c(YYYY,M) holding the start date
#' @param tsend as above holding the end date
#' @return timestr string of the format YYYYM/YYYYM holding start date/end date.
set.to.date <- function(tsstart, tsend) {
  timestr = paste0(toString(tsstart[1]), toString(tsstart[2]), '/',
                   toString(tsend[1]), toString(tsend[2]))
  return(timestr)
}

#-----------------------------------------------------------------------------------

#' @title Extract seasonal time series off a monthly time series.
#' @description This fucntion extracts a seasonal time series off a monthly time
#'   series by using the function split.xts. However, it only splits into calendar
#'   quarters, not into meteorological seasons. The return values are the separate
#'   seasonal (quarterly) time series.
#' @param vals.xts is the input extended time series which is divided into seasonal
#'   (quarterly) xts.
#' @return A named list returning a winter, spring, summer, and autumn extended
#'  time series which was split off the input xts.
#' @note could be enhanced by manually splitting into meteorol. seasons DJF, MAM,
#'   JJA, SON instead of using the generic split 'f="quarters"'.
GetSeasonalXts <- function(vals.xts) {

  vals.split.xts = split.xts(vals.xts, f="quarters")
  vals.time = vector(mode="character", length=length(vals.split.xts))
  vals.values = vector(mode="numeric", length=length(vals.split.xts))
  for (ii in seq(length(vals.split.xts))) {
    vals.time[[ii]] = as.character(index(vals.split.xts[[ii]])[3])
    vals.values[[ii]] = mean(vals.split.xts[[ii]])
  }
  vals.time = as.POSIXlt(as.yearmon(vals.time, format="%b %Y"), format="%Y-%m-%d")
  vals.xts = as.xts(vals.values, order.by=vals.time)

  # e.g., winter.xts is up to March, including JFM
  winter.xts  = vals.xts[which(index(vals.xts)$mon==2)]
  spring.xts  = vals.xts[which(index(vals.xts)$mon==5)]
  summer.xts  = vals.xts[which(index(vals.xts)$mon==8)]
  autumn.xts  = vals.xts[which(index(vals.xts)$mon==11)]

  return(list(winter.xts=winter.xts, spring.xts=spring.xts,
              summer.xts=summer.xts, autumn.xts=autumn.xts))

}

#-----------------------------------------------------------------------------------
