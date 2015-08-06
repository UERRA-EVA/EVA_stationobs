#' @title
#' @description
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

#' @title
#' @description
#' @param herz.param
#' @param herz.profile
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
