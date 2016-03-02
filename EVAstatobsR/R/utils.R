#' @title Various settings for plotting.
#' @description This function provides a named list which holds settings for
#'   plotting. Default values are set and will be adjusted depending on input data.
#'   As of now it is divided only into station and tower data.
#' @param data.vals is a data input object. It is either of class xts (which is still
#'   the case for station data) or a data frame as the data part of the ClimObject
#'   which is the new normal and already works for tower data.
#' @return is a named list holding all the parameters.
#' @importFrom xts is.xts
PlottingSettings <- function(data.vals) {

  land.a4width = 29.7/2.54
  land.a4height = 21./2.54
  port.a4width = 21./2.54
  port.a4height = 29.7/2.54
  square.a4 = 20./2.54

  col.herz = "green"
  col.smhi = "deepskyblue"
  col.mo = "red"
  col.mf = "magenta"
  col.light.obs = "gray70"

  if (ana.time.res$time.res == ana.time.res$monthly) {
    time.agg = "monthly"
    time.Agg = "Monthly"
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    time.agg = "daily"
    time.Agg = "Daily"
  } else if (ana.time.res$time.res == ana.time.res$hourly) {
    time.agg = "hourly"
    time.Agg = "Hourly"
  }

  if (is.data.frame(data.vals)) {
    obs.name = data.vals$StationName[1]
    obs.height = data.vals$height[1]
    rea.name = data.vals$ReanaName[1]
    return(list(land.a4width=land.a4width, land.a4height=land.a4height,
                port.a4width=port.a4width, port.a4height=port.a4height,
                square.a4=square.a4,
                time.agg=time.agg, time.Agg=time.Agg, obs.name=obs.name,
                obs.height=obs.height, rea.name=rea.name,
                col.herz=col.herz, col.smhi=col.smhi, col.mo=col.mo,
                col.mf=col.mf, col.light.obs=col.light.obs))
  } else if (is.xts(data.vals)) {
    return(list(land.a4width=land.a4width, land.a4height=land.a4height,
                port.a4width=port.a4width, port.a4height=port.a4height,
                square.a4=square.a4,
                time.agg=time.agg, time.Agg=time.Agg,
                col.herz=col.herz, col.smhi=col.smhi, col.mo=col.mo,
                col.mf=col.mf, col.light.obs=col.light.obs))
  } else {
    CallStop(paste0("Expected either a data.frame or xts; data.vals is class ",
                    class(data.vals)))
  }
}

#-----------------------------------------------------------------------------------

#' @title Stop on error and print error message.
#' @description This function stops and prints an error message which has been
#'   passed to it. It does not have a return value.
#' @param errstr is a string which hold the error message to be printed before
#'   stopping execution
#' @export
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
#' @export
CheckFile <- function(infile) {
  if (!class(infile) == "character") {
    CallStop(paste0("Unexpected type of infile, ABORTING!\n",
                    "   ", class(infile), "\n   should be character"))
  }
  if (any(!file.exists(infile))) {
    idx = which(!file.exists(infile))
    missing.file = infile[idx]
    CallStop(paste0("Missing File: #->", missing.file, "<-#  ABORTING!\n"))
  }
}

#-----------------------------------------------------------------------------------

#' @title Check HErZ parameter names.
#' @description HErZ parameter names are checked which is especially important if
#'   the complete profile is passed. Within the package the names are expected to
#'   follow a certain structure: "windspeed_xxxm", where xxx equals the height of
#'   the model level and is expected to be decreasing, i.e., 258m, 178m, 116m, 69m,
#'   35m, 10m. This function does not have a return value.
#' @param herz.param a string of length n which holds n parameter names.
#' @param herz.profile boolean which determines whether to check for the complete
#'   HErZ profile or only the two levels 10m and 116m.
#' @export
CheckHerzParams <- function(herz.param, herz.profile) {
  if (herz.profile) {
    if (!herz.param[1] == "windspeed_258m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[1],
                      "\n   Should be: windspeed_258m"))
    }
    if (!herz.param[2] == "windspeed_178m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[2],
                      "\n   Should be: windspeed_178m"))
    }
    if (!herz.param[3] == "windspeed_116m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[3],
                      "\n   Should be: windspeed_116m"))
    }
    if (!herz.param[4] == "windspeed_69m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[4],
                      "\n   Should be: windspeed_69m"))
    }
    if (!herz.param[5] == "windspeed_35m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[5],
                      "\n   Should be: windspeed_35m"))
    }
    if (!herz.param[6] == "windspeed_10m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[6],
                      "\n   Should be: windspeed_10m"))
    }
  } else {
    if (!herz.param[1] == "windspeed_116m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[1],
                      "\n   Should be: windspeed_116m"))
    }
    if (!herz.param[2] == "windspeed_10m") {
      CallStop(paste0("Unexpected HErZ parameter: ", herz.param[2],
                      "\n   Should be: windspeed_10m"))
    }
  }
}

#-----------------------------------------------------------------------------------

#' @title Check HErZ file height order.
#' @description HErZ file names are checked for the correct height order which is
#'   especially important if the complete profile is passed. Within the package the
#'   file names are expected to be decreasing, i.e., 258m, 178m, 116m, 69m,
#'   35m, 10m. This function does not have a return value.
#' @param fname a string of length n which holds n file names.
#' @param herz.profile boolean which determines whether to check for the complete
#'   HErZ profile or only the two levels 10m and 116m.
#' @export
CheckHerzHeightOrder <- function(fname, herz.profile) {
  if (herz.profile) {
    if (!grepl("258m", fname[1])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[1],
                      "\n   Should be: 258m"))
    }
    if (!grepl("178m", fname[2])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[2],
                      "\n   Should be: 178m"))
    }
    if (!grepl("116m", fname[3])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[3],
                      "\n   Should be: 116m"))
    }
    if (!grepl("69m", fname[4])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[4],
                      "\n   Should be: 69m"))
    }
    if (!grepl("35m", fname[5])) {
      CallStop(paste0("Unexpected HErZ : ", fname[5],
                      "\n   Should be: 35m"))
    }
    if (!grepl("10m", fname[6])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[6],
                      "\n   Should be: 10m"))
    }
  } else {
    if (!grepl("116m", fname[1])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[1],
                      "\n   Should be: 116m"))
    }
    if (!grepl("10m", fname[2])) {
      CallStop(paste0("Unexpected HErZ height: ", fname[2],
                      "\n   Should be: 10m"))
    }
  }
}

#-----------------------------------------------------------------------------------

#' @title Check parameter names of specific tower measurements.
#' @description Parameter names are checked. Within the package the names
#'   are expected to follow a certain structure: "windspeed_xxxm", where xxx
#'   equals the height of the measurement level and is expected to be monotonically
#'   decreasing. This function does not have a return value.
#' @param tower.param a string of length n which holds n parameter names.
#' @param tower.name a string which holds the name of the tower.
#' @export
CheckTowerParams <- function(tower.param, tower.name) {
  if (tower.name == "Lindenberg") {
    if (!tower.param[1] == "windspeed_98m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[1],
                      "\n   Should be: windspeed_98m"))
    }
    if (!tower.param[2] == "windspeed_80m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[2],
                      "\n  Should be: windspeed_80m"))
    }
    if (!tower.param[3] == "windspeed_60m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[3],
                      "\n   Should be: windspeed_60m"))
    }
    if (!tower.param[4] == "windspeed_40m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[4],
                      "\n   Should be: windspeed_40m"))
    }
    if (!tower.param[5] == "windspeed_20m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[5],
                      "\n  Should be: windspeed_20m"))
    }
    if (!tower.param[6] == "windspeed_10m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[6],
                      "\n   Should be: windspeed_10m"))
    }
  } else if (tower.name == "Cabauw") {
    if (!tower.param[1] == "windspeed_200m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[1],
                      "\n   Should be: windspeed_200m"))
    }
    if (!tower.param[2] == "windspeed_140m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[2],
                      "\n   Should be: windspeed_140m"))
    }
    if (!tower.param[3] == "windspeed_80m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[3],
                      "\n   Should be: windspeed_80m"))
    }
    if (!tower.param[4] == "windspeed_40m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[4],
                      "\n   Should be: windspeed_40m"))
    }
    if (!tower.param[5] == "windspeed_20m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[5],
                      "\n   Should be: windspeed_20m"))
    }
    if (!tower.param[6] == "windspeed_10m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[6],
                      "\n   Should be: windspeed_10m"))
    }
  } else {
    CallStop(paste0("\n   ***   Unexpected tower name: ", tower.name, "\n         ",
                    "Only Lindenberg and Cabauw are supported; ABORTING\n"))
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
GetNearestIdx <- function(vec, num) {
  idx = which( abs(vec - num) == min(abs(vec - num)) )
  return(idx)
}

#-----------------------------------------------------------------------------------

#' @title Extract longitude and latitude index off a lon, lat regular grid.
#' @description \code{GetLonLatIdx} extracts the longitude and latitude index of
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
#' @export
GetLonLatIdx <- function(fname, point.lon, point.lat,
                         grid.lon=NULL, grid.lat=NULL) {
  CheckFile(fname)
  if (is.null(grid.lon)) { # ERA20C, ERA-Interim data
    dat = ReadNetcdfLonLat(fname)
    grid.lon = dat$lon
    grid.lat = dat$lat
    latidx = GetNearestIdx(grid.lat, point.lat)
    lonidx = GetNearestIdx(grid.lon, point.lon)
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
SetToDate <- function(tsstart, tsend) {
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
#'  time series, (winter.xts=,spring.xts=,summer.xts=,autumn.xts=),  which was split
#'  off the input xts.
#' @note could be enhanced by manually splitting into meteorol. seasons DJF, MAM,
#'   JJA, SON instead of using the generic split 'f="quarters"'.
#' @importFrom xts as.xts split.xts
#' @importFrom zoo index as.yearmon
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

#' @title Determine y-axis limits from data.
#' @description \code{GetYlims} determinies the low and high y-axis limits from four
#'   different time series.
#'   This needs to be enhanced so that not all of those time series need to be
#'   available. Something like present= in FORTRAN.
#' @param xts1,xts2,xts3,xts4 extended time series from which to determine the low
#'   and high range of the y-axis limits.
#' @return Return a named list (yll=,ylh=) of the lower and high bound of the y-axis
#'   limits yliml and ylimh.
#' @importFrom xts is.xts
GetYlims <- function(xts1, xts2, xts3, xts4) {
  if (is.xts(xts1) & (is.xts(xts2)) & is.xts(xts3) & (is.xts(xts4))) {
    if (!any(is.finite(xts4))) {
      if (!any(is.finite(xts3))) {
        yliml = floor(min(min(xts1, na.rm=TRUE), min(xts2, na.rm=TRUE)))
        ylimh = ceiling(max(max(xts1, na.rm=TRUE), max(xts2, na.rm=TRUE)))
      } else {
        yliml = floor(min(min(xts1, na.rm=TRUE), min(xts2, na.rm=TRUE),
                          min(xts3, na.rm=TRUE)))
        ylimh = ceiling(max(max(xts1, na.rm=TRUE), max(xts2, na.rm=TRUE),
                            max(xts3, na.rm=TRUE)))
      }
    } else {
      yliml = floor(min(min(xts1, na.rm=TRUE), min(xts2, na.rm=TRUE),
                        min(xts3, na.rm=TRUE), min(xts4, na.rm=TRUE)))
      ylimh = ceiling(max(max(xts1, na.rm=TRUE), max(xts2, na.rm=TRUE),
                          max(xts3, na.rm=TRUE), max(xts4, na.rm=TRUE)))
    }
  } else {
    CallStop("XTS1 or XTS2 or XTS3 or XTS4 is not an xts, ABORTING!")
  }

  return(list(yll=yliml, ylh=ylimh))
}

#-----------------------------------------------------------------------------------
