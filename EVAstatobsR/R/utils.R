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
    CallStop(paste0("Missing File: ", infile[idx], "  ABORTING!\n"))
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

#' @title Check parameter names of specific tower measurements.
#' @description Parameter names are checked. Within the package the names
#'   are expected to follow a certain structure: "windspeed_xxxm", whereas xxx
#'   equals the height of the measurement level and is expected to be monotonically
#'   increasing or decreasing, depending on measurement site. This function does not
#'   have a return value.
#' @param tower.param a string of length n which holds n parameter names.
#' @param tower.name a string which holds the name of the tower.
CheckTowerParams <- function(tower.param, tower.name) {
  if (tower.name == "Lindenberg") {
    if (!tower.param[1] == "windspeed_10m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[1],
                      "\n   Should be: windspeed_10m"))
    }
    if (!tower.param[2] == "windspeed_20m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[2],
                      "\n  Should be: windspeed_20m"))
    }
    if (!tower.param[3] == "windspeed_40m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[3],
                      "\n   Should be: windspeed_40m"))
    }
    if (!tower.param[4] == "windspeed_60m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[4],
                      "\n   Should be: windspeed_60m"))
    }
    if (!tower.param[5] == "windspeed_80m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[5],
                      "\n  Should be: windspeed_80m"))
    }
    if (!tower.param[6] == "windspeed_98m") {
      CallStop(paste0("Unexpected Lindenberg parameter: ", tower.param[6],
                      "\n   Should be: windspeed_98m"))
    }
  } else if (tower.name == "Cabauw") {
    if (!tower.param[1] == "windspeed_10m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[1],
                      "\n   Should be: windspeed_200m"))
    }
    if (!tower.param[2] == "windspeed_20m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[2],
                      "\n   Should be: windspeed_140m"))
    }
    if (!tower.param[3] == "windspeed_40m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[3],
                      "\n   Should be: windspeed_80m"))
    }
    if (!tower.param[4] == "windspeed_80m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[4],
                      "\n   Should be: windspeed_40m"))
    }
    if (!tower.param[5] == "windspeed_140m") {
      CallStop(paste0("Unexpected Cabauw parameter: ", tower.param[5],
                      "\n   Should be: windspeed_20m"))
    }
    if (!tower.param[6] == "windspeed_200m") {
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

#' @title Calculate the relative difference between two values.
#' @description Provided is a value and the mean value to which the relative
#'   difference is to be computed.
#' @param value is the value of which the relvative difference is to be calcualted
#'   compated to mean.value
#' @param mean.value is the mean value from above
#' @return is the relative difference
RelDiff <- function(value, mean.value) {
  return((value - mean.value) / mean.value)
}

#-----------------------------------------------------------------------------------

#' @title Calculate the norm of a value given the the upper and lower bound.
#' @description Given the upper and lower bound calculate the norm value.
#' @param value is the value of which the norm is to be calcualted. Needs to satisfay:
#'   min.value =< value =< max.val.
#' @param min.val,max.val are both the lower and upper bound
#' @return is the norm value.
NormVals <- function(value, min.val, max.val) {
  return((value-min.val)/(max.val-min.val))
}

#-----------------------------------------------------------------------------------

#' @title Calculate the 2x2 contingency table of the distribution between
#'   observations and forecasts (here: reanalysis)
#' @description This function calculates the 2x2 contigency table  of the
#'   distribution between observations and forecasts (here: reanalysis). The input
#'   are two numeric vectors of observations and forecasts (reanalysis) and a
#'   benchmark for which the table shall be calculated. The output is a names list
#'   holding the distribution in the variables a, b, c, d.
#' @param obs a numeric vector holding the observations. It is sorted in chronological
#'   order (it consists of the data values of an extended time series) and needs to
#'   have the same length as frcst.
#' @param frcst same as above but the the forecast (here: reanalysis) data.
#' @param benchmark
#' @return is a names list holding the distribution in the variables a, b, c, d.
CalcContTable <- function(obs, frcst, benchmark) {

  # check for NA values and set to NA at same time steps to both time series
  idx = which(!is.finite(obs))
  frcst[idx] = NA
  idx = which(!is.finite(frcst))
  obs[idx] = NA
  abs.bench.obs = quantile(obs, benchmark, na.rm=T)
  abs.bench.frcst = quantile(frcst, benchmark, na.rm=T)

  cnt_a = 0
  cnt_b = 0
  cnt_c = 0
  cnt_d = 0

  if (length(obs) != length(frcst))
    CallStop(paste0("Length of observation and forecast vector should match; ",
                    "length(obs) = ", length(obs), "   length(frcst) = ", length(frcst)))

  for (steps in seq(obs)) {
    if (is.finite(obs[steps]) & is.finite(frcst[steps])) {
      if (obs[steps] >= abs.bench.obs) {
        if (frcst[steps] >= abs.bench.frcst) {
          cnt_a = cnt_a + 1
        } else {
          cnt_c = cnt_c + 1
        }
      } else {
        if (frcst[steps] >= abs.bench.obs) {
          cnt_b = cnt_b + 1
        } else {
          cnt_d = cnt_d + 1
        }
      }
    }
  }
  return(list(a=cnt_a, b=cnt_b, c=cnt_c, d=cnt_d))
}

#-----------------------------------------------------------------------------------

#' @title Calcualte scores and skill scores based on the contigency table.
#' @description Input are only the for distribution values of the contingency table
#'   a, b, c, d. In this function twelve scores and skill scores are calculated,
#'   including: hit rate (probability of detection (POD)), false alarm rate
#'   (probability of false detection), false alarm ratio, Hanssen-Kuipers score
#'   (True Statistic, Pierce Kill Score), threat score (critical success index),
#'   equitable threat score (Gilbert skill score), frequency bias index, Heidke
#'   skill score, accuracy (percent correct), odds ratio, extremal dependence index,
#'   symmetric extremal dependence index (both from Ferro and Stephenson, 2011, doi:
#'   \url{10.1175/WAF-D-10-05030.1}).
#' @param a,b,c,d the four parameters of the contigency table (calculated by
#'   \code{\link{CalcContTable}}) which are also called hits, flase alarms, misses,
#'   and correct rejects, respectively.
ContTableScores <- function(a, b, c, d) {

  n = a + b + c + d
  r = (a + b) * (a + c) / n

  # checks
  if (a == 0) cat(paste0("\n *** WARNING: Contigency table: a = 0"))
  if (b == 0) cat(paste0("\n *** WARNING: Contigency table: b = 0"))
  if (c == 0) cat(paste0("\n *** WARNING: Contigency table: c = 0"))
  if (d == 0) cat(paste0("\n *** WARNING: Contigency table: d = 0"))
  if (n == 0) cat(paste0("\n *** WARNING: Contigency table: n = 0"))
  if (r == 0) cat(paste0("\n *** WARNING: Contigency table: r = 0"))

  # calculate (skill) scores based on the contingency table
  POD = a / (a+c) # aka hit rate (probability of detection)
  POFD = b / (b+d) # aka false alarm rate (probability of false detection)
  FAR = b / (a+b) # false alarm ratio
  # Hanssen-Kuipers/True Skill Statistic/Pierce Skill Score (POD - POFD)
  HK = (a*d - b*c) / ( (a+c)*(b+d))
  TS = a / (a+b+c) # threat score or critical success index
  ETS = (a - r) / (a+b+c - r) # aka Gilbert Skill Score
  BIAS = (a+b) / (a+c) # aka frequency bias index
  HSS = 2*(a*d - b*c)/( (a+c)*(c+d) + (a+b)*(b+d)) # Heidke Skill Score
  PC = (a+d) / n # aka proportion/percent correct or accuracy
  OR = a*d / (b*c) # odds ratio
  EDI = (log(POFD) - log(POD)) / (log(POFD) + log(POD)) # aka
  SEDI = (log(POFD) - log(POD) - log(1-POFD) + log(1-POD)) /
    (log(POFD) + log(POD) + log(1-POFD) + log(1-POD))

  return(list(hit.rate=POD, false.alarm.rate=POFD, false.alarm.ratio=FAR,
              true_skill_stats=HK, threat_score=TS, equi_threat_score=ETS,
              bias_index=BIAS, heidke_sksc=HSS, accuracy=PC, odds_ratio=OR,
              edi=EDI, sedi=SEDI))
}

#-----------------------------------------------------------------------------------
