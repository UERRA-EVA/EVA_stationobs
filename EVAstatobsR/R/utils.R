#' @title Check for existence of file.
#' @description \code{CheckFile} checks for the existence of the file(s) passed to
#' it. If one file doese not exist it stops with an error message.
#' @param infile Character string or concatenated character strings holding the file
#' name(s).
CheckFile <- function(infile) {
  if (!class(infile) == "character") {
    err = simpleError(paste0("\n   ***\n   Unexpected type of infile, ABORTING!\n",
                             "   ", class(infile), "\n   should be character\n   ***\n"))
    tryCatch(stop(err))
  }
  if (any(!file.exists(infile))) {
    idx = which(!file.exists(infile))
    missing.file = infile[idx]
    err = simpleError(paste0("\n   ***\n   Missing File: ", infile[idx], "ABORTING!\n",
                             "   ***\n"))
    tryCatch(stop(err), finally=print(missing.file))
  }
}

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

#' @title Extract longitude and latitude index off a lon, lat regular grid.
#' @description \code{get.lon.lat.idx} extracts the longitude and latitude index of
#' a point off a netCDF file which holds data on a regular grid with longitude and
#' latitude values (as vectors) stored.
#' @param fname string of the file name to read
#' @return lonidx,latidx longitude and latitude index
get.lon.lat.idx <- function(fname, point.lon, point.lat,
                            grid.lon=NULL, grid.lat=NULL) {
  CheckFile(fname)
  if (is.null(grid.lon)) { # ERA20C, ERA-Interim data
    dat = ReadNetcdfLonLat(fname, count=c(1,1,1))
    grid.lon = dat[[2]]
    grid.lat = dat[[3]]
    latidx = get.nearest.idx(grid.lat, point.lat)
    lonidx = get.nearest.idx(grid.lon, point.lon)
    return(list(lonidx=lonidx, latidx=latidx))
  } else { # HErZ data
    dist.to.point = getNearest(point.lat, grid.lat, point.lon, grid.lon)
    index.at.point = arrayInd(which.min(dist.to.point), dim(dist.to.point))
    return(list(lonidx=index.at.point[1], latidx=index.at.point[2]))
  }
}
