
#' @title Reading grib files based on wgrib.
#' @description \code{readGrib} is based on wgrib to read grib files. It can read
#'   files by parameter and by specifyed level and time step. This function has been
#'   written by Chris Bollmeyer of Uni Bonn, received in 2014-09-25
#' @param filename string of the grib file name to read from
#' @param nlon,nlat,nlev specify the extent of the data in number of longitude,
#'   latitude and vertical level steps (pixels)
#' @param var string of the variable name to read
#' @param out a temporary file into which output is dumped. NOTE: this needs to be
#'   specifyied if this function is called more than once in parralel (i.e., totally
#'   independent from each other)
#' @param recs up to two concatinated integers which specify the beginning and end of
#'   the records (time steps) to read
#' @param pipe string to optionally specify additional selection chriterium. Default
#'   is an empty string.
#' @return x the array containing the read data
readGrib <- function(filename, nlon, nlat, nlev, var='undef', out='Rfile.dat',
                     recs=c(0), pipe='') {
  if (recs[1]==0) {
    if (var == 'undef') {
      system(paste('wgrib -d all ',filename,' -bin -nh -o ',out,sep=''))
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4), c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4), c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    } else {
      system(paste("wgrib -v ",filename," | grep '",var,"' ",pipe,
                   " | wgrib -i -bin -nh ",filename," -o ",out,sep=''))
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4), c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4), c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    }
  } else {
    if (var == 'undef') {
      system(paste("wgrib -s ",filename," | awk '{if ($1 >= ",recs[1],
                   " && $1 <= ",recs[2],") print $0}' FS=':' | wgrib -i ",
                   filename," -bin -nh -o ",out,sep=''))
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4), c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4), c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    } else {
      system(paste("wgrib -v ",filename," | grep '",var,"' | wgrib -i ",
                   filename," -grib -o tmp.grib",sep=''))
      system(paste("wgrib -s tmp.grib | awk '{if ($1 >= ",recs[1],
                   " && $1 <= ",recs[2],") print $0}' FS=':' | wgrib -i tmp.grib -bin -nh -o ",out,sep=''))
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4), c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4), c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    }
  }
  return(x)
}

#===================================================================================

#' @title Reading netCDF files.
#' @description \code{ReadNetcdf} read nc-files provided a variable name and the
#'   file name to read from. Longitude, latitude, and time values are read if
#'   available. An optional paramter "revert" regulates whether the data values
#'   should be reverted in North-South.
#' @param variable is a string containing the variable name to be read of the nc-file
#' @param infile is a string holding the file name to read
#' @param revert in an optional boolean to decided whether to revert the data in
#'   North-South direction. Default is not to revert the data (FALSE)
#' @return data,lon,lat,time.vals in a list holding the read data, longitude,
#'   latitude, and time values
ReadNetcdf <- function(variable, infile, revert=FALSE) {

  # === read and use seasonal time series data and plot only these ===
  nc <- open.ncdf(infile)
  data <- get.var.ncdf(nc, variable)

  # set missing value
  data[data==nc$var[[variable]]$missval] <- NA

  # read lat and lon
  lon <- nc$dim$lon$vals
  lat <- nc$dim$lat$vals

  close.ncdf(nc)

  # read time
  time.vals <- convertDateNcdf2R(infile)

  #=== reverse latitude vector
  if (revert) {
    lat <- rev(lat)
    # flip data along latitude
    if (length(dim(data)) == 3) {
      data <- data[, ncol(data):1, ] #lat being dimension number 2
    } else if (length(dim(data)) == 2) {
      data <- data[, ncol(data):1] #lat being dimension number 2
    } else {
      stop("   @@@   UNEXPECTED LENGTH OF DATA: ", dim(data))
    }
  }

  return(list(data, lon, lat, time.vals))
}

#-----------------------------------------------------------------------------------
