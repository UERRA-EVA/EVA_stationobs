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
#' @param recs up to two concatinated integers which specify the beginning and end
#'   of the records (time steps) to read
#' @param pipe string to optionally specify additional selection chriterium. Default
#'   is an empty string.
#' @return x the array containing the read data
readGrib <- function(filename, nlon, nlat, nlev, var='undef', out='Rfile.dat',
                     recs=c(0), pipe='', verb.grib=FALSE) {
  if (recs[1]==0) {
    if (var == 'undef') {
      system(paste0('wgrib -d all ',filename,' -bin -nh -o ',out))
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4),
                 c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4),
                 c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    } else {
      if (verb.grib) {
        system(paste0("wgrib ",filename," | grep '",var,"' ",pipe,
                     " | wgrib -i -bin -nh ",filename," -o ",out))
      } else {
        system(paste0("wgrib ",filename," | grep '",var,"' ",pipe,
                     " | wgrib -i -bin -nh ",filename," -o ",out,
                     " &> /dev/null"))
      }
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4),
                 c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4),
                 c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    }
  } else {
    if (var == 'undef') {
      system(paste0("wgrib ",filename," | awk '{if ($1 >= ",recs[1],
                   " && $1 <= ",recs[2],") print $0}' FS=':' | wgrib -i ",
                   filename," -bin -nh -o ",out))
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4),
                 c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4),
                 c(nlon,nlat,nlev) )
      }
      close(openfile)
      system(paste('rm',out,sep=' '))
    } else {
      if (verb.grib) {
        system(paste("wgrib -v ",filename," | grep '",var,"' | wgrib -i ",
                     filename," -grib -o tmp.grib"))
        system(paste("wgrib -s tmp.grib | awk '{if ($1 >= ",recs[1]," && $1 <= ",
                     recs[2],") print $0}' FS=':' | wgrib -i tmp.grib -bin -nh -o ",
                     out))
      } else {
        system(paste0("wgrib ",filename," | grep '",var,"' | wgrib -i ",
                     filename," -grib -o tmp.grib"))
        system(paste0("wgrib tmp.grib | awk '{if ($1 >= ",
                      recs[1], " && $1 <= ",recs[2],
                      ") print $0}' FS=':' | wgrib -i tmp.grib -bin -nh -o ",
                      out," &> /dev/null"))
      }
      openfile<-file(out,'rb')
      if (nlev == 1) {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat,size=4),
                 c(nlon,nlat) )
      } else {
        x<-array(readBin(con=out,'numeric',n=nlon*nlat*nlev,size=4),
                 c(nlon,nlat,nlev) )
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
ReadNetcdf <- function(variable, infile, start=NULL,
                       count=NULL, revert=FALSE, verb.dat=FALSE) {

  # === read netCDF file depending on start and count of variable index ===
  nc <- open.ncdf(infile)
  if (is.numeric(start)) {
    if (is.numeric(count)) {
      data <- get.var.ncdf(nc, variable, start=start, count=count, verbose=verb.dat)
    } else {
      data <- get.var.ncdf(nc, variable, start=start, verbose=verb.dat)
    }
  } else {
    if (is.numeric(count)) {
      data <- get.var.ncdf(nc, variable, count=count, verbose=verb.dat)
    } else {
      data <- get.var.ncdf(nc, variable, verbose=verb.dat)
    }
  }

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

  return(list(data=data, lon=lon, lat=lat, time=time.vals))
}

#-----------------------------------------------------------------------------------
#' @title Read longitude and latitude arrays off a netCDF file.
#' @description \code{ReadnercdfLonLat} is a shortcut to only read the longitude and latitude
#' information off a netCDF file. If they don't exist a NULL value will be returned.
#' @param infile string of the file name to be read
#' @return lon,lat the longitude and latitude arrays (1D or 2D) read off the netCDF
#' file.
ReadNetcdfLonLat <- function(infile) {
  CheckFile(infile)
  nc <- open.ncdf(infile)
  lon <- nc$dim$lon$vals
  lat <- nc$dim$lat$vals
  close.ncdf(nc)
  return(list(lon=lon, lat=lat))
}
