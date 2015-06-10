library(ncdf.tools)
library(ncdf)

#' @title
#' @description
#' @param
#' @return
#' Function for reading grib-Files in R
#' function written by Chris Bollmeyer, received 20140925
readGrib <- function(filename,nlon,nlat,nlev,var='undef',out='Rfile.dat',recs=c(0),pipe='') {
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

#' @title
#' @description
#' @param
#' @return
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

#' @title
#' @description
#' @param
#' @return
ReadHerzWind <- function(varname, infile, revert=FALSE) {
  # === open nc file and read data, lat, lon
  nc <- open.ncdf(infile)
  data <- get.var.ncdf(nc, varname)

  # set missing value
  data[data==nc$var[[varname]]$missval] <- NA
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

  return(list(data, time.vals))
}

#-----------------------------------------------------------------------------------
