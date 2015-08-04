#' @title A function to remove files.
#' @description \code{remove.files} can remove any files with provided extensions;
#'   by defaul it explicitely removes *.txt and *.html files.
#' @param extension is a (vector of) string(s) which holds the extensions to of the
#'   files to be removed.
remove.files <- function(extension=NULL){
  if (is.null(extension)){
    extension <- c(".txt",".html")
  }
  for (i in c(1:length(extension))){
    junk <- c(dir(path="DWD_station_data",pattern=extension[i]))
    file.remove(file.path("DWD_station_data",junk))
  }
}

#-----------------------------------------------------------------------------------

#' @title Download data from the DWD-ftp server.
#' @description \code{get.data}
#'   Allows downloading data from the DWD-ftp server according to the metadata
#'   information saved in the output argument \code{metadata} retrieved from
#'   \code{\link{get.metadata}}. The data is then unzipped and extracted for hourly
#'   or daily values as specified.
#' @param metadata list; argument retrieved from \code{\link{get.metadata}}. If
#'   empty, the user will be asked to enter the metadata manually through the
#'   R-console.
#' @param verbose.DWD boolean; determines whether to tell what's going on.
#' @return  \code{data1} data.frame. Contains the data that correspond to the given
#'   metadata.
#' @examples
#'   # If no input argument is known:
#'   data1 <- get.data() # R will ask the user through the
#'                      # console to enter each argument
#'
#'   # If the input argument is known
#'   data1 <- get.data(metadata)
get.data <- function(metadata=NULL, verbose.DWD=TRUE){
  # Remove everything besides the inputs
  rm(list=ls()[!(ls()%in% c("metadata", "verbose.DWD"))])

  ##########################################################
  # CHECK PACKAGES
  packages <- c("RCurl","XML","R.utils")
  for (package in packages){
    # Check whether the package is already installed
    id <- find.package(package,quiet=TRUE)
    if (length(id)>0){
      if (verbose.DWD) print(paste("Package:",package,"already exist"))
      # upload the package
      library(package,character.only=TRUE)
    }else{
      # install the package
      install.packages(package)
      # upload the package
      library(package,character.only=TRUE)
    }
  }
  ##########################################################
  # CHECK THE EXISTENCE OF THE INPUT ARGUMENTS
  # Check if the input arguments are not null
  if (is.null(metadata)){
    # source("get.metadata.R")
    metadata <- get.metadata()
  }

  ##########################################################
  # EXTRACT DATA
  # metadata
  for (i1 in c(1:length(metadata[[1]]))){
    assign(metadata[[1]][i1],metadata[[i1+1]])
  }

  ##########################################################
  # DEFINE THE OUTPUT PATH
  a <- getwd()
  path.output <- file.path(a,"DWD_station_data")
  dir.create(path.output,showWarnings=FALSE)

  ##########################################################
  # REMOVE ".txt" & ".html"
  remove.files(c(".txt",".html"))

  ##########################################################
  # FTP CONNECTION - HISTORICAL
  # URL path
  url <-paste(
    "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/",
    time.resol,"/",path.element,"/historical/",sep="")

  # Find out which "splitter" to use (it depends on the OS)
  os.system <- Sys.info()[[1]]
  if (tolower(os.system)=="windows"){
    splitter <- "\r\n"
  }else{
    splitter <- "\n"
  }

  # list of files contained in the "url":
  files.list <- strsplit(getURL(url, dirlistonly=TRUE),splitter)[[1]]

  # find which file contains the data of the given "station_id":
  id <- grep(paste("_",station.id,"_",sep=""),files.list)

  # Unzip the files
  if (length(id)>0){
    zip.file <- files.list[id]
    file.output <- file.path(path.output,zip.file)
    # Check if the file is saved in the computer
    if (file.exists(file.output)){
      if (verbose.DWD) print(paste(zip.file,"already exists"))
    }else{
      # Download the data from the FTP
      url1 <- paste(url,zip.file,sep="")
      download.file(url1,file.output,"auto")
    }

    ######################################################
    # DOWNLOAD THE DATA
    # Unzip the file
    unzip(file.output,exdir = path.output)
    file.name <- file.path(path.output,
                           list.files(path = path.output,
                                      pattern="produkt_*"))

    data1 <- read.table(file.name,sep=";",skip=1)

    # Check if there are duplicated columns in "data1":
    if (time.resol=="hourly"){
      duplicated.columns <- duplicated(t(data1))
      data1 <- data1[, !duplicated.columns]
    }

    colnames <- as.matrix(read.table(file.name,sep=";",
                                     nrows=1))
    colnames <- gsub(" ","",colnames)
    colnames <- tolower(colnames)
    colnames(data1) <- colnames

    ##########################################################
    # REMOVE ".txt"
    remove.files(c(".txt", ".html"))

    #######################################################
    # FTP CONNECTION - RECENT
    # URL path
    url <-paste(
      "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/",
      time.resol,"/",path.element,"/recent/",sep="")

    # Find out which "splitter" to use (it depends on the OS)
    os.system <- Sys.info()[[1]]
    if (tolower(os.system)=="windows"){
      splitter <- "\r\n"
    }else{
      splitter <- "\n"
    }

    # list of files contained in the "url":
    files.list <- strsplit(getURL(url, dirlistonly=TRUE),splitter)[[1]]

    # find which file contains the data of the given "station_id":
    id <- grep(paste("_",station.id,"_",sep=""),files.list)

    # Unzip the files
    if (length(id)>0){
      zip.file <- files.list[id]
      file.output <- file.path(path.output,zip.file)
      # Check if the file is saved in the computer
      if (file.exists(file.output)){
        if (verbose.DWD) print(paste(zip.file,"already exists"))
      }else{
        # Download the data from the FTP
        url1 <- paste(url,zip.file,sep="")
        download.file(url1,file.output,"auto")
      }

      ######################################################
      # DOWNLOAD THE DATA
      # Unzip the file
      unzip(file.output,exdir = path.output)
      file.name <- file.path(path.output,
                             list.files(path = path.output,
                                        pattern="produkt_*"))

      data2 <- read.table(file.name,sep=";",skip=1)

      colnames <- as.matrix(read.table(file.name,sep=";",
                                       nrows=1))
      colnames <- gsub(" ","",colnames)
      colnames(data2) <- colnames

      # Merge "data" and "data2"
      last.date <- data1$mess_datum[nrow(data1)-1]
      id210 <- which(data2$mess_datum>last.date)

      data3 <- rbind(data1,data2[id210,])

      id4 <- which(data3$mess_datum>=date.begin &
                     data3$mess_datum<=date.end)
      data5 <- data3[id4,]
    }

    #######################################################
    # REMOVE ".txt"
    remove.files(c(".txt", ".html"))


    ########################################################
    # GET DATA FOR SPECIFIC DATE INTERVAL

    # Find data only for the specific dates
    if (time.resol=="hourly"){
      data1$mess_datum <- format(strptime(
        as.character(data1$mess_datum),"%Y%m%d%H"),
        "%Y-%m-%d %H:%M:%S")
    }else{
      if (time.resol=="daily"){
        data1$mess_datum <- format(strptime(
          as.character(data1$mess_datum),"%Y%m%d"),
          "%Y-%m-%d")
      }
    }
    ####################################################

    # OUTPUT
    return(data1)
  }else{
    print(paste("\n   ***   No data available for ID ",station.id,"\n"))
  }
}

#-----------------------------------------------------------------------------------

#' @title Download data from the DWD-ftp server.
#' @description \code{get.data.download}
#'   Data is downloaded from the DWD-ftp server
#'   according to the metadata information saved in
#'   the output argument \code{metadata} retrieved from
#'   \code{\link{get.metadata}}. No object is returned.
#' @param metadata list. Argument retrieved
#'   from \code{\link{get.metadata}}. If empty, the
#'   user will be asked to enter the metadata
#'   manually through the R-console.
get.data.download <- function(metadata=NULL){
  # Remove everything besides the inputs
  rm(list=ls()[!(ls()%in% c("metadata"))])

  ##########################################################
  # CHECK PACKAGES
  packages <- c("RCurl","XML","R.utils")
  for (package in packages){
    # Check whether the package is already installed
    id <- find.package(package,quiet=TRUE)
    if (length(id)>0){
      print(paste("Package:",package,"already exist"))
      # upload the package
      library(package,character.only=TRUE)
    }else{
      # install the package
      install.packages(package)
      # upload the package
      library(package,character.only=TRUE)
    }
  }
  ##########################################################
  # CHECK THE EXISTENCE OF THE INPUT ARGUMENTS
  # Check if the input arguments are not null
  if (is.null(metadata)){
    metadata <- get.metadata()
  }

  ##########################################################
  # EXTRACT DATA
  # metadata
  for (i1 in c(1:length(metadata[[1]]))){
    assign(metadata[[1]][i1],metadata[[i1+1]])
  }

  ##########################################################
  # DEFINE THE OUTPUT PATH
  a <- getwd()
  path.output <- file.path(a,"data")
  dir.create(path.output,showWarnings=FALSE)

  ##########################################################
  # REMOVE ".txt" & ".html"
  remove.files(c(".txt",".html"))

  ##########################################################
  # FTP CONNECTION - HISTORICAL
  # URL path
  url <-paste(
    "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/",
    time.resol,"/",path.element,"/historical/",sep="")

  # Find out which "splitter" to use (it depends on the OS)
  os.system <- Sys.info()[[1]]
  if (tolower(os.system)=="windows"){
    splitter <- "\r\n"
  }else{
    splitter <- "\n"
  }

  # list of files contained in the "url":
  files.list <- strsplit(getURL(url, dirlistonly=TRUE),splitter)[[1]]

  # find which file contains the data of the given "station_id":
  id <- grep(paste("_",station.id,"_",sep=""),files.list)

  # Unzip the files
  if (length(id)>0){
    zip.file <- files.list[id]
    file.output <- file.path(path.output,zip.file)
    # Check if the file is saved in the computer
    if (file.exists(file.output)){
      print(paste(zip.file,"already exists"))
    }else{
      # Download the data from the FTP
      url1 <- paste(url,zip.file,sep="")
      download.file(url1,file.output,"auto")
    }
  }
}

#-----------------------------------------------------------------------------------

#' @title Provide metadata for downloading data from DWD-ftp server.
#' @description Load metadata which specifies which data to download from the
#'   DWD-ftp server.
#' @param time.resol String of station data time resolution, e.g., daily, hourly
#' @param station.id string of the station id
#' @param date.begin string of the begin date of the station data in the format
#'   "YYYY-MM-DD"
#' @param date.end same as above for the end date
#' @param path.element string as an additional element of the path to the data
#' @return metadata The metadata saved in a string vector containing the above
#'   parameters
get.metadata <- function(time.resol, station.id,
                         date.begin, date.end, path.element){
  liste <- c("time.resol" , "station.id" ,  "date.begin" ,
             "date.end", "path.element")
  attributes <- list(liste)

  metadata <- vector()
  metadata <- c(metadata, attributes)

  for (i4 in attributes[[1]]){
    metadata<-c(metadata,list(get(i4)))
  }
  return(metadata)
}

#-----------------------------------------------------------------------------------

#' @title Download DWD station data from CDC-ftp server.
#' @description \code{all.data} retrieves data from the CDC ftp-server using Raffaels
#'   function get.data and the dummy metadata as obtained from the above function.
#'   Using these two functions instead of his complete package is necessary because
#'   it was eveloped to run under Windows, and it was developed to have a user interface.
#'   Here, I have hardcoded to download all available daily climate data. I allow
#'   to choose the station via its station id. I want the output data frame to also
#'   hold information about the station name, longitude, and latitude which are input
#'   to this funciton.
#' @param station.id a string of length 5 containing the station id as provided in
#'   the metadata of the downloaded station
#' @param station.name string of the station name corresponding to the above station
#'   id
#' @param station.lon longitude of the station
#' @param station.lat latitude of the station
#' @param daily optional boolean to specify whether station data has daily or hourly
#'   time steps. Default is daily (TRUE).
#' @param  download optional boolean to specify whether only downloading shall be
#'   performed (TRUE) or unzipping and data extraction shall be performed as well
#'   (FALSE). Default is perform unzipping and extraction also (FALSE).
#' @param verbose.DWD optional boolean to specivy whether to print out what's going
#'   (TRUE) on or surpress it (FALSE). Default is to print out messages (TRUE).
#' @return \code{data1} a data frame (returned as list) containing all this data
#'   including windspeed listed chronologically
all.data <- function(station.id, station.name, station.lat, station.lon,
                     daily=TRUE, download=FALSE, verbose.DWD=TRUE){
  if (daily) {
    metadata <- get.metadata("daily",station.id,"1800-01-01","2020-12-31","kl")
  } else {
    metadata <- get.metadata("hourly",station.id,"1800-01-01","2020-12-31","wind")
  }
  if (!(download)) {
    data1 <- get.data(metadata, verbose.DWD)
    data2 <- data.frame(data1$stations_id, station.name, station.lat, station.lon,
                        data1$mess_datum,data1$windgeschwindigkeit)
    data2 = data2[-nrow(data2),]
    colnames(data2) <- c("STATIONS_ID","STATIONS_NAME", "GEO_BREITE", "GEO_LÄNGE",
                         "MESS_DATUM","WINDGESCHWINDIGKEIT")
    return(data2)
  } else {
    get.data.download(metadata, verbose.DWD)
  }
}

#-----------------------------------------------------------------------------------
