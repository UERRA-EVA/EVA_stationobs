#!/usr/bin/env Rscript

library(xts)

rm(list=ls())

#==================
#
# general settings
#
#==================
AnalyseWindSpeed.script <- function() {

  arguments <- commandArgs(TRUE)
  config.file<-arguments[1]
  CheckFile(config.file)
  # read config parameters
  source(config.file)

  # checks on variables:
  if (!res.switch == "HighRes" & !res.switch == "OrigRes") {
    cat("\n   *** Unexpected res.switch, ABORTING!\n   res.switch = ", res.switch,
        "\n   should be either HighRes or OrigRes\n   ***\n")
    quit(status=1)
  }

  #==================
  #
  #  read MM ERA data
  #
  #==================
  for (res.steps in seq(from=1, to=length(res.switch), by=1)) {
    cat("\n  ** Reading ERA data of resolution: ", res.switch[res.steps],"\n\n")
    # read ERA20C monthly mean windspeed

    if (res.switch[res.steps] == "HighRes") {
      era20c.fname = era20c.HRes.fname
    } else {
      era20c.fname = era20c.ORes.fname
    }
    CheckFile(era20c.fname)

    era20c = ReadNetcdf(era20c.param, era20c.fname)
    era20c.data = era20c[[1]]
    era20c.lon = era20c[[2]]
    era20c.lat = era20c[[3]]
    era20c.time.vals = era20c[[4]]

    era20c = ReadNetcdf(era20c100.param, era20c.fname)
    era20c100.data = era20c[[1]]

    # read ERA-Interim monthly mean windspeed
    if (res.switch[res.steps] == "HighRes") {
      eraI.fname = eraI.HRes.fname
    } else {
      eraI.fname = eraI.ORes.fname
    }
    CheckFile(eraI.fname)

    eraI = ReadNetcdf(eraI.param, eraI.fname)
    eraI.data = eraI[[1]]
    eraI.lon = eraI[[2]]
    eraI.lat = eraI[[3]]
    eraI.time.vals = eraI[[4]]

    # read HErZ monthly mean windspeed
    CheckFile(herz.grid)
    nlon = 848
    nlat = 824
    herz.lon = readGrib(herz.grid, nlon, nlat, 1, var='RLON')
    herz.lat = readGrib(herz.grid, nlon, nlat, 1, var='RLAT')

    CheckFile(herz.fname)
    dat = ReadHerzWind(var=herz10.param, infile=herz.fname)
    herz10.data <- dat[[1]]
    herz.time.vals <- dat[[2]]

    dat = ReadHerzWind(var=herz116.param, infile=herz.fname)
    herz116.data <- dat[[1]]

    # loop over time periods to analyse
    for (switch.steps in seq(from=1, to=length(time.switch), by=1)) {
      cat("\n  ** time.switch = ", time.switch[switch.steps],"\n\n")

      #0==================================================0
      # when data becomes available adjust HErZ time ranges
      #0==================================================0
      if (time.switch[switch.steps] == "short.term") {
        era20c.tsstart = c(2007,1) # this here was 2007 to 2010
        era20c.tsend = c(2009,12)
        eraI.tsstart = c(2007,1)
        eraI.tsend = c(2009,12)
        herz.tsstart = c(2007,1)
        herz.tsend = c(2009,12)
        fname_ext = "2007to2009"
      } else if (time.switch[switch.steps] == "mid.term") {
        era20c.tsstart = c(1998,1) # this here was 1998 to 2010
        era20c.tsend = c(2009,12)
        eraI.tsstart = c(1998,1)
        eraI.tsend = c(2009,12)
        herz.tsstart = c(1998,1)
        herz.tsend = c(2009,12)
        fname_ext = "1998to2009"
        cat('\n   ***: era20c.tsstart', era20c.tsstart,'\n')
      } else if (time.switch[switch.steps] == "long.term") {
        era20c.tsstart = c(1979,1) # this here was 1979 to 2010
        era20c.tsend = c(2009,12)
        eraI.tsstart = c(1979,1)
        eraI.tsend = c(2009,12)
        herz.tsstart = c(1998,1)
        herz.tsend = c(2009,12)
        fname_ext = "1979to2009"
      } else if (time.switch[switch.steps] == "all.term") {
        era20c.tsstart = c(1901,1)
        era20c.tsend = c(2010,12)
        eraI.tsstart = c(1979,1)
        eraI.tsend = c(2014,12)
        herz.tsstart = c(1998,1)
        herz.tsend = c(2009,12)
        fname_ext = "1900to2014"
      } else {
        stop('\n   *** MISSPELLED time.switch -- NO OPTION SELECTED ***\n')
      }

      #============================================================
      #
      #  read station data and extract ERA data at station location
      #
      #============================================================

      # read station data
      path = "~/datensicherung/transfer/"
      if (daily) {
        fname = "KL_Tageswerte_Beschreibung_Stationen_20150409.txt"
      } else {
        fname = "FF_Stundenwerte_Beschreibung_Stationen_wind_selected3.txt"
      }
      station.file = paste(path, fname, sep="")
      CheckFile(station.file)

      if (daily) {
        station.data = read.table(station.file, skip=2, sep=";")
        colnames(station.data) = c("Station_id", "von_datum", "bis_datum", "Stationshoehe",
                                   "geoBreite", "geoLaenge", "Stationsname", "Bundeslland")
      } else {
        station.data = read.table(station.file, skip=2, sep=";")
        colnames(station.data) = c("Station_id", "von_datum", "bis_datum", "Stationshoehe",
                                   "geoBreite", "geoLaenge", "Stationsname")
      }
      station.info = station.data[,c("Station_id", "Stationsname", "geoBreite", "geoLaenge")]
      station.info[[1]] = sprintf("%05d", station.info[[1]])

      #========================================
      # here comes the large LOOP
      # it reads station data
      # calculates MM; sets 0s to NaN
      # calculates correlations with ERAs
      # plots results
      #========================================

      # loop over number of stations
      for (steps in seq(from=1, to=dim(station.info)[1], by=1)) {
        #     for (steps in seq(from=1, to=1, by=1)){
        # read station data
        cat(paste0("\n  **  Reading station data: ", station.info[[2]][steps],"\n\n"))
        station.data = data.frame()
        station.data = all.data(station.info[[1]][steps], station.info[[2]][steps],
                                station.info[[3]][steps], station.info[[4]][steps],
                                daily = daily)

        # convert to extended time series
        cat("\n  **  Converting station data\n")

        MM.station = ExtractStationData(station.data, era20c.tsstart, era20c.tsend,
                                        eraI.tsstart, eraI.tsend,
                                        herz.tsstart, herz.tsend,
                                        daily=daily)
        if(length(MM.station) == 0) { next }

        # extract ERA data at location of station
        era20c.data.xts = ExtractERAxts(era20c.data, era20c.time.vals,
                                        era20c.lon, era20c.lat,
                                        era20c.tsstart, era20c.tsend,
                                        station.data$GEO_LÄNGE[1],
                                        station.data$GEO_BREITE[1])

        era20c100.data.xts = ExtractERAxts(era20c100.data, era20c.time.vals,
                                           era20c.lon, era20c.lat,
                                           era20c.tsstart, era20c.tsend,
                                           station.data$GEO_LÄNGE[1],
                                           station.data$GEO_BREITE[1])

        eraI.data.xts = ExtractERAxts(eraI.data, eraI.time.vals,
                                      eraI.lon, eraI.lat,
                                      eraI.tsstart, eraI.tsend,
                                      station.data$GEO_LÄNGE[1],
                                      station.data$GEO_BREITE[1])

        herz.data.xts = ExtractHErZxts(herz10.data, herz.time.vals,
                                       herz.lon, herz.lat,
                                       herz.tsstart, herz.tsend,
                                       station.data$GEO_LÄNGE[1],
                                       station.data$GEO_BREITE[1])

        herz116.data.xts = ExtractHErZxts(herz116.data, herz.time.vals,
                                          herz.lon, herz.lat,
                                          herz.tsstart, herz.tsend,
                                          station.data$GEO_LÄNGE[1],
                                          station.data$GEO_BREITE[1])
        #-----------------------------------------------------------------------------
        cat("\n  **  Plotting\n")

        fname = paste0("ERAsStation_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeriesMonthly_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                       titname, outdir, fname, width=a4width, height=a4height,
                       monthly=TRUE, anomaly=FALSE)

        fname = paste0("ERAsStation_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeriesAnnual_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                       titname, outdir, fname, width=a4width, height=a4height,
                       monthly=FALSE, anomaly=FALSE)

        fname = paste0("ERAsStation_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeriesMonthly_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed anomaly [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                       titname, outdir, fname, width=a4width, height=a4height,
                       monthly=TRUE, anomaly=TRUE)

        fname = paste0("ERAsStation_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeriesAnnual_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed anomaly [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                       titname, outdir, fname, width=a4width, height=a4height,
                       monthly=FALSE, anomaly=TRUE)

        #------------------------------------------------------------------------------
        ### COMPARE 100m ERA20c AND 116m HErZ###
        fname = paste0("100mEra20cHerz_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeriesMonthly_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        Plot100mEraHerz(era20c100.data.xts, herz116.data.xts,
                        titname, outdir, fname, width=a4width, height=a4height)


        #-----------------------------------------------------------------------------

        fname = paste0("ERAsStation_Seasons_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeries1979.pdf")
        titname = paste0('Seasonal windspeed [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEraSeasons(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                              titname, outdir, fname, width=a4width, height=a4height,
                              anomaly=FALSE, seasons=FALSE)

        fname = paste0("ERAsStation_SeasonsAnomlay_",
                       gsub("/", "-",station.data$STATIONS_NAME[1]),
                       "_TimeSeries.pdf")
        titname = paste0('Seasonal windspeed anomaly [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEraSeasons(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                              titname, outdir, fname, width=a4width, height=a4height,
                              anomaly=TRUE, seasons=TRUE)

        #-----------------------------------------------------------------------------

        fname = paste0("ERAsStation_Months_",
                       gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeries_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEraMonths(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                             titname, outdir, fname, width=a4width, height=a4height,
                             anomaly=FALSE)

        fname = paste0("ERAsStation_MonthsAnomlay_",
                       gsub("/", "-", station.data$STATIONS_NAME[1]),
                       "_TimeSeries_", res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('Windspeed anomaly [m/s^2] for station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotStationEraMonths(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                             titname, outdir, fname, width=a4width, height=a4height,
                             anomaly=TRUE)

        #-----------------------------------------------------------------------------

        fname = paste0("PDFscore_ERA20C_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('PDF score of ERA20C and station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotMonthlyPDFScore(era20c.data.xts, MM.station, outdir, fname, titname)

        fname = paste0("PDFscore_ERAI_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('PDF score of ERA-I and station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotMonthlyPDFScore(eraI.data.xts, MM.station, outdir, fname, titname)

        fname = paste0("PDFscore_HErZ_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                       res.switch[res.steps], '_', fname_ext, ".pdf")
        titname = paste0('PDF score of HErZ and station ',
                         as.character(station.data$STATIONS_NAME[1]))
        PlotMonthlyPDFScore(herz.data.xts, MM.station, outdir, fname, titname)

      }
    }
  }

}
