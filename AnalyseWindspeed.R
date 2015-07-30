#!/usr/bin/env Rscript

library(xts)
library(EVAstatobsR)
rm(list=ls())

interactive = F
if (interactive) {
  arguments <- commandArgs(TRUE)
  config.file<-arguments[1]
} else {
  setwd("/home/mborsche/work/UERRA/EVA_stationobs")
  config.file = "./Settings.R"
  CheckFile(config.file)
}
CheckFile(config.file)
source(config.file)

# checks on variables:
if (!res.switch == "HighRes" & !res.switch == "OrigRes") {
  cat("\n   *** Unexpected res.switch, ABORTING!\n   res.switch = ", res.switch,
      "\n   should be either HighRes or OrigRes\n   ***\n")
  quit(status=1)
}

#============================================================
#
#  read station data and extract ERA data at station location
#
#============================================================

# read station data
path = "~/datensicherung/transfer/"
if (station.daily) {
  station.fname = station.daily.fname
} else {
  station.fname = station.hourly.fname
}
CheckFile(station.fname)

if (station.daily) {
  station.data = read.table(station.fname, skip=2, sep=";")
  colnames(station.data) = c("Station_id", "von_datum", "bis_datum", "Stationshoehe",
                             "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
} else {
  station.data = read.table(station.fname, skip=2, sep=";")
  colnames(station.data) = c("Station_id", "von_datum", "bis_datum", "Stationshoehe",
                             "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
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
  cat(paste0("\n  **  Reading station data: ", station.info[[2]][steps],"\n\n"))
  station.data = data.frame()
  station.data = all.data(station.info[[1]][steps], station.info[[2]][steps],
                          station.info[[3]][steps], station.info[[4]][steps],
                          daily = station.daily)

  MM.station = ExtractStationData(station.data, era20c.tsstart, era20c.tsend,
                                  eraI.tsstart, eraI.tsend,
                                  herz.tsstart, herz.tsend,
                                  daily=station.daily)
  if(length(MM.station) == 0) {
    cat(paste0("\n  ***  ",
               "The length of the station data record ",
               "was zero for this time period. ***\n\n"))
    next
  }
  if (any(!is.finite(MM.station))) {
    cat(paste0("\n  ***  ",
               "There were no finite values in the station data record ",
               "for this time period. ***\n\n"))
    next
  }

  #================================
  #
  #  read MM / [ DD (??) ] ERA data
  #
  #================================
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
    era20c.data = era20c$data
    era20c.lon = era20c$lon
    era20c.lat = era20c$lat
    era20c.time.vals = era20c$time

    era20c = ReadNetcdf(era20c100.param, era20c.fname)
    era20c100.data = era20c$data

    # read ERA-Interim monthly mean windspeed
    if (res.switch[res.steps] == "HighRes") {
      eraI.fname = eraI.HRes.fname
    } else {
      eraI.fname = eraI.ORes.fname
    }
    CheckFile(eraI.fname)

    eraI = ReadNetcdf(eraI.param, eraI.fname)
    eraI.data = eraI$data
    eraI.lon = eraI$lon
    eraI.lat = eraI$lat
    eraI.time.vals = eraI$time

    # read HErZ monthly mean windspeed
    CheckFile(herz.grid)
    nlon = 848
    nlat = 824
    herz.lon = readGrib(herz.grid, nlon, nlat, 1, var='RLON')
    herz.lat = readGrib(herz.grid, nlon, nlat, 1, var='RLAT')

    CheckFile(herz.fname)
    dat = ReadNetcdf(var=herz10.param, infile=herz.fname)
    herz10.data <- dat$data
    herz.time.vals <- dat$time

    dat = ReadNetcdf(var=herz116.param, infile=herz.fname)
    herz116.data <- dat$data

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

    if (plot.EraStatComp) {
      fname = paste0("ERA-Station_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     "_TimeSeriesMonthly_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('Windspeed [m/s^2] for station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                     titname, outdir, fname, width=a4width, height=a4height,
                     monthly=TRUE, anomaly=FALSE)

      fname = paste0("ERA-Station_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     "_TimeSeriesAnnual_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('Windspeed [m/s^2] for station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                     titname, outdir, fname, width=a4width, height=a4height,
                     monthly=FALSE, anomaly=FALSE)

      fname = paste0("ERA-Station_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     "_TimeSeriesMonthly_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('Windspeed anomaly [m/s^2] for station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                     titname, outdir, fname, width=a4width, height=a4height,
                     monthly=TRUE, anomaly=TRUE)

      fname = paste0("ERA-Station_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     "_TimeSeriesAnnual_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('Windspeed anomaly [m/s^2] for station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotStationEra(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                     titname, outdir, fname, width=a4width, height=a4height,
                     monthly=FALSE, anomaly=TRUE)
    }

    #------------------------------------------------------------------------------

    if (plot.100mEraHerz) {
      ### COMPARE 100m ERA20c AND 116m HErZ###
      statname = as.character(station.data$STATIONS_NAME[1])
      fname = paste0("100m-Era20cHerz_", gsub("/", "-", statname),
                     "_TimeSeriesMonthly_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('100m Windspeed [m/s^2] at station location ', statname)
      Plot100mEraHerz(era20c100.data.xts, herz116.data.xts, titname, statname,
                      outdir, fname, width=a4width, height=a4height)
    }

    #-----------------------------------------------------------------------------

    #       if (plot.EraStationSeasons) {
    #         #@*** THIS PLOTTING FUNCTION NEEDS TO BE FINALIZED ***@#
    #         fname = paste0("ERA-Station_Seasons_", gsub("/", "-", station.data$STATIONS_NAME[1]),
    #                        "_TimeSeries1979.pdf")
    #         titname = paste0('Seasonal windspeed [m/s^2] for station ',
    #                          as.character(station.data$STATIONS_NAME[1]))
    #         PlotStationEraSeasons(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
    #                               titname, outdir, fname, width=a4width, height=a4height,
    #                               anomaly=FALSE, seasons=FALSE)
    #
    #         fname = paste0("ERA-Station_SeasonsAnomlay_",
    #                        gsub("/", "-",station.data$STATIONS_NAME[1]),
    #                        "_TimeSeries.pdf")
    #         titname = paste0('Seasonal windspeed anomaly [m/s^2] for station ',
    #                          as.character(station.data$STATIONS_NAME[1]))
    #         PlotStationEraSeasons(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
    #                               titname, outdir, fname, width=a4width, height=a4height,
    #                               anomaly=TRUE, seasons=TRUE)
    #       }

    #-----------------------------------------------------------------------------

    if (plot.EraStationMonths) {
      #@*** THIS PLOTTING FUNCTION NEEDS TO BE FINALIZED ***@#
      fname = paste0("ERA-Station_Months_",
                     gsub("/", "-", station.data$STATIONS_NAME[1]),
                     "_TimeSeries_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('Windspeed [m/s^2] for station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotStationEraMonths(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                           titname, outdir, fname, width=a4width, height=a4height,
                           anomaly=FALSE)

      fname = paste0("ERA-Station_MonthsAnomlay_",
                     gsub("/", "-", station.data$STATIONS_NAME[1]),
                     "_TimeSeries_", res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('Windspeed anomaly [m/s^2] for station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotStationEraMonths(era20c.data.xts, eraI.data.xts, herz.data.xts, MM.station,
                           titname, outdir, fname, width=a4width, height=a4height,
                           anomaly=TRUE)
    }

    #-----------------------------------------------------------------------------

    if(plot.PDFscore) {
      fname = paste0("PDFscore_ERA20C_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     '_', res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('PDF score of ERA20C and station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotMonthlyPDFScore(era20c.data.xts, MM.station, outdir, fname, titname)

      fname = paste0("PDFscore_ERAI_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     '_', res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('PDF score of ERA-I and station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotMonthlyPDFScore(eraI.data.xts, MM.station, outdir, fname, titname)

      fname = paste0("PDFscore_HErZ_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                     '_', res.switch[res.steps], '_', fname_ext, ".pdf")
      titname = paste0('PDF score of HErZ and station ',
                       as.character(station.data$STATIONS_NAME[1]))
      PlotMonthlyPDFScore(herz.data.xts, MM.station, outdir, fname, titname)
    }

  }
}
