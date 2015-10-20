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
}
CheckFile(config.file)
source(config.file)

#Checks on parameters
CheckHerzParams(herz.param, herz.profile)

#============================================================
#
#  read station data and extract ERA data at station location
#
#============================================================

# read station data
if (station.daily) {
  station.fname = station.daily.fname
} else {
  station.fname = station.hourly.fname
}

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
  cat(paste0("  **  Reading station data: ", station.info[[2]][steps], "\n"))
  station.data = data.frame()
  station.data = AllData(station.info[[1]][steps], station.info[[2]][steps],
                         station.info[[3]][steps], station.info[[4]][steps],
                         daily = station.daily, verbose.DWD=verb.stat.dat)

  MM.station = ExtractStationData(station.data, era20c.tsstart, era20c.tsend,
                                  eraI.tsstart, eraI.tsend,
                                  herz.tsstart, herz.tsend,
                                  ana.time.res, station.daily)
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
    #     next
  }

  #=================================
  #
  #  read monthly and daily ERA data
  #
  #=================================

  # read ERA20C data
  cat(paste0("  **  Reading ERA20C reanalysis data\n"))
  stat.lon = station.data$GEO_LÄNGE[1]
  stat.lat = station.data$GEO_BREITE[1]
  idx = GetLonLatIdx(era20c.fname, stat.lon, stat.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                  era20c.tsstart, era20c.tsend,
                                  lonidx, latidx, ana.time.res,
                                  era20c=TRUE, verb.dat=verb.era.dat)
  era20c.data.xts = era20c.data$era10
  era20c100.data.xts = era20c.data$era20c100

  # read ERA-Interim data
  cat(paste0("  **  Reading ERA-Interim reanalysis data\n"))
  idx = GetLonLatIdx(eraI.fname, stat.lon, stat.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                era20c=FALSE, verb.dat=verb.era.dat)
  eraI.data.xts = eraI.data$era10

  # read HErZ data
  cat(paste0("  **  Reading HErZ reanalysis data\n"))
  nlon = 848
  nlat = 824
  herz.lon = readGrib(herz.grid, nlon, nlat, 1, var='RLON', verb.grib=verb.grib)
  herz.lat = readGrib(herz.grid, nlon, nlat, 1, var='RLAT', verb.grib=verb.grib)

  # only read first file name (if there are more than one)
  # because all daily files have the same grid
  idx = GetLonLatIdx(herz.fname[1], stat.lon, stat.lat, herz.lon, herz.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                             herz.tsstart, herz.tsend,
                                             lonidx, latidx,
                                             ana.time.res, herz.profile,
                                             verb.era.dat)
  herz10.data.xts = herz.data$herz10
  herz116.data.xts = herz.data$herz116
  if (herz.profile) {
    herz35.data.xts = herz.data$herz35
    herz69.data.xts = herz.data$herz69
    herz178.data.xts = herz.data$herz178
    herz258.data.xts = herz.data$herz258
  }

  #-----------------------------------------------------------------------------

  if (plot.EraStatComp) {
    cat("  **  Plotting Era-station comparison\n")
    fname = paste0("ERA-Station_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries", time.ext,"_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Windspeed [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEra(era20c.data.xts, eraI.data.xts, herz10.data.xts, MM.station,
                   titname, outdir, fname, monthly=TRUE, anomaly=FALSE)

    fname = paste0("ERA-Station_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeriesAnnual_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Windspeed [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEra(era20c.data.xts, eraI.data.xts, herz10.data.xts, MM.station,
                   titname, outdir, fname, monthly=FALSE, anomaly=FALSE)

    fname = paste0("ERA-Station_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries", time.ext, "_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Windspeed anomaly [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEra(era20c.data.xts, eraI.data.xts, herz10.data.xts, MM.station,
                   titname, outdir, fname, monthly=TRUE, anomaly=TRUE)

    fname = paste0("ERA-Station_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeriesAnnual_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Windspeed anomaly [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEra(era20c.data.xts, eraI.data.xts, herz10.data.xts, MM.station,
                   titname, outdir, fname, monthly=FALSE, anomaly=TRUE)
  }

  #------------------------------------------------------------------------------

  if (plot.100mEraHerz) {
    cat("  **  Plotting 100m Era vs HErZ\n")

    statname = as.character(station.data$STATIONS_NAME[1])
    titname = paste0('100m ERA20C and 116m HErZ windspeed [m/s] at station location ',
                     statname)

    fname = paste0("100m-Era20cHerz_", gsub("/", "-", statname),
                   "_TimeSeries-", time.ext, "_", res.switch, '_', fname_ext, ".pdf")
    Plot100mEraHerz(era20c100.data.xts, herz116.data.xts, titname, statname,
                    outdir, fname, ana.time.res)
    fname = paste0("PDFScore_100mEraHerz_", statname, "_TimeSeries-", time.ext,
                   "_", res.switch, '_', fname_ext, ".pdf")
    PlotPDFScore(era20c100.data.xts, herz116.data.xts, outdir, fname, titname,
                 ana.time.res)
  }

  #-----------------------------------------------------------------------------

  if (plot.EraStationSelSeasons & ana.time.res$time.res == ana.time.res$monthly) {
    cat("  **  Plotting selected seasonal time series\n")
    fname = paste0("ERA-Station_Seasons_",
                   gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Seasonal windspeed [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSelSeasons(era20c.data.xts, eraI.data.xts, herz10.data.xts,
                             MM.station, titname, outdir, fname,
                             anomaly=FALSE, seasons=FALSE)

    fname = paste0("ERA-Station_SeasonsAnomlay_",
                   gsub("/", "-",station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Seasonal windspeed anomaly [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSelSeasons(era20c.data.xts, eraI.data.xts, herz10.data.xts,
                             MM.station, titname, outdir, fname,
                             anomaly=TRUE, seasons=TRUE)
  }

  #-----------------------------------------------------------------------------

  if (plot.EraStationSelMonths & ana.time.res$time.res == ana.time.res$monthly) {
    cat("  **  Plotting selected monthly time series\n")
    fname = paste0("ERA-Station_", time.ext, "_",
                   gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Monthly windspeed [m/s] at station location ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSelMonths(era20c.data.xts, eraI.data.xts, herz10.data.xts,
                            MM.station, titname, outdir, fname, anomaly=FALSE)

    fname = paste0("ERA-Station_", time.ext, "Anomlay_",
                   gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Monthly windspeed anomaly [m/s] at station location ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSelMonths(era20c.data.xts, eraI.data.xts, herz10.data.xts,
                            MM.station, titname, outdir, fname, anomaly=TRUE)
  }

  #-----------------------------------------------------------------------------

  if(plot.EraStationSQ) {
    cat("  **  Plotting scatter and QQ-plot analysis\n")
    fname = paste0("ERA-Station_", time.ext, "_",
                   gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('windspeed [m/s] at station location ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSQ(era20c.data.xts, eraI.data.xts, herz10.data.xts, MM.station,
                      titname, outdir, fname, ana.time.res)

  }

  #-----------------------------------------------------------------------------

  if(plot.HerzProfile) {
    cat("  **  Plotting HErZ profile\n")
  }

  #-----------------------------------------------------------------------------

  if(plot.PDFscore) {

    cat("  **  Plotting PDFscore\n")
    fname = paste0("PDFscore_ERA20C_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    titname = c('ERA20C ', as.character(station.data$STATIONS_NAME[1]))
    PlotPDFScore(era20c.data.xts, MM.station, outdir, fname, titname,
                 ana.time.res)

    fname = paste0("PDFscore_ERAI_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    titname = c('ERA-I ', as.character(station.data$STATIONS_NAME[1]))
    PlotPDFScore(eraI.data.xts, MM.station, outdir, fname, titname,
                 ana.time.res)

    fname = paste0("PDFscore_HErZ_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    titname = c('HErZ ', as.character(station.data$STATIONS_NAME[1]))
    PlotPDFScore(herz10.data.xts, MM.station, outdir, fname, titname,
                 ana.time.res)
  }

  #-----------------------------------------------------------------------------

  if(plot.histograms) {

    cat("  **  Plotting Histograms\n")
    fname = paste0("Histogram_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    statname = gsub("/", "-", station.data$STATIONS_NAME[1])
    if (herz.profile) {
      PlotHistograms(outdir, fname, statname, ana.time.res,
                     era20c.data.xts, era20c100.data.xts, eraI.data.xts,
                     herz10.data.xts, herz35.data.xts, herz69.data.xts,
                     herz116.data.xts, herz178.data.xts, herz258.data.xts,
                     MM.station,
                     plot.10m=TRUE, plot.10m100m=TRUE, plot.HerzProfile=TRUE)

    } else {
      PlotHistograms(outdir, fname, statname, ana.time.res,
                     era20c.data.xts, era20c100.data.xts, eraI.data.xts,
                     herz10.data.xts, HerzXts35=NULL, HerzXts69=NULL,
                     herz116.data.xts, HerzXts178=NULL, HerzXts258=NULL,
                     MM.station,
                     plot.10m=TRUE, plot.10m100m=TRUE, plot.HerzProfile=FALSE)
    }
  }
}
