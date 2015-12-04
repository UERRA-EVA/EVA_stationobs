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
if (ana.time.res$time.res != ana.time.res$hourly) {
  CheckHerzParams(herz.param, herz.profile)
}

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

station.data = read.table(station.fname, skip=2, sep=";")
colnames(station.data) = c("Station_id", "von_datum", "bis_datum", "Stationshoehe",
                           "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
station.info = station.data[,c("Station_id", "Stationsname", "geoBreite",
                               "geoLaenge")]
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

  station.name = as.character(station.info[[2]][steps])
  cat(paste0("  **  Reading station data: ", station.name, "\n"))
  station.data = data.frame()
  station.data = AllData(station.info[[1]][steps], station.name,
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
  if (any(!is.finite(MM.station)) && ana.time.res$time.res != hourly) {
    cat(paste0("\n  ***  ",
               "There were non-finite values in the station data record ",
               "for this time period. ***\n\n"))
    #     next
  }
  stat.tsstart = c(head(as.POSIXlt(index(MM.station))$year+1900,1),
                   head(as.POSIXlt(index(MM.station))$mon+1,1))
  stat.tsend = c(tail(as.POSIXlt(index(MM.station))$year+1900,1),
                 tail(as.POSIXlt(index(MM.station))$mon+1,1))

  #=================================
  #
  #  read monthly and daily ERA data
  #
  #=================================

  if (ana.time.res$time.res == ana.time.res$monthly |
      ana.time.res$time.res == ana.time.res$daily) {

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
    if (herz.grid.read.grb) {
      nlon = 848
      nlat = 824
      herz.lon = readGrib(herz.grid.grb, nlon, nlat, 1, var="RLON", verb.grib=verb.grib)
      herz.lat = readGrib(herz.grid.grb, nlon, nlat, 1, var="RLAT", verb.grib=verb.grib)
    } else {
      herz.lon = ReadNetcdf("longitude", herz.grid.nc, conv.time=F)$data
      herz.lat = ReadNetcdf("latitude", herz.grid.nc, conv.time=F)$data
    }

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

  } else if (ana.time.res$time.res == ana.time.res$hourly) {

    if (station.daily) {
      CallStop(paste0("If hourly station analysis is selected, ",
                      "station data need to be hourly as well."))
    }

    # aligne station.name to file name in herz.fname
    for (name.step in seq(herz.fname)) {
      test.str = strsplit(station.name, ' ')[[1]]
      for (j in seq(test.str)) {
        test.str[j] = gsub("\\(", "", test.str[j])
        test.str[j] = gsub("\\)", "", test.str[j])
        test.str[j] = gsub("\\)", "", test.str[j])
        test.str[j] = gsub("/", "", test.str[j])
        test.str[j] = gsub(",", "", test.str[j])
      }
      test.str1 = test.str[1]
      if (length(test.str) == 2) {
        test.str2 = test.str[2]
        if (grepl(test.str1, herz.fname[name.step]) &&
            grepl(test.str2, herz.fname[name.step])) break
      } else if (length(test.str) == 3) {
        test.str2 = test.str[2]
        test.str3 = test.str[3]
        if (grepl(test.str1, herz.fname[name.step]) &&
            grepl(test.str2, herz.fname[name.step]) &&
            grepl(test.str3, herz.fname[name.step])) break
      } else if (length(test.str) == 4) {
        test.str2 = test.str[2]
        test.str3 = test.str[3]
        test.str4 = test.str[4]
        if (grepl(test.str1, herz.fname[name.step]) &&
            grepl(test.str2, herz.fname[name.step]) &&
            grepl(test.str3, herz.fname[name.step]) &&
            grepl(test.str4, herz.fname[name.step])) break
      } else if(grepl(test.str, herz.fname[name.step])) break
    }
    cat(paste0("Station: ", herz.fname[name.step], "\n"))

    herz.data = ReadHerzNetcdfHourly2Xts(herz.param, herz.fname[name.step],
                                         herz.tsstart, herz.tsend,
                                         herz.profile)
    herz10.data.xts = herz.data$herz10
    herz35.data.xts = herz.data$herz35
    herz69.data.xts = herz.data$herz69
    herz116.data.xts = herz.data$herz116
    herz178.data.xts = herz.data$herz178
    herz258.data.xts = herz.data$herz258

#     # assign NaN values to herz data as in station data
#     if (any(!is.finite(MM.station))) {
#       idx = which(!is.finite(MM.station))
#       herz10.data.xts[idx] = NA
#       herz35.data.xts[idx] = NA
#       herz69.data.xts[idx] = NA
#       herz116.data.xts[idx] = NA
#       herz178.data.xts[idx] = NA
#       herz258.data.xts[idx] = NA
#     }
    era20c.data.xts = NULL
    era20c100.data.xts = NULL
    eraI.data.xts = NULL
  }

  # == get time series of same length ==
  # === data in objects are ordered from heighest to lowest height ===
  climobj = GetObsObject(obs.xts=MM.station,
                         herz10.xts=herz10.data.xts,
                         herz35.xts=herz35.data.xts,
                         herz69.xts=herz69.data.xts,
                         herz116.xts=herz116.data.xts,
                         herz178.xts=herz178.data.xts,
                         herz258.xts=herz258.data.xts,
                         eraI10.xts=eraI.data.xts,
                         era20c10.xts=era20c.data.xts,
                         era20c100.xts=era20c100.data.xts,
                         obs.tsstart=stat.tsstart, obs.tsend=stat.tsend,
                         herz.tsend=herz.tsend, eraI.tsend=eraI.tsend,
                         era20c.tsend=era20c.tsend,
                         obs.name=as.character(station.info$Stationsname),
                         obs.lon=station.info$geoLaenge,
                         obs.lat=station.info$geoBreite,
                         obs.param=stat.param, eraI.param=eraI.param,
                         era20c.param=era20c.param)
  stat.climobj = climobj$obs.object

  #-----------------------------------------------------------------------------

  if (plot.EraStatComp) {
    cat("  **  Plotting Era-station comparison\n")
    fname = paste0("ERA-Station_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries", time.ext,"_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('wind speed [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEra(stat.climobj$climate_data_objects,
                   titname, outdir, fname, anomaly=FALSE)

    fname = paste0("ERA-Station_Anomlay_", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries", time.ext, "_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('wind speed anomaly [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEra(stat.climobj$climate_data_objects,
                   titname, outdir, fname, anomaly=TRUE)
  }

  #------------------------------------------------------------------------------

  if ((plot.100mEraHerz & ana.time.res$time.res == ana.time.res$monthly) |
      (plot.100mEraHerz & ana.time.res$time.res == ana.time.res$daily)) {
    cat("  **  Plotting 100m Era vs HErZ\n")

    statname = as.character(station.data$STATIONS_NAME[1])
    titname = paste0('100m ERA20C and 116m HErZ wind speed [m/s] at station location ',
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
    titname = paste0('Seasonal wind speed [m/s] for station ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSelSeasons(era20c.data.xts, eraI.data.xts, herz10.data.xts,
                             MM.station, titname, outdir, fname,
                             anomaly=FALSE, seasons=FALSE)

    fname = paste0("ERA-Station_SeasonsAnomlay_",
                   gsub("/", "-",station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Seasonal wind speed anomaly [m/s] for station ',
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
    titname = paste0('Monthly wind speed [m/s] at station location ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSelMonths(era20c.data.xts, eraI.data.xts, herz10.data.xts,
                            MM.station, titname, outdir, fname, anomaly=FALSE)

    fname = paste0("ERA-Station_", time.ext, "Anomlay_",
                   gsub("/", "-", station.data$STATIONS_NAME[1]),
                   "_TimeSeries_", res.switch, '_', fname_ext, ".pdf")
    titname = paste0('Monthly wind speed anomaly [m/s] at station location ',
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
    titname = paste0('wind speed [m/s] at station location ',
                     as.character(station.data$STATIONS_NAME[1]))
    PlotStationEraSQ(era20c.data.xts, eraI.data.xts, herz10.data.xts, MM.station,
                     titname, outdir, fname, ana.time.res)
  }

  #-----------------------------------------------------------------------------

  if(plot.PDFscore) {

    cat("  **  Plotting PDFscore\n")
    fname = paste0("PDFscore_ERA20C-", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    titname = paste0('ERA20C and ', as.character(station.data$STATIONS_NAME[1]))
    PlotPDFScore(era20c.data.xts, MM.station, outdir, fname, titname,
                 ana.time.res)

    fname = paste0("PDFscore_ERAI-", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    titname = paste0('ERA-I and ', as.character(station.data$STATIONS_NAME[1]))
    PlotPDFScore(eraI.data.xts, MM.station, outdir, fname, titname,
                 ana.time.res)

    fname = paste0("PDFscore_HErZ-", gsub("/", "-", station.data$STATIONS_NAME[1]),
                   '_', res.switch, '_', time.ext, "_", fname_ext, ".pdf")
    titname = paste0('HErZ and ', as.character(station.data$STATIONS_NAME[1]))
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
