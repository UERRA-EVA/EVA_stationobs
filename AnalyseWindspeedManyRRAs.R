#!/usr/bin/env Rscript

library(xts)
library(EVAstatobsR)
rm(list=ls())

#===================================================================================
ExtractRRAdataAtStation <- function(rra.grid, rra.fname, rra.tsstart, rra.tsend,
                                    ana.time.res, stat.lon, stat.lat,
                                    herz.profile, only.10m) {
  rra.lon = ReadNetcdf("longitude", rra.grid, conv.time=F)$data
  rra.lat = ReadNetcdf("latitude", rra.grid, conv.time=F)$data

  # only read first file name (if there are more than one)
  # because all daily files have the same grid
  idx = GetLonLatIdx(rra.fname, stat.lon, stat.lat, rra.lon, rra.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  rra.data = ReadHerzNetcdfMonthlyDaily2Xts(rra.param, rra.fname,
                                            rra.tsstart, rra.tsend,
                                            lonidx, latidx,
                                            ana.time.res, herz.profile,
                                            only.10m, verb.era.dat)

  return(rra.data$herz10)
}
#-----------------------------------------------------------------------------------
FindStationNameInRraName <- function(rra.fname, stat.name) {
  for (name.step in seq(rra.fname)) {
    test.str = strsplit(stat.name, ' ')[[1]]
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
      if (grepl(test.str1, rra.fname[name.step]) &&
          grepl(test.str2, rra.fname[name.step])) break
    } else if (length(test.str) == 3) {
      test.str2 = test.str[2]
      test.str3 = test.str[3]
      if (grepl(test.str1, rra.fname[name.step]) &&
          grepl(test.str2, rra.fname[name.step]) &&
          grepl(test.str3, rra.fname[name.step])) break
    } else if (length(test.str) == 4) {
      test.str2 = test.str[2]
      test.str3 = test.str[3]
      test.str4 = test.str[4]
      if (grepl(test.str1, rra.fname[name.step]) &&
          grepl(test.str2, rra.fname[name.step]) &&
          grepl(test.str3, rra.fname[name.step]) &&
          grepl(test.str4, rra.fname[name.step])) break
    } else if(grepl(test.str, rra.fname[name.step])) break
  }

  return(name.step)
}
#===================================================================================

interactive = F
if (interactive) {
  arguments <- commandArgs(TRUE)
  config.file<-arguments[1]
} else {
  setwd("/home/mborsche/work/UERRA/EVA_stationobs")
  config.file = "./SettingsManyRRAs.R"
}
CheckFile(config.file)
source(config.file)

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

  if (ana.time.res$time.res == ana.time.res$monthly |
      ana.time.res$time.res == ana.time.res$daily) {

    stat.lon = station.data$GEO_LÄNGE[1]
    stat.lat = station.data$GEO_BREITE[1]

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
    herz.data.xts = ExtractRRAdataAtStation(herz.grid.nc, herz.fname[1], herz.tsstart,
                                            herz.tsend, ana.time.res, stat.lon,
                                            stat.lat, herz.profile, only.10m)
    herz.hourly.data.xts = NULL
    stats.atherz.data.xts = NULL

    cat(paste0("  **  Reading SMHI reanalysis data\n"))
    smhi.data.xts = ExtractRRAdataAtStation(smhi.grid.nc, smhi.fname[1], smhi.tsstart,
                                            smhi.tsend, ana.time.res, stat.lon,
                                            stat.lat, herz.profile, only.10m)
    smhi.hourly.data.xts = NULL
    stats.atsmhi.data.xts = NULL

    cat(paste0("  **  Reading MO reanalysis data\n"))
    mo.data.xts = ExtractRRAdataAtStation(mo.grid.nc, mo.fname[1], mo.tsstart,
                                          mo.tsend, ana.time.res, stat.lon,
                                          stat.lat, herz.profile, only.10m)
    mo.hourly.data.xts = NULL
    stats.atmo.data.xts = NULL

    cat(paste0("  **  Reading MF reanalysis data\n"))
    mf.data.xts = ExtractRRAdataAtStation(mf.grid.nc, mf.fname[1], mf.tsstart,
                                          mf.tsend, ana.time.res, stat.lon,
                                          stat.lat, herz.profile, only.10m)
    mf.hourly.data.xts = NULL
    stats.atmf.data.xts = NULL

  } else if (ana.time.res$time.res == ana.time.res$hourly) {

    if (station.daily) {
      CallStop(paste0("If hourly station analysis is selected, ",
                      "station data need to be hourly as well."))
    }

    name.step = FindStationNameInRraName(herz.fname, station.name)
    cat(paste0("HErZ at Station:\n   ", herz.fname[name.step], "\n"))
    herz.data = ReadHerzNetcdfHourly2Xts(rra.param, herz.fname[name.step],
                                         herz.tsstart, herz.tsend,
                                         herz.profile, only.10m)
    herz.data.xts = herz.data$herz10
    herz.hourly.data.xts = NULL
    stats.atherz.data.xts = NULL

    name.step = FindStationNameInRraName(smhi.fname, station.name)
    cat(paste0("SMHI at Station:\n   ", smhi.fname[name.step], "\n"))
    smhi.data = ReadHerzNetcdfHourly2Xts(rra.param, smhi.fname[name.step],
                                         smhi.tsstart, smhi.tsend,
                                         herz.profile, only.10m)
    smhi.data.xts = smhi.data$herz10
    smhi.hourly.data.xts = AligneRRA2Obsxts(MM.station, smhi.data.xts)
    stats.atsmhi.data.xts = AligneObs2RRAxts(MM.station, smhi.data.xts)


    name.step = FindStationNameInRraName(mo.fname, station.name)
    cat(paste0("MetOffice at Station:\n   ", mo.fname[name.step], "\n"))
    mo.data = ReadHerzNetcdfHourly2Xts(rra.param, mo.fname[name.step],
                                       mo.tsstart, mo.tsend,
                                       herz.profile, only.10m)
    mo.data.xts = mo.data$herz10
    mo.hourly.data.xts = AligneRRA2Obsxts(MM.station, mo.data.xts)
    stats.atmo.data.xts = AligneObs2RRAxts(MM.station, mo.data.xts)

    name.step = FindStationNameInRraName(mf.fname, station.name)
    cat(paste0("Meteo France at Station:\n   ", mf.fname[name.step], "\n"))
    mf.data = ReadHerzNetcdfHourly2Xts(rra.param, mf.fname[name.step],
                                       mf.tsstart, mf.tsend,
                                       herz.profile, only.10m)
    mf.data.xts = mf.data$herz10
    mf.hourly.data.xts = AligneRRA2Obsxts(MM.station, mf.data.xts)
    stats.atmf.data.xts = AligneObs2RRAxts(MM.station, mf.data.xts)

    eraI.data.xts = NULL
  }
}

# == get time series of same length ==
# === data in objects are ordered from heighest to lowest height ===
climobj = Get10mRRAObsObject(obs.xts=MM.station,
                             rra10.xts=herz.data.xts,
                             rra10.hourly.xts=herz.hourly.data.xts,
                             stats.atrra10.xts=stats.atherz.data.xts,
                             eraI10.xts=NULL,
                             obs.tsstart=stat.tsstart, obs.tsend=stat.tsend,
                             rra.tsend=herz.tsend, eraI.tsend=NULL,
                             rra.name=herz.name,
                             obs.name=as.character(station.info$Stationsname),
                             obs.lon=station.info$geoLaenge,
                             obs.lat=station.info$geoBreite, obs.param=stat.param,
                             eraI.param="")
StatHerz.climobj = climobj$obs.object$climate_data_objects

climobj = Get10mRRAObsObject(obs.xts=MM.station,
                             rra10.xts=smhi.data.xts,
                             rra10.hourly.xts=smhi.hourly.data.xts,
                             stats.atrra10.xts=stats.atsmhi.data.xts,
                             eraI10.xts=NULL,
                             obs.tsstart=stat.tsstart, obs.tsend=stat.tsend,
                             rra.tsend=smhi.tsend, eraI.tsend=NULL,
                             rra.name=smhi.name, eraI.name="",
                             obs.name=as.character(station.info$Stationsname),
                             obs.lon=station.info$geoLaenge,
                             obs.lat=station.info$geoBreite, obs.param="",
                             eraI.param="")
StatSMHI.climobj = climobj$obs.object$climate_data_objects

climobj = Get10mRRAObsObject(obs.xts=MM.station,
                             rra10.xts=mo.data.xts,
                             rra10.hourly.xts=mo.hourly.data.xts,
                             stats.atrra10.xts=stats.atmo.data.xts,
                             eraI10.xts=NULL,
                             obs.tsstart=stat.tsstart, obs.tsend=stat.tsend,
                             rra.tsend=mo.tsend, eraI.tsend=NULL,
                             rra.name=mo.name, eraI.name="",
                             obs.name=as.character(station.info$Stationsname),
                             obs.lon=station.info$geoLaenge,
                             obs.lat=station.info$geoBreite, obs.param="",
                             eraI.param="")
StatMO.climobj = climobj$obs.object$climate_data_objects

climobj = Get10mRRAObsObject(obs.xts=MM.station,
                             rra10.xts=mf.data.xts,
                             rra10.hourly.xts=mo.hourly.data.xts,
                             stats.atrra10.xts=stats.atmo.data.xts,
                             eraI10.xts=NULL,
                             obs.tsstart=stat.tsstart, obs.tsend=stat.tsend,
                             rra.tsend=mf.tsend, eraI.tsend=NULL,
                             rra.name=mf.name, eraI.name="",
                             obs.name=as.character(station.info$Stationsname),
                             obs.lon=station.info$geoLaenge,
                             obs.lat=station.info$geoBreite, obs.param=stat.param,
                             eraI.param="")
StatMF.climobj = climobj$obs.object$climate_data_objects

fname = paste0(outdir, "RRAs-timeSeries_20082009_", time.ext,"_",
               res.switch, ".pdf")
hourly.switch = T # plot hourly data (T); or at time res of RRA only (F)
PlotRRAtimeSeries(StatHerz.climobj, StatSMHI.climobj,
                  StatMO.climobj, StatMF.climobj, fname)

a = "asdf"
# read monthly RRA data (need to cdo monmean that data)
# read monthly station data (that should be straight forward, right?)

# read hourly RRA data (read data as is)
# read hourly station data (read data as is)

# plot what's to be plotted
