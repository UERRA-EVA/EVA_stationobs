#!/usr/bin/env Rscript

library(EVAstatobsR)
rm(list=ls())

interactive = F
if (interactive) {
  arguments <- commandArgs(TRUE)
  config.file<-arguments[1]
  CheckFile(config.file)
  source(config.file)
} else {
  setwd("~/work/UERRA/EVA_stationobs")
  config.file = "./Settings.R"
  CheckFile(config.file)
  source(config.file)
  config.file = "./SettingsTower.R"
  CheckFile(config.file)
  source(config.file)
}

# == Checks on parameters ==
CheckHerzParams(herz.param, herz.profile)
if (!herz.profile) CallStop("HErZ profile is needed!")
CheckLindParams(lind.param)

# == read tower measurements ==
cat(paste0("  **  Reading tower measurements\n"))
# for FINO1
dat = ExtractTowerData(fino1.file, fino1.param, era.monthly)
fino1.100.xts = dat[[fino1.param]]

# for FINO2
dat = ExtractTowerData(fino2.file, fino2.param, era.monthly)
fino2.102.xts = dat[[fino2.param]]

# for Lindenberg
dat = ExtractTowerData(lind.file, lind.param, era.monthly)
lind.10.xts = dat[[lind.param[1]]]
lind.20.xts = dat[[lind.param[2]]]
lind.40.xts = dat[[lind.param[3]]]
lind.60.xts = dat[[lind.param[4]]]
lind.80.xts = dat[[lind.param[5]]]
lind.98.xts = dat[[lind.param[6]]]

# == read ERA20C data ==
cat(paste0("  **  Reading ERA20C reanalysis data\n"))
# for FINO1
idx = GetLonLatIdx(era20c.fname, fino1.lon, fino1.lat)
lonidx = idx$lonidx
latidx = idx$latidx
era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                era20c.tsstart, era20c.tsend,
                                lonidx, latidx, era.monthly,
                                era20c=TRUE, verb.dat=verb.era.dat)
era20c.fino1.xts = era20c.data$era10
era20c100.fino1.xts = era20c.data$era20c100

# for FINO2
idx = GetLonLatIdx(era20c.fname, fino2.lon, fino2.lat)
lonidx = idx$lonidx
latidx = idx$latidx
era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                era20c.tsstart, era20c.tsend,
                                lonidx, latidx, era.monthly,
                                era20c=TRUE, verb.dat=verb.era.dat)
era20c.fino2.xts = era20c.data$era10
era20c100.fino2.xts = era20c.data$era20c100

# for Lindenberg
idx = GetLonLatIdx(era20c.fname, lind.lon, lind.lat)
lonidx = idx$lonidx
latidx = idx$latidx
era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                era20c.tsstart, era20c.tsend,
                                lonidx, latidx, era.monthly,
                                era20c=TRUE, verb.dat=verb.era.dat)
era20c.lind.xts = era20c.data$era10
era20c100.lind.xts = era20c.data$era20c100

# == read HErZ data ==
cat(paste0("  **  Reading HErZ reanalysis data\n"))
nlon = 848
nlat = 824
herz.lon = readGrib(herz.grid, nlon, nlat, 1, var='RLON', verb.grib=verb.grib)
herz.lat = readGrib(herz.grid, nlon, nlat, 1, var='RLAT', verb.grib=verb.grib)

# for FINO1
# only read first file name (if there are more than one)
# because all daily files have the same grid
idx = GetLonLatIdx(herz.fname[1], fino1.lon, fino1.lat, herz.lon, herz.lat)
lonidx = idx$lonidx
latidx = idx$latidx
herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                           herz.tsstart, herz.tsend,
                                           lonidx, latidx,
                                           era.monthly, herz.profile,
                                           verb.era.dat)
herz10.fino1.xts = herz.data$herz10
herz35.fino1.xts = herz.data$herz35
herz69.fino1.xts = herz.data$herz69
herz116.fino1.xts = herz.data$herz116
herz178.fino1.xts = herz.data$herz178
herz258.fino1.xts = herz.data$herz258

# for FINO2
idx = GetLonLatIdx(herz.fname[1], fino2.lon, fino2.lat, herz.lon, herz.lat)
lonidx = idx$lonidx
latidx = idx$latidx
herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                           herz.tsstart, herz.tsend,
                                           lonidx, latidx,
                                           era.monthly, herz.profile,
                                           verb.era.dat)
herz10.fino2.xts = herz.data$herz10
herz35.fino2.xts = herz.data$herz35
herz69.fino2.xts = herz.data$herz69
herz116.fino2.xts = herz.data$herz116
herz178.fino2.xts = herz.data$herz178
herz258.fino2.xts = herz.data$herz258

# for Lindenberg
idx = GetLonLatIdx(herz.fname[1], lind.lon, lind.lat, herz.lon, herz.lat)
lonidx = idx$lonidx
latidx = idx$latidx
herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                           herz.tsstart, herz.tsend,
                                           lonidx, latidx,
                                           era.monthly, herz.profile,
                                           verb.era.dat)
herz10.lind.xts = herz.data$herz10
herz35.lind.xts = herz.data$herz35
herz69.lind.xts = herz.data$herz69
herz116.lind.xts = herz.data$herz116
herz178.lind.xts = herz.data$herz178
herz258.lind.xts = herz.data$herz258

#-----------------------------------------------------------------------------

if (plot.TowerEraProfile) {
  cat("  **  Plotting tower-ERA profile\n")
  fname = paste0("TowerERAprofile_TimeSeries", time.ext,"_", res.switch, '_',
                 fname_ext, ".pdf")
  titname = paste0()
  PlotTowerERAprofile(herz10.data.xts, herz35.data.xts, herz69.data.xts,
                      herz116.data.xts, herz178.data.xts, herz258.data.xts,
                      era20c100.data.xts)
}

#-----------------------------------------------------------------------------
