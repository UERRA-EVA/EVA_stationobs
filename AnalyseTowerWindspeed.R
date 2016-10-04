#!/usr/bin/env Rscript

rm(list=ls())
library(EVAstatobsR)
source("~/work/UERRA/EVA_stationobs/PlotHist_SeparatePlots.R")
source("~/work/UERRA/EVA_stationobs/Calc.thresholds.R")

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

if (use.synthetic) {
  cat("  **  Plotting scores and skill scores of randomly distributed data\n")

  # prepare one pair of random data with predifined n
  n = 100000
  use.distr = "Gaussian" # "Gaussian", "Weibull"
  data.vals = PrepareRandomData(n, use.distr)

    # put that data into a ClimObject
  climobj = GetRandomClimObject(data.vals$obs, data.vals$forec)
  random.climobj = climobj$random.object
  # plot extremes
  extremes.thresh = seq(0.01,0.99,0.01)
  fname = paste0(outdir, paste0(use.distr, "-randomly-distributed-hourly-extremes.pdf"))
  PlotRandomExtremesContr(random.climobj, fname, extremes.thresh)

  CallStop("Program finished normally")
}

# == Checks on parameters ==
if (ana.time.res$time.res != ana.time.res$hourly) {
  CheckHerzParams(herz.param, herz.profile)
} else {
  CheckHerzHeightOrder(herz.fname$Fino1, herz.profile)
  CheckHerzHeightOrder(herz.fname$Fino2, herz.profile)
  CheckHerzHeightOrder(herz.fname$Lindenberg, herz.profile)
  CheckHerzHeightOrder(herz.fname$Cabauw, herz.profile)
}
if (!herz.profile) CallStop("HErZ profile is needed!")
CheckTowerParams(lind.param, tower.name="Lindenberg")
CheckTowerParams(cabauw.param, tower.name="Cabauw")

# == read tower measurements ==
cat(paste0("  **  Reading tower measurements\n"))
# for FINO1
dat = ExtractTowerData(fino1.file, fino1.param, ana.time.res)
fino1.100.xts = dat[[fino1.param]]

# for FINO2
dat = ExtractTowerData(fino2.file, fino2.param, ana.time.res)
fino2.102.xts = dat[[fino2.param]]

# for Lindenberg
dat = ExtractTowerData(lind.file, lind.param, ana.time.res)
lind.10.xts = dat[[lind.param[6]]]
lind.20.xts = dat[[lind.param[5]]]
lind.40.xts = dat[[lind.param[4]]]
lind.60.xts = dat[[lind.param[3]]]
lind.80.xts = dat[[lind.param[2]]]
lind.98.xts = dat[[lind.param[1]]]

# for Cabauw
dat = ExtractTowerData(cabauw.file, cabauw.param, ana.time.res)
cabauw.10.xts = dat[[cabauw.param[[6]]]]
cabauw.20.xts = dat[[cabauw.param[[5]]]]
cabauw.40.xts = dat[[cabauw.param[[4]]]]
cabauw.80.xts = dat[[cabauw.param[[3]]]]
cabauw.140.xts = dat[[cabauw.param[[2]]]]
cabauw.200.xts = dat[[cabauw.param[[1]]]]

if (ana.time.res$time.res == monthly | ana.time.res$time.res == daily) {
  # == read ERA20C data ==
  cat(paste0("  **  Reading global reanalysis data\n"))
  # for FINO1
  idx = GetLonLatIdx(era20c.fname, fino1.lon, fino1.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  era20c.data = ReadEraNetcdf2Xts(era20c.param, c(era20c.fname,era20c.fname),
                                  era20c.tsstart, era20c.tsend,
                                  lonidx, latidx, ana.time.res,
                                  read.10m=TRUE, read.100m=TRUE, verb.dat=verb.era.dat)
  era20c10.fino1.xts = era20c.data$era10
  era20c100.fino1.xts = era20c.data$era100
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=TRUE, read.100m=FALSE, verb.dat=verb.era.dat)
  eraI10.fino1.xts = eraI.data$era10
  idx = GetLonLatIdx(eraI.fname[2], fino1.lon, fino1.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=FALSE, read.100m=TRUE, verb.dat=verb.era.dat)
  eraI100.fino1.xts = eraI.data$era100

  # for FINO2
  idx = GetLonLatIdx(era20c.fname, fino2.lon, fino2.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  era20c.data = ReadEraNetcdf2Xts(era20c.param, c(era20c.fname,era20c.fname),
                                  era20c.tsstart, era20c.tsend,
                                  lonidx, latidx, ana.time.res,
                                  read.10m=TRUE, read.100m=TRUE, verb.dat=verb.era.dat)
  era20c10.fino2.xts = era20c.data$era10
  era20c100.fino2.xts = era20c.data$era100
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=TRUE, read.100m=FALSE, verb.dat=verb.era.dat)
  eraI10.fino2.xts = eraI.data$era10
  idx = GetLonLatIdx(eraI.fname[2], fino1.lon, fino1.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=FALSE, read.100m=TRUE, verb.dat=verb.era.dat)
  eraI100.fino2.xts = eraI.data$era100

  # for Lindenberg
  idx = GetLonLatIdx(era20c.fname, lind.lon, lind.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  era20c.data = ReadEraNetcdf2Xts(era20c.param, c(era20c.fname,era20c.fname),
                                  era20c.tsstart, era20c.tsend,
                                  lonidx, latidx, ana.time.res,
                                  read.10m=TRUE, read.100m=TRUE, verb.dat=verb.era.dat)
  era20c10.lind.xts = era20c.data$era10
  era20c100.lind.xts = era20c.data$era100
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=TRUE, read.100m=FALSE, verb.dat=verb.era.dat)
  eraI10.lind.xts = eraI.data$era10
  idx = GetLonLatIdx(eraI.fname[2], fino1.lon, fino1.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=FALSE, read.100m=TRUE, verb.dat=verb.era.dat)
  eraI100.lind.xts = eraI.data$era100

  # for Cabauw
  idx = GetLonLatIdx(era20c.fname, cabauw.lon, cabauw.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  era20c.data = ReadEraNetcdf2Xts(era20c.param, c(era20c.fname,era20c.fname),
                                  era20c.tsstart, era20c.tsend,
                                  lonidx, latidx, ana.time.res,
                                  read.10m=TRUE, read.100m=TRUE, verb.dat=verb.era.dat)
  era20c10.cabauw.xts = era20c.data$era10
  era20c100.cabauw.xts = era20c.data$era100
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=TRUE, read.100m=FALSE, verb.dat=verb.era.dat)
  eraI10.cabauw.xts = eraI.data$era10
  idx = GetLonLatIdx(eraI.fname[2], fino1.lon, fino1.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  eraI.data = ReadEraNetcdf2Xts(eraI.param, eraI.fname,
                                eraI.tsstart, eraI.tsend,
                                lonidx, latidx, ana.time.res,
                                read.10m=FALSE, read.100m=TRUE, verb.dat=verb.era.dat)
  eraI100.cabauw.xts = eraI.data$era100

  # == read HErZ data ==
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

  # for FINO1
  # only read first file name (if there are more than one)
  # because all daily files have the same grid
  idx = GetLonLatIdx(herz.fname[1], fino1.lon, fino1.lat, herz.lon, herz.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                             herz.tsstart, herz.tsend,
                                             lonidx, latidx,
                                             ana.time.res, herz.profile,
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
                                             ana.time.res, herz.profile,
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
                                             ana.time.res, herz.profile,
                                             verb.era.dat)
  herz10.lind.xts = herz.data$herz10
  herz35.lind.xts = herz.data$herz35
  herz69.lind.xts = herz.data$herz69
  herz116.lind.xts = herz.data$herz116
  herz178.lind.xts = herz.data$herz178
  herz258.lind.xts = herz.data$herz258

  # for Cabauw
  idx = GetLonLatIdx(herz.fname[1], cabauw.lon, cabauw.lat, herz.lon, herz.lat)
  lonidx = idx$lonidx
  latidx = idx$latidx
  herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                             herz.tsstart, herz.tsend,
                                             lonidx, latidx,
                                             ana.time.res, herz.profile,
                                             verb.era.dat)
  herz10.cabauw.xts = herz.data$herz10
  herz35.cabauw.xts = herz.data$herz35
  herz69.cabauw.xts = herz.data$herz69
  herz116.cabauw.xts = herz.data$herz116
  herz178.cabauw.xts = herz.data$herz178
  herz258.cabauw.xts = herz.data$herz258

} else if (ana.time.res$time.res == ana.time.res$hourly) {

  herz.fname.names = names(herz.fname)
  for (name.step in seq(herz.fname.names)) {
    tower.hourly.data = ReadHerzNetcdfHourly2Xts(herz.param, herz.fname[[name.step]],
                                                 herz.tsstart, herz.tsend,
                                                 herz.profile)
    if (herz.fname.names[name.step] == "Fino1") {
      herz10.fino1.xts = tower.hourly.data$herz10
      herz35.fino1.xts = tower.hourly.data$herz35
      herz69.fino1.xts = tower.hourly.data$herz69
      herz116.fino1.xts = tower.hourly.data$herz116
      herz178.fino1.xts = tower.hourly.data$herz178
      herz258.fino1.xts = tower.hourly.data$herz258
    } else if (herz.fname.names[name.step] == "Fino2") {
      herz10.fino2.xts = tower.hourly.data$herz10
      herz35.fino2.xts = tower.hourly.data$herz35
      herz69.fino2.xts = tower.hourly.data$herz69
      herz116.fino2.xts = tower.hourly.data$herz116
      herz178.fino2.xts = tower.hourly.data$herz178
      herz258.fino2.xts = tower.hourly.data$herz258
    } else if (herz.fname.names[name.step] == "Lindenberg") {
      herz10.lind.xts = tower.hourly.data$herz10
      herz35.lind.xts = tower.hourly.data$herz35
      herz69.lind.xts = tower.hourly.data$herz69
      herz116.lind.xts = tower.hourly.data$herz116
      herz178.lind.xts = tower.hourly.data$herz178
      herz258.lind.xts = tower.hourly.data$herz258
    } else if (herz.fname.names[name.step] == "Cabauw") {
      herz10.cabauw.xts = tower.hourly.data$herz10
      herz35.cabauw.xts = tower.hourly.data$herz35
      herz69.cabauw.xts = tower.hourly.data$herz69
      herz116.cabauw.xts = tower.hourly.data$herz116
      herz178.cabauw.xts = tower.hourly.data$herz178
      herz258.cabauw.xts = tower.hourly.data$herz258
    } else {
      CallStop(paste0("This tower name was not expected: ",
                      herz.fname.names[name.step]))
    }
  }
  era20c10.fino1.xts = NULL
  era20c100.fino1.xts = NULL
  eraI10.fino1.xts = NULL
  era20c10.fino2.xts = NULL
  era20c100.fino2.xts = NULL
  eraI10.fino2.xts = NULL
  era20c10.lind.xts = NULL
  era20c100.lind.xts = NULL
  eraI10.lind.xts = NULL
  era20c10.cabauw.xts = NULL
  era20c100.cabauw.xts = NULL
  eraI10.cabauw.xts = NULL
}

# == get time series of same length ==
# === data in objects are ordered from heighest to lowest height ===
climobj = GetObsObject(obs.xts=fino1.100.xts,
                       herz10.xts=herz10.fino1.xts,
                       herz35.xts=herz35.fino1.xts,
                       herz69.xts=herz69.fino1.xts,
                       herz116.xts=herz116.fino1.xts,
                       herz178.xts=herz178.fino1.xts,
                       herz258.xts=herz258.fino1.xts,
                       eraI10.xts=eraI10.fino1.xts,
                       eraI100.xts=eraI100.fino1.xts,
                       era20c10.xts=era20c10.fino1.xts,
                       era20c100.xts=era20c100.fino1.xts,
                       obs.tsstart=fino1.tsstart, obs.tsend=fino1.tsend,
                       herz.tsend=herz.tsend, eraI.tsend=eraI.tsend,
                       era20c.tsend=era20c.tsend,
                       obs.name="Fino1", obs.lon=fino1.lon,
                       obs.lat=fino1.lat, obs.param=fino1.param,
                       eraI.param=eraI.param, era20c.param=era20c.param,
                       herz.profile=herz.profile)
fino1.climobj = climobj$obs.object

climobj = GetObsObject(obs.xts=fino2.102.xts,
                       herz10.xts=herz10.fino2.xts,
                       herz35.xts=herz35.fino2.xts,
                       herz69.xts=herz69.fino2.xts,
                       herz116.xts=herz116.fino2.xts,
                       herz178.xts=herz178.fino2.xts,
                       herz258.xts=herz258.fino2.xts,
                       eraI10.xts=eraI10.fino2.xts,
                       eraI100.xts=eraI100.fino2.xts,
                       era20c10.xts=era20c10.fino2.xts,
                       era20c100.xts=era20c100.fino2.xts,
                       obs.tsstart=fino2.tsstart, obs.tsend=fino2.tsend,
                       herz.tsend=herz.tsend, eraI.tsend=eraI.tsend,
                       era20c.tsend=era20c.tsend,
                       obs.name="Fino2", obs.lon=fino2.lon,
                       obs.lat=fino2.lat, obs.param=fino2.param,
                       eraI.param=eraI.param, era20c.param=era20c.param,
                       herz.profile=herz.profile)
fino2.climobj = climobj$obs.object

climobj = GetObsObject(obs.xts=lind.98.xts, obs2.xts=lind.80.xts,
                       obs3.xts=lind.60.xts, obs4.xts=lind.40.xts,
                       obs5.xts=lind.20.xts, obs6.xts=lind.10.xts,
                       herz10.xts=herz10.lind.xts,
                       herz35.xts=herz35.lind.xts,
                       herz69.xts=herz69.lind.xts,
                       herz116.xts=herz116.lind.xts,
                       herz178.xts=herz178.lind.xts,
                       herz258.xts=herz258.lind.xts,
                       eraI10.xts=eraI10.lind.xts,
                       eraI100.xts=eraI100.lind.xts,
                       era20c10.xts=era20c10.lind.xts,
                       era20c100.xts=era20c100.lind.xts,
                       obs.tsstart=lind.tsstart, obs.tsend=lind.tsend,
                       herz.tsend=herz.tsend, eraI.tsend=eraI.tsend,
                       era20c.tsend=era20c.tsend,
                       obs.name="Lindenberg", obs.lon=lind.lon,
                       obs.lat=lind.lat, obs.param=lind.param,
                       eraI.param=eraI.param, era20c.param=era20c.param,
                       herz.profile=herz.profile)
lind.climobj = climobj$obs.object

climobj = GetObsObject(obs.xts=cabauw.200.xts, obs2.xts=cabauw.140.xts,
                       obs3.xts=cabauw.80.xts, obs4.xts=cabauw.40.xts,
                       obs5.xts=cabauw.20.xts, obs6.xts=cabauw.10.xts,
                       herz10.xts=herz10.cabauw.xts,
                       herz35.xts=herz35.cabauw.xts,
                       herz69.xts=herz69.cabauw.xts,
                       herz116.xts=herz116.cabauw.xts,
                       herz178.xts=herz178.cabauw.xts,
                       herz258.xts=herz258.cabauw.xts,
                       eraI10.xts=eraI10.cabauw.xts,
                       eraI100.xts=eraI100.cabauw.xts,
                       era20c10.xts=era20c10.cabauw.xts,
                       era20c100.xts=era20c100.cabauw.xts,
                       obs.tsstart=cabauw.tsstart, obs.tsend=cabauw.tsend,
                       herz.tsend=herz.tsend, eraI.tsend=eraI.tsend,
                       era20c.tsend=era20c.tsend,
                       obs.name="Cabauw", obs.lon=cabauw.lon,
                       obs.lat=cabauw.lat, obs.param=cabauw.param,
                       eraI.param=eraI.param, era20c.param=era20c.param,
                       herz.profile=herz.profile)
cabauw.climobj = climobj$obs.object

#-----------------------------------------------------------------------------

if (calc.threshold) {
  Calc.thresholds(fino1.climobj$climate_data_objects)
  Calc.thresholds(fino2.climobj$climate_data_objects)
  Calc.thresholds(lind.climobj$climate_data_objects)
  Calc.thresholds(cabauw.climobj$climate_data_objects)
}

if (plot.TowerEraProfile) {
  cat("  **  Plotting tower-ERA profile box plots\n")
  fname = paste0(outdir, "LindenbergHErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(lind.climobj, fname, ana.time.res)

  fname = paste0(outdir, "Fino1HErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(fino1.climobj, fname, ana.time.res)

  fname = paste0(outdir, "Fino2HErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(fino2.climobj, fname, ana.time.res)

  fname = paste0(outdir, "CabauwHErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(cabauw.climobj, fname, ana.time.res)
}

#-----------------------------------------------------------------------------

if (plot.separate.hist) {
  cat("  **  Plotting histograms separately one on each page")

  fname = paste0(outdir, "Histogram_Lindenberg-98m_", res.switch, ".pdf")
  titname = "Hourly wind speed at Lindenberg in 98m"
  PlotHist_SeparatePlots(fname,
                         lind.climobj$climate_data_objects$obs$data$wind_speed,
                         lind.climobj$climate_data_objects$obs$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_REA6atLindenberg-116m_", res.switch, ".pdf")
  titname = "Hourly wind speed of COSMO-REA6\nat Lindenberg in 116m"
  PlotHist_SeparatePlots(fname,
                         lind.climobj$climate_data_objects$herz116$data$wind_speed,
                         lind.climobj$climate_data_objects$herz116$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_Cabauw-80m_", res.switch, ".pdf")
  titname = "Hourly wind speed at Cabauw in 80m"
  PlotHist_SeparatePlots(fname,
                         cabauw.climobj$climate_data_objects$obs3$data$wind_speed,
                         cabauw.climobj$climate_data_objects$obs3$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_REA6atCabauw-69m_", res.switch, ".pdf")
  titname = "Hourly wind speed of COSMO-REA6\nat Cabauw in 69m"
  PlotHist_SeparatePlots(fname,
                         cabauw.climobj$climate_data_objects$herz69$data$wind_speed,
                         cabauw.climobj$climate_data_objects$herz69$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_Cabauw-140m_", res.switch, ".pdf")
  titname = "Hourly wind speed at Cabauw in 140m"
  PlotHist_SeparatePlots(fname,
                         cabauw.climobj$climate_data_objects$obs2$data$wind_speed,
                         cabauw.climobj$climate_data_objects$obs2$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_REA6atCabauw-116m_", res.switch, ".pdf")
  titname = "Hourly wind speed of COSMO-REA6\nat Cabauw in 116m"
  PlotHist_SeparatePlots(fname,
                         cabauw.climobj$climate_data_objects$herz116$data$wind_speed,
                         cabauw.climobj$climate_data_objects$herz116$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_FINO1-100m_", res.switch, ".pdf")
  titname = "Hourly wind speed at FINO1 in 100m"
  PlotHist_SeparatePlots(fname,
                         fino1.climobj$climate_data_objects$obs$data$wind_speed,
                         fino1.climobj$climate_data_objects$obs$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_REA6atFINO1-116m_", res.switch, ".pdf")
  titname = "Hourly wind speed of COSMO-REA6\nat FINO1 in 116m"
  PlotHist_SeparatePlots(fname,
                         fino1.climobj$climate_data_objects$herz116$data$wind_speed,
                         fino1.climobj$climate_data_objects$herz116$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_FINO2-102m_", res.switch, ".pdf")
  titname = "Hourly wind speed at FINO2 in 102m"
  PlotHist_SeparatePlots(fname,
                         fino2.climobj$climate_data_objects$obs$data$wind_speed,
                         fino2.climobj$climate_data_objects$obs$data$date,
                         titname)

  fname = paste0(outdir, "Histogram_REA6atFINO2-116m_", res.switch, ".pdf")
  titname = "Hourly wind speed of COSMO-REA6\nat FINO2 in 116m"
  PlotHist_SeparatePlots(fname,
                         fino2.climobj$climate_data_objects$herz116$data$wind_speed,
                         fino2.climobj$climate_data_objects$herz116$data$date,
                         titname)
}

if(plot.histograms) {
  cat("  **  Plotting Histograms\n")

  if (herz.profile) {
    plot.HerzProfile = TRUE
  } else {
    plot.HerzProfile = FALSE
  }
  plot.10m = TRUE
  plot.100m = TRUE

  fname = paste0("Histogram_Lindenberg_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, ana.time.res, lind.climobj,
                      plot.10m = plot.10m, plot.100m = plot.100m,
                      plot.HerzProfile = plot.HerzProfile)

  fname = paste0("Histogram_Fino1_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, ana.time.res, fino1.climobj,
                      plot.10m = plot.10m, plot.100m = plot.100m,
                      plot.HerzProfile = plot.HerzProfile)

  fname = paste0("Histogram_Fino2_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, ana.time.res, fino2.climobj,
                      plot.10m = plot.10m, plot.100m = plot.100m,
                      plot.HerzProfile = plot.HerzProfile)

  fname = paste0("Histogram_Cabauw_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, ana.time.res, cabauw.climobj,
                      plot.10m = plot.10m, plot.100m = plot.100m,
                      plot.HerzProfile = plot.HerzProfile)
}

#-----------------------------------------------------------------------------

if (plot.ProfileTS & ana.time.res$time.res == ana.time.res$monthly) {
  cat("  **  Plotting tower-ERA profile TS\n")
  fname = paste0(outdir, "LindenbergHErZERA20C_relativeDifferences_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileRelDiff(lind.climobj, fname)
  fname = paste0(outdir, "Fino1HErZERA20C_relativeDifferences_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileRelDiff(fino1.climobj, fname)
  fname = paste0(outdir, "Fino2HErZERA20C_relativeDifferences_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileRelDiff(fino2.climobj, fname)
  fname = paste0(outdir, "CabauwHErZERA20C_relativeDifferences_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileRelDiff(cabauw.climobj, fname)

  fname = paste0(outdir, "LindenbergHErZERA20C_selectedMonths_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualVar(lind.climobj, fname)
  fname = paste0(outdir, "Fino1HErZERA20C_selectedMonths_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualVar(fino1.climobj, fname)
  fname = paste0(outdir, "Fino2HErZERA20C_selectedMonths_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualVar(fino2.climobj, fname)
  fname = paste0(outdir, "CabauwHErZERA20C_selectedMonths_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualVar(cabauw.climobj, fname)

  fname = paste0(outdir, "LindenbergHErZERA20C_annualCycle_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualCycle(lind.climobj, fname)
  fname = paste0(outdir, "Fino1HErZERA20C_annualCycle_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualCycle(fino1.climobj, fname)
  fname = paste0(outdir, "Fino2HErZERA20C_annualCycle_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualCycle(fino2.climobj, fname)
  fname = paste0(outdir, "CabauwHErZERA20C_annualCycle_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileAnnualCycle(cabauw.climobj, fname)
}

#-----------------------------------------------------------------------------

if(plot.Extremes) {
  cat("  **  Plotting tower extreme values and their scores\n")
  extremes.thresh = seq(0.01,0.99,0.01)
  if (ana.time.res$time.res == ana.time.res$monthly) {
    fname = paste0(outdir, "Fino1_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino1.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Fino2_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino2.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Cabauw_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(cabauw.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Lindenberg_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(lind.climobj, fname, extremes.thresh)
  } else if (ana.time.res$time.res == ana.time.res$hourly) {
    fname = paste0(outdir, "Fino1_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino1.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Fino2_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino2.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Cabauw_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(cabauw.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Lindenberg_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(lind.climobj, fname, extremes.thresh)
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    fname = paste0(outdir, "Fino1_DD-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino1.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Fino2_DD-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino2.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Cabauw_DD-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(cabauw.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Lindenberg_DD-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(lind.climobj, fname, extremes.thresh)
  }
}

#-----------------------------------------------------------------------------

if(plot.DailyCycle & ana.time.res$time.res == ana.time.res$hourly) {
  cat("  **  Plotting the daily cycle of tower measurements\n")
  fname = paste0(outdir, "HErZ-Fino1_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingHerzDailyCycle(fino1.climobj, fname)
  fname = paste0(outdir, "HErZ-Fino2_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingHerzDailyCycle(fino2.climobj, fname)
  fname = paste0(outdir, "HErZ-Cabauw_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingHerzDailyCycle(cabauw.climobj, fname)
  fname = paste0(outdir, "HErZ-Lindenberg_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingHerzDailyCycle(lind.climobj, fname)

  fname = paste0(outdir, "Fino1_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingTowerDailyCycle(fino1.climobj, fname)
  fname = paste0(outdir, "Fino2_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingTowerDailyCycle(fino2.climobj, fname)
  fname = paste0(outdir, "Cabauw_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingTowerDailyCycle(cabauw.climobj, fname)
  fname = paste0(outdir, "Lindenberg_DailyCycle_", res.switch, '_', fname_ext, ".pdf")
  PreparePlottingTowerDailyCycle(lind.climobj, fname)
}

#-----------------------------------------------------------------------------
