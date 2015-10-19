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
CheckTowerParams(lind.param, tower.name="Lindenberg")
CheckTowerParams(cabauw.param, tower.name="Cabauw")

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

# for Cabauw
dat = ExtractTowerData(cabauw.file, cabauw.param, era.monthly)
cabauw.10.xts = dat[[cabauw.param[[1]]]]
cabauw.20.xts = dat[[cabauw.param[[2]]]]
cabauw.40.xts = dat[[cabauw.param[[3]]]]
cabauw.80.xts = dat[[cabauw.param[[4]]]]
cabauw.140.xts = dat[[cabauw.param[[5]]]]
cabauw.200.xts = dat[[cabauw.param[[6]]]]

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
era20c10.fino1.xts = era20c.data$era10
era20c100.fino1.xts = era20c.data$era20c100

# for FINO2
idx = GetLonLatIdx(era20c.fname, fino2.lon, fino2.lat)
lonidx = idx$lonidx
latidx = idx$latidx
era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                era20c.tsstart, era20c.tsend,
                                lonidx, latidx, era.monthly,
                                era20c=TRUE, verb.dat=verb.era.dat)
era20c10.fino2.xts = era20c.data$era10
era20c100.fino2.xts = era20c.data$era20c100

# for Lindenberg
idx = GetLonLatIdx(era20c.fname, lind.lon, lind.lat)
lonidx = idx$lonidx
latidx = idx$latidx
era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                era20c.tsstart, era20c.tsend,
                                lonidx, latidx, era.monthly,
                                era20c=TRUE, verb.dat=verb.era.dat)
era20c10.lind.xts = era20c.data$era10
era20c100.lind.xts = era20c.data$era20c100

# for Cabauw
idx = GetLonLatIdx(era20c.fname, cabauw.lon, cabauw.lat)
lonidx = idx$lonidx
latidx = idx$latidx
era20c.data = ReadEraNetcdf2Xts(era20c.param, era20c.fname,
                                era20c.tsstart, era20c.tsend,
                                lonidx, latidx, era.monthly,
                                era20c=TRUE, verb.dat=verb.era.dat)
era20c10.cabauw.xts = era20c.data$era10
era20c100.cabauw.xts = era20c.data$era20c100


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

# for Cabauw
idx = GetLonLatIdx(herz.fname[1], cabauw.lon, cabauw.lat, herz.lon, herz.lat)
lonidx = idx$lonidx
latidx = idx$latidx
herz.data = ReadHerzNetcdfMonthlyDaily2Xts(herz.param, herz.fname,
                                           herz.tsstart, herz.tsend,
                                           lonidx, latidx,
                                           era.monthly, herz.profile,
                                           verb.era.dat)
herz10.cabauw.xts = herz.data$herz10
herz35.cabauw.xts = herz.data$herz35
herz69.cabauw.xts = herz.data$herz69
herz116.cabauw.xts = herz.data$herz116
herz178.cabauw.xts = herz.data$herz178
herz258.cabauw.xts = herz.data$herz258

# == get time series of same length ==
climobj = GetTowerObject(tower.xts=fino1.100.xts,
                         herz10.xts=herz10.fino1.xts,
                         herz35.xts=herz35.fino1.xts,
                         herz69.xts=herz69.fino1.xts,
                         herz116.xts=herz116.fino1.xts,
                         herz178.xts=herz178.fino1.xts,
                         herz258.xts=herz258.fino1.xts,
                         era20c10.xts=era20c10.fino1.xts,
                         era20c100.xts=era20c100.fino1.xts,
                         tower.tsstart=fino1.tsstart, tower.tsend=fino1.tsend,
                         herz.tsend=herz.tsend, era20c.tsend=era20c.tsend,
                         tower.name="Fino1", tower.lon=fino1.lon,
                         tower.lat=fino1.lat, tower.param=fino1.param,
                         herz.param=herz.param, era20c.param=era20c.param)
fino1.climobj = climobj$tower.object

climobj = GetTowerObject(tower.xts=fino2.102.xts,
                         herz10.xts=herz10.fino2.xts,
                         herz35.xts=herz35.fino2.xts,
                         herz69.xts=herz69.fino2.xts,
                         herz116.xts=herz116.fino2.xts,
                         herz178.xts=herz178.fino2.xts,
                         herz258.xts=herz258.fino2.xts,
                         era20c10.xts=era20c10.fino2.xts,
                         era20c100.xts=era20c100.fino2.xts,
                         tower.tsstart=fino2.tsstart, tower.tsend=fino2.tsend,
                         herz.tsend=herz.tsend, era20c.tsend=era20c.tsend,
                         tower.name="Fino2", tower.lon=fino2.lon,
                         tower.lat=fino2.lat, tower.param=fino2.param,
                         herz.param=herz.param, era20c.param=era20c.param)
fino2.climobj = climobj$tower.object

climobj = GetTowerObject(tower.xts=lind.10.xts, tower2.xts=lind.20.xts,
                         tower3.xts=lind.40.xts, tower4.xts=lind.60.xts,
                         tower5.xts=lind.80.xts, tower6.xts=lind.98.xts,
                         herz10.xts=herz10.lind.xts,
                         herz35.xts=herz35.lind.xts,
                         herz69.xts=herz69.lind.xts,
                         herz116.xts=herz116.lind.xts,
                         herz178.xts=herz178.lind.xts,
                         herz258.xts=herz258.lind.xts,
                         era20c10.xts=era20c10.lind.xts,
                         era20c100.xts=era20c100.lind.xts,
                         tower.tsstart=lind.tsstart, tower.tsend=lind.tsend,
                         herz.tsend=herz.tsend, era20c.tsend=era20c.tsend,
                         tower.name="Lindenberg", tower.lon=lind.lon,
                         tower.lat=lind.lat, tower.param=lind.param,
                         herz.param=herz.param, era20c.param=era20c.param)
lind.climobj = climobj$tower.object

climobj = GetTowerObject(tower.xts=cabauw.10.xts, tower2.xts=cabauw.20.xts,
                         tower3.xts=cabauw.40.xts, tower4.xts=cabauw.80.xts,
                         tower5.xts=cabauw.140.xts, tower6.xts=cabauw.200.xts,
                         herz10.xts=herz10.cabauw.xts,
                         herz35.xts=herz35.cabauw.xts,
                         herz69.xts=herz69.cabauw.xts,
                         herz116.xts=herz116.cabauw.xts,
                         herz178.xts=herz178.cabauw.xts,
                         herz258.xts=herz258.cabauw.xts,
                         era20c10.xts=era20c10.cabauw.xts,
                         era20c100.xts=era20c100.cabauw.xts,
                         tower.tsstart=cabauw.tsstart, tower.tsend=cabauw.tsend,
                         herz.tsend=herz.tsend, era20c.tsend=era20c.tsend,
                         tower.name="Cabauw", tower.lon=cabauw.lon,
                         tower.lat=cabauw.lat, tower.param=cabauw.param,
                         herz.param=herz.param, era20c.param=era20c.param)
cabauw.climobj = climobj$tower.object

#-----------------------------------------------------------------------------

if (plot.TowerEraProfile) {
  cat("  **  Plotting tower-ERA profile box plots\n")
  fname = paste0(outdir, "LindenbergHErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(lind.climobj, fname, era.monthly)

  fname = paste0(outdir, "Fino1HErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(fino1.climobj, fname, era.monthly)

  fname = paste0(outdir, "Fino2HErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(fino2.climobj, fname, era.monthly)

  fname = paste0(outdir, "CabauwHErZERA20C_boxPlots_", time.ext,"_",
                 res.switch, '_', fname_ext, ".pdf")
  PlotTowerERAprofileBP(cabauw.climobj, fname, era.monthly)
}

#-----------------------------------------------------------------------------

if(plot.histograms) {
  cat("  **  Plotting Histograms\n")
  fname = paste0("Histogram_Lindenberg_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, era.monthly, lind.climobj)

  fname = paste0("Histogram_Fino1_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, era.monthly, fino1.climobj)

  fname = paste0("Histogram_Fino2_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, era.monthly, fino2.climobj)

  fname = paste0("Histogram_Cabauw_", res.switch, '_', time.ext, "_",
                 fname_ext, ".pdf")
  PlotHistogramsTower(outdir, fname, era.monthly, cabauw.climobj)
}

#-----------------------------------------------------------------------------

if (plot.ProfileTS & era.monthly) {
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

  extremes.thresh = seq(0.5,0.99,0.01)
  if (era.monthly) {
    fname = paste0(outdir, "Fino1_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino1.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Fino2_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(fino2.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Cabauw_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(cabauw.climobj, fname, extremes.thresh)
    fname = paste0(outdir, "Lindenberg_MM-extremes_", res.switch, '_', fname_ext, ".pdf")
    PlotTowerExtremesContr(lind.climobj, fname, extremes.thresh)
  } else {
    if (TowerHour) {
      fname = paste0(outdir, "Fino1_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
      PlotTowerExtremesContr(fino1.climobj, fname, extremes.thresh)
      fname = paste0(outdir, "Fino2_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
      PlotTowerExtremesContr(fino2.climobj, fname, extremes.thresh)
      fname = paste0(outdir, "Cabauw_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
      PlotTowerExtremesContr(cabauw.climobj, fname, extremes.thresh)
      fname = paste0(outdir, "Lindenberg_HH-extremes_", res.switch, '_', fname_ext, ".pdf")
      PlotTowerExtremesContr(lind.climobj, fname, extremes.thresh)
    } else {
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
}
