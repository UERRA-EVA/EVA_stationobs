# this switch concerns the resolution of the ERA20C and ERA-Interim reanalyses
# (HR is the version interpolated to 0.125°)
high.res = "HighRes"
orig.res = "OrigRes"
res.switch = high.res

# set time resolution whether to use monthly, daily, or hourly data for analysis
# by setting parameter ana.time.res$time.res
hourly="hourly"
daily="daily"
monthly="monthly"
time.res=hourly
ana.time.res = list(hourly=hourly, daily=daily, monthly=monthly, time.res=time.res)
if (ana.time.res$time.res == ana.time.res$monthly) {
  time.ext = "Monthly"
} else if (ana.time.res$time.res == ana.time.res$daily) {
  time.ext = "Daily"
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  time.ext = "Hourly"
}

# here choose to only use 10m windspeed of RRAs (especially of HErZ)
herz.profile = F
only.10m = T

#=== diagnostics (verbose) ===
verb.era.dat = FALSE
verb.stat.dat = FALSE
verb.grib = FALSE

#=== Paths ===
# path to store resulting plots into
outdir = "./output/"

#=== ERA20C === -> I do not need ERa20C for this analysis

#=== ERA-I ===
# filename to ERA-Interim reanalysis file
if (ana.time.res$time.res == ana.time.res$monthly) {
  if (res.switch == high.res) {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_MonMean_highRes_1979to2014.nc"
  } else if (res.switch == orig.res) {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_MonMean_origRes_1979to2014.nc"
  }
  CheckFile(eraI.fname)
} else if (ana.time.res$time.res == ana.time.res$daily) {
  if (res.switch == high.res) {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_DayMean_highRes_1979to2014.nc"
  } else if (res.switch == orig.res) {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_DayMean_origRes_1979to2014.nc"
  }
  CheckFile(eraI.fname)
}
# variable names to read from above files
eraI.param = "windspeed_10m"

#=== HErZ ===
# decide whether to read herz grid file in grb (T) or in nc (F) format
herz.grid.read.grb = F
# grid file name of the COSMO HErZ reanalysis
if (herz.grid.read.grb) {
  herz.grid.grb = "./data/COSMO_REA6_CONST_withOUTsponge.grb"
  CheckFile(herz.grid.grb)
} else {
  herz.grid.nc = "./data/COSMO_REA6_CONST_withOUTsponge.nc"
  CheckFile(herz.grid.nc)
}

#=== SMHI ===
# decide whether to read herz grid file in grb (T) or in nc (F) format
smhi.grid.read.grb = F # only netCDF file available
# grid file name of the COSMO HErZ reanalysis
if (smhi.grid.read.grb) {
  smhi.grid.grb = ""
  CheckFile(smhi.grid.grb)
} else {
  smhi.grid.nc = "./data/euro4m.hirlam.latlon.nc"
  CheckFile(smhi.grid.nc)
}

#=== MetOffice ===
# decide whether to read herz grid file in grb (T) or in nc (F) format
mo.grid.read.grb = F # only netCDF file available
# grid file name of the COSMO HErZ reanalysis
if (mo.grid.read.grb) {
  mo.grid.grb = ""
  CheckFile(mo.grid.grb)
} else {
  mo.grid.nc = "./data/euro4m.mo.latlon.nc"
  CheckFile(mo.grid.nc)
}

#=== Meteo France ===
# decide whether to read herz grid file in grb (T) or in nc (F) format
mf.grid.read.grb = F # only netCDF file available
# grid file name of the COSMO HErZ reanalysis
if (mf.grid.read.grb) {
  mf.grid.grb = ""
  CheckFile(mf.grid.grb)
} else {
  mf.grid.nc = "./data/euro4m.mf.latlon.nc"
  CheckFile(mf.grid.nc)
}

# filename(s) of the COSMO HErZ reanalysis file(s)
if (ana.time.res$time.res == ana.time.res$monthly) {
  herz.fname = "./data/WindSpeed_HErZ_MonMean_1995to2014.nc"
  smhi.fname = "./data/SMHI_10m_windspeed_20082009_MonMean.nc"
  mo.fname = "./data/MO_10m_windspeed_20082009_MonMean.nc"
  mf.fname = "./data/MF_10m_windspeed_20082009_MonMean.nc"
} else if (ana.time.res$time.res == ana.time.res$daily) {
  herz.fname = c("./data/WindSpeed_1995_DayMean.nc",
                 "./data/WindSpeed_1996_DayMean.nc",
                 "./data/WindSpeed_1997_DayMean.nc",
                 "./data/WindSpeed_1998_DayMean.nc",
                 "./data/WindSpeed_1999_DayMean.nc",
                 "./data/WindSpeed_2000_DayMean.nc",
                 "./data/WindSpeed_2001_DayMean.nc",
                 "./data/WindSpeed_2002_DayMean.nc",
                 "./data/WindSpeed_2003_DayMean.nc",
                 "./data/WindSpeed_2004_DayMean.nc",
                 "./data/WindSpeed_2005_DayMean.nc",
                 "./data/WindSpeed_2006_DayMean.nc",
                 "./data/WindSpeed_2007_DayMean.nc",
                 "./data/WindSpeed_2008_DayMean.nc",
                 "./data/WindSpeed_2009_DayMean.nc",
                 "./data/WindSpeed_2010_DayMean.nc",
                 "./data/WindSpeed_2011_DayMean.nc",
                 "./data/WindSpeed_2012_DayMean.nc",
                 "./data/WindSpeed_2013_DayMean.nc",
                 "./data/WindSpeed_2014_DayMean.nc")
  smhi.fname = "./data/SMHI_10m_windspeed_20082009_DayMean.nc"
  mo.fname = "./data/MO_10m_windspeed_20082009_DayMean.nc"
  mf.fname = "./data/MF_10m_windspeed_20082009_DayMean.nc"
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  herz.fname = Sys.glob(file.path("/data/mborsche/EURO4M/ExtractedStations/UBDWD",
                                  paste0("*1995TO2014.nc")))
  smhi.fname = Sys.glob(file.path("/data/mborsche/EURO4M/ExtractedStations/SMHI",
                                  paste0("*2008TO2009*.nc")))
  mo.fname = Sys.glob(file.path("/data/mborsche/EURO4M/ExtractedStations/MO",
                                paste0("*2008TO2009*.nc")))
  mf.fname = Sys.glob(file.path("/data/mborsche/EURO4M/ExtractedStations/MF",
                                paste0("*2008TO2009*.nc")))
}
CheckFile(herz.fname)
CheckFile(smhi.fname)
CheckFile(mo.fname)
CheckFile(mf.fname)

# variable names to read from above files
rra.param = c("windspeed_10m")

# names of the reanalyses
eraI.name = "ERA-Interim"
herz.name = "HErZ"
smhi.name = "SMHI"
mo.name = "MetOffice"
mf.name = "Meteo France"

#=== Station data ===
# station data based on daily (T) or hourly (F) measurements; hourly DWD station
# data might be more reliable
station.daily = F
if (ana.time.res$time.res == ana.time.res$hourly) station.daily = F
# filename of station name list
station.daily.fname = "./data/KL_Tageswerte_Beschreibung_Stationen.txt"
station.hourly.fname =
  "./data/FF_Stundenwerte_Beschreibung_Stationen_wind_selected4.txt"
CheckFile(c(station.daily.fname, station.hourly.fname))
# this so far is only a hack; needs to be incorporated in a better way
stat.param = "windspeed_10m"

#=== time period ===
# available data (beginning to end of year):
#- era20c: 1900 to 2010
#- eraI: 1979 to 2014
#- herz: 1995 to 2014
eraI.tsstart = c(2008,1)
eraI.tsend = c(2009,12)
era20c.tsstart = eraI.tsstart # for legacy reasons
era20c.tsend = eraI.tsend # for legacy reasons
herz.tsstart = c(2008,1)
herz.tsend = c(2009,12)
smhi.tsstart = c(2008,1)
smhi.tsend = c(2009,09)
mo.tsstart = c(2008,1)
mo.tsend = c(2009,12)
mf.tsstart = c(2008,1)
mf.tsend = c(2009,12)
fname_ext = "2008to2009"

#=== plotting ===
# The following switches decide on which plots to generate

# time series between reanalyses and station data at 10m height
plot.TS = T
# scatter (and QQ) plot between reanalyses and station data at 10m height
plot.scatter = T
# plot extreme value analysis
plot.Extremes = T
