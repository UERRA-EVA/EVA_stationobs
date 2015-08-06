a4width = 29.7/2.54
a4height = 21./2.54

# this switch concerns the resolution of the ERA20C and ERA-Interim reanalyses
# (HR is the version interpolated to 0.125°)
res.switch = "HighRes"   # HighRes, OrigRes
# set whether to use monthly (T) or daily (F) reanalysis data for analysis
era.monthly = TRUE
if (era.monthly) {
  time.ext = "Monthly"
} else {
  time.ext = "Daily"
}

# choose whether to analyse the profile of six model levels of HErZ data or only
# 10m and 116m
herz.profile = FALSE

#=== diagnostics ===
verb.era.dat = FALSE
verb.stat.dat = FALSE
verb.grib = FALSE

#=== Paths ===
# path to store resulting plots into
outdir = "./output/"

#=== ERA20C ===
# filename to ERA20C reanalysis file
if (era.monthly) {
  if (res.switch == "HighRes") {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_MonMean_highRes_1900to2010.nc"
  } else if (res.switch == "OrigRes") {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_MonMean_origRes_1900to2010.nc"
  } else { # first and only necessary check on res.switch
    err = simpleError(paste0("\n   ***\n   Unexpected res.switch, ABORTING!\n   ",
                             "res.switch = ", res.switch,
                             "\n   should be either HighRes or OrigRes\n   ***\n"))
    tryCatch(stop(err))
  }
} else if (!era.monthly) {
  if (res.switch == "HighRes") {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_DayMean_highRes_1900to2010.nc"
  } else if (res.switch == "OrigRes") {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_DayMean_origRes_1900to2010.nc"
  } else {
    err = simpleError(paste0("\n   ***\n   Unexpected res.switch, ABORTING!\n   ",
                             "res.switch = ", res.switch,
                             "\n   should be either HighRes or OrigRes\n   ***\n"))
    tryCatch(stop(err))
  }
} else {  # first and only necessary check on era.monthly
  err = simpleError(paste0("\n   ***\n   Unexpected era.monthly, ABORTING!\n   ",
                           "era.monthly = ", era.monthly,
                           "\n   should be either TRUE or FALSE\n   ***\n"))
  tryCatch(stop(err))
}
CheckFile(era20c.fname)
# variable names to read from above files
era20c.param = c("windspeed_10m", "windspeed_100m")

#=== ERA-I ===
# filename to ERA-Interim reanalysis file
if (era.monthly) {
  if (res.switch == "HighRes") {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_MonMean_highRes_1979to2014.nc"
  } else {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_MonMean_origRes_1979to2014.nc"
  }
} else {
  if (res.switch == "HighRes") {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_DayMean_highRes_1979to2014.nc"
  } else {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_DayMean_origRes_1979to2014.nc"
  }
}
CheckFile(eraI.fname)
# variable names to read from above files
eraI.param = "windspeed_10m"

#=== HErZ ===
# grid file name of the COSMO HErZ reanalysis
herz.grid = "./data/COSMO_REA6_CONST_withOUTsponge.grb"
CheckFile(herz.grid)
# filename(s) of the COSMO HErZ reanalysis file(s)
if (era.monthly) {
  herz.fname = "./data/WindSpeed_HErZ_MonMean_1997to2014.nc"
} else {
  herz.fname = c("./data/WindSpeed_1997_DayMean.nc", "./data/WindSpeed_1998_DayMean.nc",
                 "./data/WindSpeed_1999_DayMean.nc", "./data/WindSpeed_2000_DayMean.nc",
                 "./data/WindSpeed_2001_DayMean.nc", "./data/WindSpeed_2002_DayMean.nc",
                 "./data/WindSpeed_2003_DayMean.nc", "./data/WindSpeed_2004_DayMean.nc",
                 "./data/WindSpeed_2005_DayMean.nc", "./data/WindSpeed_2006_DayMean.nc",
                 "./data/WindSpeed_2007_DayMean.nc", "./data/WindSpeed_2008_DayMean.nc",
                 "./data/WindSpeed_2009_DayMean.nc", "./data/WindSpeed_2010_DayMean.nc",
                 "./data/WindSpeed_2011_DayMean.nc", "./data/WindSpeed_2012_DayMean.nc",
                 "./data/WindSpeed_2013_DayMean.nc", "./data/WindSpeed_2014_DayMean.nc")
}
CheckFile(herz.fname)
# variable names to read from above files
if (herz.profile) {
  herz.param = c("windspeed_10m", "windspeed_35m", "windspeed_69m",
                 "windspeed_116m", "windspeed_178m", "windspeed_258m")
} else {
  herz.param = c("windspeed_10m", "windspeed_116m")
}

#=== Station data ===
# station data based on daily (T) or hourly (F) measurements
station.daily = FALSE
# filename of station name list
station.daily.fname = "./data/KL_Tageswerte_Beschreibung_Stationen.txt"
station.hourly.fname = "./data/FF_Stundenwerte_Beschreibung_Stationen_wind_selected3.txt"
CheckFile(c(station.daily.fname, station.hourly.fname))

#=== time period ===
# available data (beginning to end of year):
#- era20c: 1900 to 2010
#- eraI: 1979 to 2014
#- herz: 1995 to 2014
era20c.tsstart = c(1997,1)
era20c.tsend = c(2010,12)
eraI.tsstart = c(1997,1)
eraI.tsend = c(2010,12)
herz.tsstart = c(1997,1)
herz.tsend = c(2010,12)
fname_ext = "1997to2010"

#=== plotting ===
# The following switches decided on which plots to generate

# monthly mean time series between reanalyses and station data at 10m height
plot.EraStatComp = F
# monthly mean time series between ERA20C and COSMO HErZ reanalyses at 100m height
plot.100mEraHerz = F
# only specific seasons of reanalyses and station data at 10m height - NOT YET FINISHED
plot.EraStationSeasons = F
# only specific months of reanalyses and station data at 10m height
plot.EraStationMonths = T
# PDF score between station data and each reanalysis at 10m height
plot.PDFscore = F
