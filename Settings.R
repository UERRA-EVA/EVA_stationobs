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
time.res=monthly
ana.time.res = list(hourly=hourly, daily=daily, monthly=monthly, time.res=time.res)
if (ana.time.res$time.res == ana.time.res$monthly) {
  time.ext = "Monthly"
} else if (ana.time.res$time.res == ana.time.res$daily) {
  time.ext = "Daily"
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  time.ext = "Hourly"
}

# choose whether to analyse the profile (T) of six model levels of HErZ data or only
# 10m and 116m (F)
herz.profile = T

#=== diagnostics (verbose) ===
verb.era.dat = FALSE
verb.stat.dat = FALSE
verb.grib = FALSE

#=== Paths ===
# path to store resulting plots into
outdir = "./output/"

#=== ERA20C ===
# filename to ERA20C reanalysis file
if (ana.time.res$time.res == ana.time.res$monthly) {
  if (res.switch == high.res) {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_MonMean_highRes_1900to2010.nc"
  } else if (res.switch == orig.res) {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_MonMean_origRes_1900to2010.nc"
  }
  CheckFile(era20c.fname)
} else if (ana.time.res$time.res == ana.time.res$daily) {
  if (res.switch == high.res) {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_DayMean_highRes_1900to2010.nc"
  } else if (res.switch == orig.res) {
    era20c.fname =
      "./data/10m100m_WindSpeedDirection_ERA20C_DayMean_origRes_1900to2010.nc"
  }
  CheckFile(era20c.fname)
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  if (res.switch == high.res) {
    era20c.fname =
      list(Fino1=c("./data/ERA20C_windspeed10m100m_2000to2010_Fino1.nc"),
           Fino2=c("./data/ERA20C_windspeed10m100m_2000to2010_Fino2.nc"),
           Lindenberg=c("./data/ERA20C_windspeed10m100m_2000to2010_Lindenberg.nc"),
           Cabauw=c("./data/ERA20C_windspeed10m100m_2000to2010_Cabauw.nc"))
  } else if (res.switch == orig.res) {
    CallStop("No ERA20C orig res hourly data included!")
    era20c.fname = ""
  }
  CheckFile(c(era20c.fname$Fino1, era20c.fname$Fino2, era20c.fname$Lindenberg,
              era20c.fname$Cabauw))
}
# variable names to read from above files
era20c.param = c("windspeed_10m", "windspeed_100m")

#=== ERA-I ===
# filename to ERA-Interim reanalysis file
if (ana.time.res$time.res == ana.time.res$monthly) {
  if (res.switch == high.res) {
    eraI.fname =
      c("./data/10mWindSpeedDirection-2mTemp_ERAInterim_MonMean_highRes_1979to2014.nc",
      "./data/ERAInterim_100m_windspeed_MonMean_highRes.nc")
  } else if (res.switch == orig.res) {
    eraI.fname =
      C("./data/10mWindSpeedDirection-2mTemp_ERAInterim_MonMean_origRes_1979to2014.nc",
      "./data/ERAInterim_100m_windspeed_MonMean_origRes.nc")
  }
  CheckFile(eraI.fname)
} else if (ana.time.res$time.res == ana.time.res$daily) {
  if (res.switch == high.res) {
    eraI.fname =
      c("./data/10mWindSpeedDirection-2mTemp_ERAInterim_DayMean_highRes_1979to2014.nc",
        "./data/100mWindSpeed_ERAInterim_DayMean_highRes_200to2014.nc")
  } else if (res.switch == orig.res) {
    eraI.fname =
      "./data/10mWindSpeedDirection-2mTemp_ERAInterim_DayMean_origRes_1979to2014.nc"
  }
  CheckFile(eraI.fname)
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  if (res.switch == high.res) {
    eraI.fname =
      list(Fino1=c("./data/ERA-Interim_windspeed10m_2000to2015_Fino1.nc",
                   "./data/ERA-Interim_windspeed100m_19802014_highRes_Fino1.nc"),
           Fino2=c("./data/ERA-Interim_windspeed10m_2000to2015_Fino2.nc",
                   "./data/ERA-Interim_windspeed100m_19802014_highRes_Fino2.nc"),
           Lindenberg=c("./data/ERA-Interim_windspeed10m_2000to2015_Lindenberg.nc",
                       "./data/ERA-Interim_windspeed100m_19802014_highRes_Lindenberg.nc"),
           Cabauw=c("./data/ERA-Interim_windspeed10m_2000to2015_Cabauw.nc",
                    "./data/ERA-Interim_windspeed100m_19802014_highRes_Cabauw.nc"))
  } else if (res.switch == orig.res) {
    CallStop("No ERA-Interim orig res hourly data included!")
        eraI.fname = ""
  }
  CheckFile(c(eraI.fname$Fino1[[1]], eraI.fname$Fino1[[2]], eraI.fname$Fino2[[1]],
              eraI.fname$Fino2[[2]], eraI.fname$Lindenberg[[1]],
              eraI.fname$Lindenberg[[2]], eraI.fname$Cabauw[[1]],
              eraI.fname$Cabauw[[2]]))
}
# variable names to read from above files
eraI.param = c("windspeed_10m", "windspeed_100m")

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

# filename(s) of the COSMO HErZ reanalysis file(s)
if (ana.time.res$time.res == ana.time.res$monthly) {
  herz.fname = "./data/WindSpeed_HErZ_MonMean_1995to2014.nc"
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
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  herz.fname = Sys.glob(file.path("/data/mborsche/HErZ/HerzStats",
                                  paste0("*1995TO2014.nc")))
}
CheckFile(herz.fname)

# variable names to read from above files
if (herz.profile) {
  herz.param = c("windspeed_258m", "windspeed_178m", "windspeed_116m",
                 "windspeed_69m", "windspeed_35m", "windspeed_10m")
} else {
  herz.param = c("windspeed_116m", "windspeed_10m")
}

#=== lon lat idx file ===
# specify the file as one string of which the indices of the lon, lat locations of
# the tower sites are to be specified
ifile.lonlat.idx = herz.grid.nc
lonlat.idx.crea6 = T # specify whether to read COSMO-REA6 data or not (ERA-I, ERA20C)

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
era20c.tsstart = c(2000,1)
era20c.tsend = c(2010,12)
eraI.tsstart = c(2000,1)
eraI.tsend = c(2010,12)
herz.tsstart = c(2000,1)
herz.tsend = c(2010,12)
fname_ext = "2000to2010"

#=== plotting ===
# The following switches decide on which plots to generate

# time series between reanalyses and station data at 10m height
plot.EraStatComp = F
# time series between ERA20C and COSMO HErZ reanalyses at 100m height
plot.100mEraHerz = F
# only selected seasons of reanalyses and station data at 10m height
plot.EraStationSelSeasons = F
# only selected months of reanalyses and station data at 10m height
plot.EraStationSelMonths = F
# daily mean time series analysis of reanalyses and station data fo 10m wind speed
plot.EraStationSQ = F
# PDF score between station data and each reanalysis at 10m height
plot.PDFscore = F
# plot histograms of 10m and 100m Era and HErZ data, and HErZ profiles
plot.histograms = F
# plot extreme value analysis
plot.Extremes = F
