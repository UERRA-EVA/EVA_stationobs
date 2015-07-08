a4width = 29.7/2.54
a4height = 21./2.54

# this switch concerns the time period to analyse and depends on the availability
# of the reanalyses:
# * short.term: 2007-01 to 2010-12 (arbitrarily chosen short term analysis)
# * mid.term:   1998-01 to 2010-12 (availability of COSMO HErZ reanalysis)
# * long.term:  1979-01 to 2010-12 (availablity of ERA-Interim reanalysis to end of HErZ reanalysis)
# * all.term:   1901-01 to 2010-12 and 2014-12 (availability of ERA20C reanalysis to end of global and regional reanalysis, respectively)
time.switch = c("long.term")  # short.term, mid.term, long.term, all.term
# this switch concerns the resolution of the ERA20C and ERA-Interim reanalyses
# (HR is the version interpolated to 0.125°)
res.switch = c("HighRes")   # HighRes, OrigRes

#=== Paths ===
# path to store resulting plots into
outdir = "./output/"

#=== ERA20C ===
# filename to ERA20C reanalysis file
era20c.HRes.fname = "./data/10m100m_WindSpeedDirection_ERA20C_19012010_MonMean.nc"
era20c.ORes.fname = "./data/10m100m_WindSpeedDirection_ERA20C_1900to2010_MonMean.nc"
# variable names to read from above files
era20c.param = "windspeed_10m"
era20c100.param = "windspeed_100m"

#=== ERA-I ===
# filename to ERA-Interim reanalysis file
eraI.HRes.fname = "./data/ERAInterim_10mWindspeed_MonMean_highres.nc"
eraI.ORes.fname = "./data/ERAInterim_10mWindspeed_MonMean.nc"
# variable names to read from above files
eraI.param = "si10"

#=== HErZ ===
# filename to COSMO HErZ reanalysis file
herz.fname = "./data/WindSpeed_HErZ_MonMean_1997to2014.nc"
herz.grid = "./data/COSMO_REA6_CONST_withOUTsponge.grb"
# variable names to read from above files
herz10.param = "windspeed_10m"
herz116.param = "windspeed_116m"

#=== Station data ===
# station data based on daily (T) or hourly (F) measurements
daily = FALSE
# filename of station name list
station.daily.fname = "./data/KL_Tageswerte_Beschreibung_Stationen.txt"
station.hourly.fname = "./data/FF_Stundenwerte_Beschreibung_Stationen_wind_selected3.txt"

#=== plotting ===
# The following switches decided on which plots to generate

# monthly mean time series between reanalyses and station data at 10m height
plot.EraStatComp = F
# monthly mean time series between ERA20C and COSMO HErZ reanalyses at 100m height
plot.100mEraHerz = F
# only specific seasons of reanalyses and station data at 10m height - NOT YET FINISHED
plot.EraStationSeasons = F
# only specific months of reanalyses and station data at 10m height
plot.EraStationMonths = F
# PDF score between station data and each reanalysis at 10m height
plot.PDFscore = T
