daily = FALSE # station data base (daily (T) or hourly (F))

a4width = 29.7/2.54
a4height = 21./2.54

# this switch concerns the period to analyse and depends on the availability of the
# reanalyses
time.switch = c("long.term")  # short.term, mid.term, long.term, all.term
# this switch concerns the resolution of the ERA20C and ERAI reanalyses (HR is the
# version interpolated to 0.125°)
res.switch = c("HighRes")   # HighRes, OrigRes

#=== Paths ===
outdir = "./output/"

#=== ERA20C ===
era20c.HRes.fname = paste0("./data/10m100m_WindSpeedDirection_ERA20C_19012010_MonMean.nc")
era20c.ORes.fname = paste0("./data/10m100m_WindSpeedDirection_ERA20C_1900to2010_MonMean.nc")
era20c.param = "windspeed_10m"
era20c100.param = "windspeed_100m"

#=== ERA-I ===
eraI.HRes.fname = "./data/ERAInterim_10mWindspeed_MonMean_highres.nc"
eraI.ORes.fname = "./data/ERAInterim_10mWindspeed_MonMean.nc"
eraI.param = "si10"

#=== HErZ ===
herz.fname = "./data/WindSpeed_HErZ_MonMean_19982014.nc"
herz.grid = "./data/COSMO_REA6_CONST_withOUTsponge"
herz10.param = "windspeed_10m"
herz116.param = "windspeed_116m"

#=== Station data ===
station.daily.fname = "./data/KL_Tageswerte_Beschreibung_Stationen.txt"
station.hourly.fname = "./data/FF_Stundenwerte_Beschreibung_Stationen_wind_selected3.txt"
