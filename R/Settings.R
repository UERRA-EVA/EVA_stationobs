daily = FALSE # station data base (daily (T) or hourly (F))
outdir = "~/work/programming/scripts_R/analyseWS/output/"
a4width = 29.7/2.54
a4height = 21./2.54
# this switch concerns the period to analyse and depends on the availability of the
# reanalyses
time.switch = c("long.term")  # short.term, mid.term, long.term, all.term
# this switch concerns the resolution of the ERA20C and ERAI reanalyses (HR is the
# version interpolated to 0.125°)
res.switch = c("HighRes")   # HighRes, OrigRes
era20c.HRes.fname = paste0("/data/mborsche/ERA20C/germany_12p5-interp/",
                           "10m100m_WindSpeedDirection_ERA20C_19012010_MonMean.nc")
era20c.ORes.fname = paste0("/data/mborsche/ERA20C/orig_res/",
                           "10m100m_WindSpeedDirection_ERA20C_1900to2010_MonMean.nc")
era20c.param = "windspeed_10m"
era20c100.param = "windspeed_100m"

eraI.HRes.fname = "/data/mborsche/ERAI/ERAInterim_10mWindspeed_MonMean_highres.nc"
eraI.ORes.fname = "/data/mborsche/ERAI/ERAInterim_10mWindspeed_MonMean.nc"
eraI.param = "si10"

herz.fname = "/data/mborsche/HErZ/ENERGY/WindSpeed_1998-2009_MonMean.nc"
herz10.param = "windspeed_10m"
herz116.param = "windspeed_116m"
herz.grid = "/data/mborsche/HErZ/COSMO_REA6_CONST_withOUTsponge"
