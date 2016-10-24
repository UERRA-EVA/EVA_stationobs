# produce synthetic equalliy distributed data (T) or real data (F)
use.synthetic = F

if (ana.time.res$time.res == ana.time.res$monthly) {
  fino1.file = paste0("./data/FINO1_Windgeschwindigkeit_100m_20040101_20131231_MonMean.nc")
  fino2.file = paste0("./data/FINO2_Windgeschwindigkeit_102m_20070101_20131231_MonMean.nc")
  lind.file = paste0("./data/Lindenberg_Windgeschwindigkeit_20010101_20141231_MonMean.nc")
  cabauw.file = paste0("./data/Cabauw_20000401_20150731_MonMean.nc")
  hamburg.file = paste0("./data/Hamburg_19960101_20150930_MonMean.nc")
} else if (ana.time.res$time.res == ana.time.res$hourly) {
  fino1.file = paste0("./data/FINO1_Windgeschwindigkeit_100m_20040101_20131231_HourMean.nc")
  fino2.file = paste0("./data/FINO2_Windgeschwindigkeit_102m_20070101_20131231_HourMean.nc")
  lind.file = paste0("./data/Lindenberg_Windgeschwindigkeit_20010101_20141231_HourMean.nc")
  cabauw.file = paste0("./data/Cabauw_20000401_20150731_HourMean.nc")
  hamburg.file = paste0("./data/Hamburg_19960101_20150930_HourMean.nc")

  # HErZ values in different height at specific location
  herz.fname = list(Fino1=c("./data/WindSpeed_1995TO2014_HErZ-258m_at_Fino1.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-178m_at_Fino1.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-116m_at_Fino1.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-69m_at_Fino1.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-35m_at_Fino1.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-10m_at_Fino1.nc"),
                    Fino2=c("./data/WindSpeed_1995TO2014_HErZ-258m_at_Fino2.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-178m_at_Fino2.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-116m_at_Fino2.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-69m_at_Fino2.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-35m_at_Fino2.nc",
                            "./data/WindSpeed_1995TO2014_HErZ-10m_at_Fino2.nc"),
                    Lindenberg=c("./data/WindSpeed_1995TO2014_HErZ-258m_at_Lindenberg.nc",
                                 "./data/WindSpeed_1995TO2014_HErZ-178m_at_Lindenberg.nc",
                                 "./data/WindSpeed_1995TO2014_HErZ-116m_at_Lindenberg.nc",
                                 "./data/WindSpeed_1995TO2014_HErZ-69m_at_Lindenberg.nc",
                                 "./data/WindSpeed_1995TO2014_HErZ-35m_at_Lindenberg.nc",
                                 "./data/WindSpeed_1995TO2014_HErZ-10m_at_Lindenberg.nc"),
                    Cabauw=c("./data/WindSpeed_1995TO2014_HErZ-258m_at_Cabauw.nc",
                             "./data/WindSpeed_1995TO2014_HErZ-178m_at_Cabauw.nc",
                             "./data/WindSpeed_1995TO2014_HErZ-116m_at_Cabauw.nc",
                             "./data/WindSpeed_1995TO2014_HErZ-69m_at_Cabauw.nc",
                             "./data/WindSpeed_1995TO2014_HErZ-35m_at_Cabauw.nc",
                             "./data/WindSpeed_1995TO2014_HErZ-10m_at_Cabauw.nc"))
  herz.param = "windspeed"
} else if (ana.time.res$time.res == ana.time.res$daily) {
  fino1.file = paste0("./data/FINO1_Windgeschwindigkeit_100m_20040101_20131231_DayMean.nc")
  fino2.file = paste0("./data/FINO2_Windgeschwindigkeit_102m_20070101_20131231_DayMean.nc")
  lind.file = paste0("./data/Lindenberg_Windgeschwindigkeit_20010101_20141231_DayMean.nc")
  cabauw.file = paste0("./data/Cabauw_20000401_20150731_DayMean.nc")
  hamburg.file = paste0("./data/Hamburg_19960101_20150930_DayMean.nc")
  CheckFile(fino1.file)
  CheckFile(fino2.file)
  CheckFile(lind.file)
  CheckFile(cabauw.file)
  CheckFile(hamburg.file)
}
CheckFile(c(fino1.file, fino2.file, lind.file, cabauw.file, hamburg.file))

fino1.param = "windspeed_100m"
fino2.param = "windspeed_102m"
lind.param = c("windspeed_98m", "windspeed_80m", "windspeed_60m",
               "windspeed_40m", "windspeed_20m", "windspeed_10m")
cabauw.param = c("windspeed_200m", "windspeed_140m", "windspeed_80m",
                 "windspeed_40m", "windspeed_20m", "windspeed_10m")
hamburg.param = c("wind_speed_280m", "wind_speed_250m", "wind_speed_175m",
                  "wind_speed_110m", "wind_speed_50m", "wind_speed_10m")

# following www.fino1.de and www.fino2.de the location of the towers is:
fino1.lon =  6.58764
fino1.lat = 54.01486
fino2.lon = 13.15419
fino2.lat = 55.00693
lind.lon = 14.12222
lind.lat = 52.16653
cabauw.lon = 4.927
cabauw.lat = 51.971
hamburg.lon = 10.10286
hamburg.lat = 53.51917

#=== time period ===
# available data of the tower measurements, ERA20C, and HErZ
# the beginning of the ERA data will be set equal to the measurements as needed
#- era20c: 1900 to 2010
#- herz: 1995 to 2014
#- fino1: 2004-01-01 00:10 to 2013-12-31 23:50
#- fino2: 2007-08-01 00:05 to 2013-12-31 23:55
#- Lindenberg: 2001-01-01 00:00 to 2014-12-31 23:50
fino1.tsstart = c(2004,1)
fino1.tsend = c(2013,12)
fino2.tsstart = c(2007,8)
fino2.tsend = c(2013,12)
lind.tsstart = c(2001,1)
lind.tsend = c(2014,12)
cabauw.tsstart = c(2000,04)
cabauw.tsend = c(2015,07)

plot.TowerEraProfile = F
plot.histograms = F
plot.separate.hist = F
plot.ProfileTS = F
plot.Extremes = F
plot.DailyCycle = F
calc.threshold = F
get.cor.vals = T
print.lonlat.idx = F
