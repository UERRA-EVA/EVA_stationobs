TowerHour = F
if (era.monthly) {
  fino1.file = paste0("./data/FINO1_Windgeschwindigkeit_100m_20040101_20131231_MonMean.nc")
  fino2.file = paste0("./data/FINO2_Windgeschwindigkeit_102m_20070101_20131231_MonMean.nc")
  lind.file = paste0("./data/Lindenberg_Windgeschwindigkeit_20010101_20141231_MonMean.nc")
  cabauw.file = paste0("./data/Cabauw_20000401_20150731_MonMean.nc")
  hamburg.fiel = paste0("./data/Hamburg_19960101_20150930_MonMean.nc")
} else {
  if (TowerHour) {
    fino1.file = paste0("./data/FINO1_Windgeschwindigkeit_100m_20040101_20131231_HourMean.nc")
    fino2.file = paste0("./data/FINO2_Windgeschwindigkeit_102m_20070101_20131231_HourMean.nc")
    lind.file = paste0("./data/Lindenberg_Windgeschwindigkeit_20010101_20141231_HourMean.nc")
    cabauw.file = paste0("./data/Cabauw_20000401_20150731_HourMean.nc")
    hamburg.fiel = paste0("./data/Hamburg_19960101_20150930_HourMean.nc")
  } else {
    fino1.file = paste0("./data/FINO1_Windgeschwindigkeit_100m_20040101_20131231_DayMean.nc")
    fino2.file = paste0("./data/FINO2_Windgeschwindigkeit_102m_20070101_20131231_DayMean.nc")
    lind.file = paste0("./data/Lindenberg_Windgeschwindigkeit_20010101_20141231_DayMean.nc")
    cabauw.file = paste0("./data/Cabauw_20000401_20150731_DayMean.nc")
    hamburg.fiel = paste0("./data/Hamburg_19960101_20150930_DayMean.nc")
  }
}

fino1.param = "windspeed_100m"
fino2.param = "windspeed_102m"
lind.param = c("windspeed_10m", "windspeed_20m", "windspeed_40m",
               "windspeed_60m", "windspeed_80m", "windspeed_98m")
cabauw.param = c("windspeed_10m", "windspeed_20m", "windspeed_40m",
                 "windspeed_80m", "windspeed_140m", "windspeed_200m")
hamburg.param = c("wind_speed_10m", "wind_speed_50m", "wind_speed_110m",
                  "wind_speed_175m", "wind_speed_250m", "wind_speed_280m")

# following www.fino1.de and www.fino2.de the location of the towers is:
fino1.lon =  6.58764
fino1.lat = 54.01486
fino2.lon = 13.15419
fino2.lat = 55.00693
lind.lon = 14.12222
lind.lat = 52.16653
cabauw.lon = 4.927
cabauw.lat = 51.971

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
era20c.tsend = c(2010,12)
herz.tsend = c(2014,12)
cabauw.tsstart = c(2000,04)
cabauw.tsend = c(2015,07)

plot.TowerEraProfile = F
plot.histograms = F
plot.ProfileTS = F
plot.Extremes = T
