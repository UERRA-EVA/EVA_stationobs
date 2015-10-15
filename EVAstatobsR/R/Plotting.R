#' @title Determine y-axis limits from data.
#' @description \code{GetYlims} determinies the low and high y-axis limits from four
#'   different time series.
#'   This needs to be enhanced so that not all of those time series need to be
#'   available. Something like present= in FORTRAN.
#' @param xts1,xts2,xts3,xts4 extended time series from which to determine the low
#'   and high range of the y-axis limits.
#' @return Return a named list (yll=,ylh=) of the lower and high bound of the y-axis
#'   limits yliml and ylimh.
GetYlims <- function(xts1, xts2, xts3, xts4) {
  if (is.xts(xts1) & (is.xts(xts2)) & is.xts(xts3) & (is.xts(xts4))) {
    yliml = floor(min(min(xts1, na.rm=TRUE), min(xts2, na.rm=TRUE),
                      min(xts3, na.rm=TRUE), min(xts4, na.rm=TRUE)))
    ylimh = ceiling(max(max(xts1, na.rm=TRUE), max(xts2, na.rm=TRUE),
                        max(xts3, na.rm=TRUE), max(xts4, na.rm=TRUE)))
  } else {
    CallStop("XTS1 or XTS2 or XTS3 or XTS4 is not an xts, ABORTING!")
  }

  return(list(yll=yliml, ylh=ylimh))
}

#-----------------------------------------------------------------------------------

#' @title Plot station measurements together with ERA20C, ERA-I, and HErZ.
#' @description \code{PlotStationEra} plots the station values together with the
#'   corresponding ERA20C, ERA-I, and HErZ pixel and provides the correlation
#'   between these time series. Optionally, it is possible to plot the anomaly.
#'   The plot is saved in pdf format and there is no return value.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the file name of the plot
#' @param monthly is an optional parameter which determines to plot the monthly
#'   values of the above time series
#' @param anomaly is an optional parameter which determines whether to plot anomalies
PlotStationEra <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                           titname, outdir, fname, monthly=TRUE, anomaly=FALSE) {

  roll.mean = TRUE
  roll.time = 12

  pdf(paste(outdir, fname, sep=""), width=land.a4width, height=land.a4height,
      onefile=TRUE, pointsize=13)

  if (anomaly) {
    Era20cXts = Era20cXts - mean(Era20cXts)
    EraIXts = EraIXts - mean(EraIXts)
    HerzXts = HerzXts - mean(HerzXts)
    StatXts = StatXts - mean(StatXts)
  }
  Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  # ERA20C
  dummy = numeric(length=length(Era20cXts)) * NA
  dummy = xts(dummy, order.by = index(Era20cXts))
  plot(dummy, main=titname, ylab="wind speed [m/s]", ylim=c(yliml, ylimh))

  if (monthly) { lines(Era20cXts, type="b", pch=16, col="blue", lw=1.5) }
  if (!monthly & roll.mean) {
    lines(rollmean(Era20cXts, roll.time), type="p", pch=16, col="blue", lw=1.5)
    lines(rollmean(Era20cXts, roll.time), col="blue", lw=1.5)
  }

  # ERA-I
  if (monthly) { lines(EraIXts, type="b", pch=16, col="red", lw=1.5) }
  if (!monthly & roll.mean) {
    lines(rollmean(EraIXts, roll.time), type="p", pch=16, col="red", lw=1.5)
    lines(rollmean(EraIXts, roll.time), col="red", lw=1.5)
  }

  # HErZ
  if (monthly) { lines(HerzXts, type="b", pch=16, col="green3", lw=1.5) }
  if (!monthly & roll.mean) {
    lines(rollmean(HerzXts, roll.time), type="p", pch=16, col="green3", lw=1.5)
    lines(rollmean(HerzXts, roll.time), col="green3", lw=1.5)
  }

  # Station
  if (monthly) { lines(StatXts, type="b", pch=16, col="black", lw=1.5) }
  if (!monthly & roll.mean) {
    lines(rollmean(StatXts, roll.time), type="p", pch=16, col="black", lw=1.5)
    lines(rollmean(StatXts, roll.time), col="black", lw=1.5)
  }

  Corr.vals = GetCorrXts(era20c=Era20cXts, eraI=EraIXts, herz=HerzXts, stat=StatXts)

  legend("topleft", legend=c(paste0("Corr(ERA20C, Stat) = ",
                                    round(Corr.vals$c.20c.S, 2)),
                             paste0("Corr(ERAI, Stat) = ",
                                    round(Corr.vals$c.I.S, 2)),
                             paste0("Corr(HErZ, Stat) = ",
                                    round(Corr.vals$c.H.S, 2)),
                             paste0("Corr(ERA20C, ERAI)= ",
                                    round(Corr.vals$c.20c.I, 2)),
                             paste0("Corr(ERA20C, HErZ)= ",
                                    round(Corr.vals$c.20c.H, 2)),
                             paste0("Corr(ERAI, HErZ)= ",
                                    round(Corr.vals$c.I.H, 2))),
         text.col=c("blue", "red", "green", "black", "black", "black"))
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot a 2-by-2 multi panel plot.
#' @description This is supposed to be a rather generic routine to plot a 2-by-2
#'   multi panel plot. So far, it is used to plot the four different data sources
#'   (ERA20C, ERA-Interim, HErZ, station data) for monthly and seasonal values. The
#'   plot is saved into a pdf file, and the function does not have a return value.
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the file name of the plot
#' @param titname string of the plot title name
#' @param Era20c extended time series of monthly or seasonal ERA20C data
#' @param EraI extended time series of momthly or seasonal ERA-Interim data
#' @param Herz extended time series of monthly or seasonal HErZ data
#' @param Stat extended time series of monthly or seasonal station data
#' @param length.plot a list holding integers for the months or seasons to plot.
#'   It holds 1, .., 12 specifying the months, or 1,..,4 specifying the seasons.
#' @param era.months boolean which specifies whether monthly or seasonal data
#'   shall be plotted.
#' @param plot.diff optional boolean which determines whether to plot the difference
#'   of the reanalyses to the station data or the absolute values of each time
#'   series. When plotting the difference, only three panels will be plotted. The
#'   default value is to plot the absolute values of each time series
#'   (plot.diff=FALSE).
PlotMultiPanel <- function(outdir, fname, titname, Era20c, EraI, Herz, Stat,
                           length.plot, era.months, plot.diff=FALSE) {

  if (era.months) { # months will be plotted
    all.months = c("January","February","March","April","May","June","July",
                   "August","September","October","November","December")
  } else { # seasons will be plotted
    all.seasons = c("Winter", "Spring", "Summer", "Autumn")
  }

  pdf(paste(outdir, fname, sep=""), width=land.a4width, height=land.a4height,
      onefile=TRUE, pointsize=13)

  yliml = vector(mode="numeric", length=length(length.plot))
  ylimh = vector(mode="numeric", length=length(length.plot))
  for (cnt in seq(length.plot)) {
    Ylims = GetYlims(Era20c[[length.plot[[cnt]]]], EraI[[length.plot[[cnt]]]],
                     Herz[[length.plot[[cnt]]]], Stat[[length.plot[[cnt]]]])
    yliml[cnt] = Ylims$yll
    ylimh[cnt] = Ylims$ylh
  }
  yliml = min(yliml)
  ylimh = max(ylimh)

  dummy = numeric(length=length(Era20c[[1]])) * NA
  dummy = xts(dummy, order.by = index(Era20c[[1]]))

  par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(3,5,3,0.5))
  color = c("red", "black")

  plot(dummy, main=NULL, axes=FALSE, ylim=c(yliml, ylimh))
  axis(2)
  for (cnt in seq(length.plot)) {
    lines(Era20c[[length.plot[[cnt]]]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  if (era.months) {
    legend("topleft", legend=c(paste0("ERA20C ", all.months[length.plot[[1]]]),
                               paste0("ERA20C ", all.months[length.plot[[2]]])),
           text.col=color)
  } else {
    legend("topleft", legend=c(paste0("ERA20C ", all.seasons[length.plot[[1]]]),
                               paste0("ERA20C ", all.seasons[length.plot[[2]]])),
           text.col=color)
  }

  plot(dummy, main=NULL, axes=FALSE, ylim=c(yliml, ylimh))
  for (cnt in seq(length.plot)) {
    lines(EraI[[length.plot[[cnt]]]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  if (era.months) {
    legend("topleft", legend=c(paste0("ERA-I ", all.months[length.plot[[1]]]),
                               paste0("ERA-I ", all.months[length.plot[[2]]])),
           text.col=color)
  } else {
    legend("topleft", legend=c(paste0("ERA-I ", all.seasons[length.plot[[1]]]),
                               paste0("ERA-I ", all.seasons[length.plot[[2]]])),
           text.col=color)
  }

  plot(dummy, main=NULL, ylim=c(yliml, ylimh))
  for (cnt in seq(length.plot)) {
    lines(Herz[[length.plot[[cnt]]]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  if (era.months) {
    legend("topleft", legend=c(paste0("HErZ ", all.months[length.plot[[1]]]),
                               paste0("HErZ ", all.months[length.plot[[2]]])),
           text.col=color)
  } else {
    legend("topleft", legend=c(paste0("HErZ ", all.seasons[length.plot[[1]]]),
                               paste0("HErZ ", all.seasons[length.plot[[2]]])),
           text.col=color)
  }

  if (!plot.diff) {
    plot(dummy, main=NULL, yaxt="n", ylim=c(yliml, ylimh))
    for (cnt in seq(length.plot)) {
      lines(Stat[[length.plot[[cnt]]]], type="b", pch=21, col=color[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    if (era.months) {
      legend("topleft", legend=c(paste0("Station ", all.months[length.plot[[1]]]),
                                 paste0("Station ", all.months[length.plot[[2]]])),
             text.col=color)
    } else {
      legend("topleft", legend=c(paste0("Station ", all.seasons[length.plot[[1]]]),
                                 paste0("Station ", all.seasons[length.plot[[2]]])),
             text.col=color)
    }
  }
  mtext(titname, outer=TRUE)
  mtext("wind speed [m/s]", side=2, line=3, outer=TRUE)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Prepare and plot seasonal time series of station data and ERA20C, ERA-I,
#'   and HErZ data.
#' @description \code{PlotStationEraSelSeasons} prepares extended time series of
#'   seasonal means, and optionally their anomalies, of station data and locally
#'   corresponding global and regional reanalyses. The seasons for which data shall
#'   be prepared and plotted need to be set hard-coded within this function. Of
#'   course, this setting can be put into the Settings.R file if necessary.
#'   A generic plotting routine is called which actually performs the plotting.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the file name of the plot
#' @param seasons is an optional parameter which determines whetther to plot the
#'   monthly (F) or seasonal (T) values of the above time series
#' @param anomaly is an optional parameter which determines whether to plot anomalies
PlotStationEraSelSeasons <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                     titname, outdir, fname,
                                     anomaly=FALSE, seasons=FALSE) {

  # specify seasons to plot with 1 for winter, 2 spring, 3 summer, and 4 for autumn
  # within the list below; the list must exactly hold two entries!
  seasons = list(1,3)

  seas.Era20c = list()
  seas.EraI = list()
  seas.Herz = list()
  seas.Stat = list()

  era20c.seasons = GetSeasonalXts(Era20cXts)
  seas.Era20c[[1]] = era20c.seasons$winter.xts
  seas.Era20c[[2]] = era20c.seasons$spring.xts
  seas.Era20c[[3]] = era20c.seasons$summer.xts
  seas.Era20c[[4]] = era20c.seasons$autumn.xts

  eraI.seasons = GetSeasonalXts(EraIXts)
  seas.EraI[[1]] = eraI.seasons$winter.xts
  seas.EraI[[2]] = eraI.seasons$spring.xts
  seas.EraI[[3]] = eraI.seasons$summer.xts
  seas.EraI[[4]] = eraI.seasons$autumn.xts

  herz.seasons = GetSeasonalXts(HerzXts)
  seas.Herz[[1]] = herz.seasons$winter.xts
  seas.Herz[[2]] = herz.seasons$spring.xts
  seas.Herz[[3]] = herz.seasons$summer.xts
  seas.Herz[[4]] = herz.seasons$autumn.xts

  stat.seasons = GetSeasonalXts(StatXts)
  seas.Stat[[1]] = stat.seasons$winter.xts
  seas.Stat[[2]] = stat.seasons$spring.xts
  seas.Stat[[3]] = stat.seasons$summer.xts
  seas.Stat[[4]] = stat.seasons$autumn.xts

  if (anomaly) {
    for (cnt in seq(4)) {
      seas.Era20c[[cnt]] = seas.Era20c[[cnt]] - mean(seas.Era20c[[cnt]])
      seas.EraI[[cnt]] = seas.EraI[[cnt]] - mean(seas.EraI[[cnt]])
      seas.Herz[[cnt]] = seas.Herz[[cnt]] - mean(seas.Herz[[cnt]])
      seas.Stat[[cnt]] = seas.Stat[[cnt]] - mean(seas.Stat[[cnt]])
    }
  }

  PlotMultiPanel(outdir, fname, titname,
                 seas.Era20c, seas.EraI, seas.Herz, seas.Stat,
                 seasons, era.months=FALSE)

  station.diff = TRUE
  if (station.diff & !anomaly) {
    for (cnt in seq(4)) {
      seas.Era20c[[cnt]] = seas.Era20c[[cnt]] - seas.Stat[[cnt]]
      seas.EraI[[cnt]] = seas.EraI[[cnt]] - seas.Stat[[cnt]]
      seas.Herz[[cnt]] = seas.Herz[[cnt]] - seas.Stat[[cnt]]
      seas.Stat[[cnt]] = seas.Stat[[cnt]] - seas.Stat[[cnt]]
    }
    fname = gsub(".pdf", "_diff.pdf", fname)
    titname = gsub("Seasonal", "Seasonal difference in", titname)
    PlotMultiPanel(outdir, fname, titname,
                   seas.Era20c, seas.EraI, seas.Herz, seas.Stat,
                   seasons, era.months=FALSE, station.diff)
  }
}

#-----------------------------------------------------------------------------------

#' @title Prepare and plot specifically selected months of station data and ERA20C,
#'   ERA-I, and HErZ data.
#' @description \code{PlotStationEraSelMonths} prepares extended time series of
#'   station data, ERA20C, ERA-I, and HErZ data - for specific months only. These
#'   months are set hard-coded within this function. Of course, this setting can be
#'   put into the Settings.R file if necessary.
#'   Optionally, it is possible to prepare the anomalies. A generic plotting routine
#'   is called which actually performs the plotting.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the output file name
#' @param anomaly is an optional parameter which determines whether to plot anomalies
#' @note need to adopt titname to months; need to plot into four different panals
PlotStationEraSelMonths <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                    titname, outdir, fname, anomaly=FALSE) {

  # specify months to plot starting from 1 for January to 12 for December
  # within the list below; the list must exactly hold two entries!
  months = list(1,8)

  mon.Era20c = list()
  mon.EraI = list()
  mon.Herz = list()
  mon.Stat = list()

  date.Era20c <- as.POSIXlt(index(Era20cXts))
  date.EraI <- as.POSIXlt(index(EraIXts))
  date.Herz <- as.POSIXlt(index(HerzXts))
  date.Stat <- as.POSIXlt(index(StatXts))

  for (cnt in seq(12)) {
    mon.Era20c[[cnt]] = Era20cXts[which( date.Era20c$mon==cnt )]
    mon.EraI[[cnt]] = EraIXts[which( date.EraI$mon==cnt )]
    mon.Herz[[cnt]] = HerzXts[which( date.Herz$mon==cnt )]
    mon.Stat[[cnt]] = StatXts[which( date.Stat$mon==cnt )]

    if (anomaly) {
      mon.Era20c[[cnt]] = mon.Era20c[[cnt]] - mean(mon.Era20c[[cnt]])
      mon.EraI[[cnt]] = mon.EraI[[cnt]] - mean(mon.EraI[[cnt]])
      mon.Herz[[cnt]] = mon.Herz[[cnt]] - mean(mon.Herz[[cnt]])
      mon.Stat[[cnt]] = mon.Stat[[cnt]] - mean(mon.Stat[[cnt]])
    }
  }

  PlotMultiPanel(outdir, fname, titname,
                 mon.Era20c, mon.EraI, mon.Herz, mon.Stat,
                 months, era.months=TRUE)

  station.diff = TRUE
  if (station.diff & !anomaly) {
    for (cnt in seq(12)) {
      mon.Era20c[[cnt]] = mon.Era20c[[cnt]] - mon.Stat[[cnt]]
      mon.EraI[[cnt]] = mon.EraI[[cnt]] - mon.Stat[[cnt]]
      mon.Herz[[cnt]] = mon.Herz[[cnt]] - mon.Stat[[cnt]]
      mon.Stat[[cnt]] = mon.Stat[[cnt]] - mon.Stat[[cnt]]
    }
    fname = gsub(".pdf", "_diff.pdf", fname)
    titname = gsub("wind speed", "Monthly difference in wind speed", titname)
    PlotMultiPanel(outdir, fname, titname,
                   mon.Era20c, mon.EraI, mon.Herz, mon.Stat,
                   months, era.months=TRUE, station.diff)
  }
}

#-----------------------------------------------------------------------------------

#' @title Prepare and plot time series of station data and ERA20C, ERA-I,
#'   and HErZ data.
#' @description This function plots station and reanalysis data into scatter plots
#'   and Quantile-quantile plots (QQ-plot).
#' @param Era20cXts extended time series of the ERA20C pixel corresponding to the
#'   station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot/s is/are saved
#' @param fname string of the output file name
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
PlotStationEraSQ <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                             titname, outdir, fname, era.monthly=FALSE) {

  Era20  = as.numeric(Era20cXts)
  EraI = as.numeric(EraIXts)
  Herz = as.numeric(HerzXts)
  Stat = as.numeric(StatXts)

  Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  axis.n = 'n'
  axis.y = 's'
  mtext.titname = "Daily wind speed at 10m height"
  if (era.monthly) {
    mtext.titname = "Monthly wind speed at 10m height"
  }

  fname.scatter = gsub(".pdf", "_scatterQQ-Plots.pdf", fname)
  pdf(paste0(outdir, fname.scatter), width=port.a4width, height=port.a4height,
      onefile=TRUE, pointsize=13)

  par(mfrow=c(3,2), mar=c(0,0,0,0), oma=c(5,5,4,0.5))

  xlabname = "10m ERA20C wind speed [m/s]"
  ylabname = "10m ERA-I wind speed [m/s]"
  if (era.monthly) {
    titname.scatter = gsub("wind speed", "Scatter and QQ-plot of monthly wind speed",
                           titname)
  } else {
    titname.scatter = gsub("wind speed", "Scatter and QQ-plot of daily wind speed",
                           titname)
  }
  text.str = "Era20c vs ERA-Interim"
  scatterPlot(Era20, EraI, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(Era20, EraI, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m ERA20C wind speed [m/s]"
  ylabname = "10m HErZ wind speed [m/s]"
  text.str = "Era20c vs HErZ"
  scatterPlot(Era20, Herz, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(Era20, Herz, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m ERA-I wind speed [m/s]"
  ylabname = "10m HErZ wind speed [m/s]"
  text.str = "ERA-Interim vs HErZ"
  scatterPlot(EraI, Herz, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.y)
  qqPlot(EraI, Herz, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.n)

  mtext(titname.scatter, line=1, outer=TRUE)
  mtext("wind speed [m/s]", side=2, line=3, outer=TRUE)
  mtext("wind speed [m/s]", side=1, line=3, outer=TRUE)

  xlabname = "10m ERA20C wind speed [m/s]"
  ylabname = "10m Station wind speed [m/s]"
  text.str = "ERA20C vs station data"
  scatterPlot(Era20, Stat, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(Era20, Stat, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m ERA-I wind speed [m/s]"
  ylabname = "10m Station wind speed [m/s]"
  text.str = "ERA-Interim vs station data"
  scatterPlot(EraI, Stat, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(EraI, Stat, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m HErZ wind speed [m/s]"
  ylabname = "10m Station wind speed [m/s]"
  text.str = "HErZ vs station data"
  scatterPlot(Herz, Stat, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.y)
  qqPlot(Herz, Stat, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.n)

  mtext(titname.scatter, line=1, outer=TRUE)
  mtext("wind speed [m/s]", side=2, line=3, outer=TRUE)
  mtext("wind speed [m/s]", side=1, line=3, outer=TRUE)

  dev.off()

}

#-----------------------------------------------------------------------------------

#' @title Compare 100m wind speed of ERA20C and HErZ pixel by pixel.
#' @description \code{Plot100mEraHerz} compares the 100m wind speed of the ERA20C
#'   global reanalysis with the 116m wind speed of the HErZ regional reanalysis.
#'   This function performs a pixel wise comparison at the station locationto
#'   provided by the package. Scatter plots, QQplots, histogram plots, and the
#'   PDFscore are produced.
#' @param Era20cXts extended time series of an ERA20C pixel
#' @param HerzXts same as above for HErZ
#' @param titname string of the plot title name
#' @param statname string of the station name whose pixel is plotted
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the file name of the plot
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
Plot100mEraHerz <- function(Era20cXts, HerzXts,
                            titname, statname, outdir, fname,
                            era.monthly) {

  same.length = F
  if (length(Era20cXts) == length(HerzXts)) {same.length = T}

  axis.n = 'n'
  axis.y = 's'
  if (era.monthly) {
    titname = paste0("Monthly ", titname)
    titname.ext = "monthly"
  } else {
    titname = paste0("Daily ", titname)
    titname.ext = "daily"
  }

  pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height,
      onefile=TRUE, pointsize=13)

  dummy = numeric(length=length(Era20cXts)) * NA
  dummy = xts(dummy, order.by = index(Era20cXts))

  Ylims = GetYlims(Era20cXts, HerzXts, dummy, dummy)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  plot(dummy, main=titname, ylab="wind speed [m/s]", ylim=c(yliml, ylimh))

  # ERA20C
  lines(Era20cXts, type="b", pch=16, col="blue", lw=1.5)

  # HErZ
  lines(HerzXts, type="b", pch=16, col="green3", lw=1.5)

  if (same.length) {
    Corr.vals = GetCorrXts(era20c=Era20cXts, herz=HerzXts, eraI=dummy, stat=dummy)

    legend("topleft", legend=c(paste0("Corr(ERA20C, HErZ)= ",
                                      round(Corr.vals$c.20c.H, 2)),
                               'ERA20C', 'HErZ'),
           text.col=c("black", "blue", "green3"))
  }

  if (same.length) {
    Herz = as.numeric(HerzXts)
    Era = as.numeric(Era20cXts)

    xlabname = "100m ERA20C wind speed [m/s]"
    ylabname = "116m HErZ wind speed [m/s]"
    text.str = ""

    scatterPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname,
                text.str=text.str)

    titname = "Quantile-quantile plot"
    qqPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname, text.str=text.str)

    min.val = floor(min(min(Era, na.rm=TRUE), min(Herz, na.rm=TRUE)))
    max.val = ceiling(max(max(Era, na.rm=TRUE), max(Herz, na.rm=TRUE)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(Era)) * NA

    titname = paste0("Frequency distribution of ", titname.ext, " wind speed\nof ",
                     "ERA20C at 100m")
    ylabname = "Density"
    histoPlot(Era, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = paste0("Frequency distribution of ", titname.ext, " wind speed\nof ",
                     "COSMO HErZ at 116m")
    xlabname = ylabname
    histoPlot(Herz, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = paste0("Frequency distribution of ", titname.ext,
                     " ERA20C wind speed at 100m\n",
                     "in green and COSMO HErZ at 116m shaded")
    xlabname = "wind speed [m/s]"
    histoPlot(Era, Herz, breaks, xlims=c(min.val, max.val),
              titname, xlabname, ylabname, addPlot=T)

  }
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot the wind speed histograms of station data and reanalyses at different
#'   heights.
#' @description The distributions of monthly and daily wind speed station data and
#'   reanalysis data, optionally at differnet heights are plotted into histograms.
#' @param outdir is a string containing the output path of the plot
#' @param fname is a string of the file name of the plot file
#' @param station.name is a string holding the station name at which location the
#'   distribution data was extraced
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
#' @param Era20cXts10,Era20cXts100,EraIXts extended time series of ERA20C and ERA-I
#'   10m and 100m height
#' @param HerzXts10,HerzXts35,HerzXts69,HerzXts116,HerzXts178,HerzXts258 extended
#'   time series of available HErZ data at different heights
#' @param StatXts extended time series of station data
#' @param plot.10m,plot.10m100m,plot.HerzProfile optional parameters setting whether
#'   to plot only 10m values, 10m and 100m values together, or the herz profile,
#'   respectively. The default value is FALSE for all three settings.
PlotHistograms <- function(outdir, fname, station.name, era.monthly,
                           Era20cXts10=NULL, Era20cXts100=NULL, EraIXts=NULL,
                           HerzXts10=NULL, HerzXts35=NULL, HerzXts69=NULL,
                           HerzXts116=NULL, HerzXts178=NULL, HerzXts258=NULL,
                           StatXts=NULL, plot.10m=FALSE, plot.10m100m=FALSE,
                           plot.HerzProfile=FALSE) {

  if (is.null(Era20cXts10) & is.null(Era20cXts100)
      & is.null(EraIXts) & is.null(HerzXts10)
      & is.null(HerzXts35) & is.null(HerzXts69)
      & is.null(HerzXts116) & is.null(HerzXts178)
      & is.null(HerzXts258) & is.null(StatXts)) {
    CallStop("All passed data are NULL!")
  }

  plot.cnt = 0
  if (!is.null(Era20cXts10)) plot.cnt = plot.cnt + 1
  if (!is.null(Era20cXts100)) plot.cnt = plot.cnt + 1
  if (!is.null(EraIXts)) plot.cnt = plot.cnt + 1
  if (!is.null(HerzXts10)) plot.cnt = plot.cnt + 1
  if (!is.null(HerzXts35)) plot.cnt = plot.cnt + 1
  if (!is.null(HerzXts69)) plot.cnt = plot.cnt + 1
  if (!is.null(HerzXts116)) plot.cnt = plot.cnt + 1
  if (!is.null(HerzXts178)) plot.cnt = plot.cnt + 1
  if (!is.null(HerzXts258)) plot.cnt = plot.cnt + 1
  if (!is.null(StatXts)) plot.cnt = plot.cnt + 1

  if(is.null(HerzXts258)) {
    Ylims = GetYlims(EraIXts, HerzXts10, Era20cXts100, StatXts)
  } else {
    Ylims = GetYlims(EraIXts, HerzXts10, HerzXts258, StatXts)
  }
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  era20c10  = as.numeric(Era20cXts10)
  era20c100  = as.numeric(Era20cXts100)
  eraI = as.numeric(EraIXts)
  herz10 = as.numeric(HerzXts10)
  herz35 = as.numeric(HerzXts35)
  herz69 = as.numeric(HerzXts69)
  herz116 = as.numeric(HerzXts116)
  herz178 = as.numeric(HerzXts178)
  herz258 = as.numeric(HerzXts258)
  stat = as.numeric(StatXts)

  axis.n = 'n'
  axis.y = 's'
  if (era.monthly) monthly.ext = 'monthly'
  if (!era.monthly) monthly.ext = 'daily'

  xlabname.empty = ""
  xlabname.full = "wind speed [m/s]"
  ylabname = "Density"

  if (plot.10m) {
    if (plot.cnt != 4 & plot.cnt != 6 & plot.cnt != 10) {
      CallStop(paste0("Depending on data to plot I expect 4, 6, or 10 plots to be ",
                      "plotted;\n", "   plot.cnt = ", plot.cnt, " for plot.10m: ",
                      plot.10m, ", plot.10m100m: ", plot.10m100m,
                      ", and plot.HerzProfile: ", plot.HerzProfile))
    }

    mtext.titname = paste0("Daily wind speed at 10m height at ", station.name)
    if (era.monthly) {
      mtext.titname = paste0("Monthly wind speed at 10m height at ", station.name)
    }

    fname = gsub('Histogram', 'Histogram_ERA-Station-10m', fname)
    pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height,
        onefile=TRUE, pointsize=13)

    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    min.val = floor(min(min(era20c10, na.rm=TRUE), min(eraI, na.rm=TRUE),
                        min(herz10, na.rm=TRUE), min(stat, na.rm=TRUE)))
    max.val = ceiling(max(max(era20c10, na.rm=TRUE), max(eraI, na.rm=TRUE),
                          max(herz10, na.rm=TRUE), max(stat, na.rm=TRUE)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(era20c10)) * NA

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA20C")
    histoPlot(era20c10, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.empty, ylabname)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA-I")
    histoPlot(eraI, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.empty, ylabname)

    titname = paste0("Frequency distribution of ", monthly.ext, " HErZ")
    histoPlot(herz10, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname)

    titname = paste0("Frequency distribution of ", monthly.ext, " station data")
    histoPlot(stat, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA20C\n",
                     "in green and ERA-Interim shaded")
    histoPlot(era20c10, eraI, breaks, xlims=c(min.val, max.val), titname,
              xlabname.empty, ylabname, xaxis=axis.n, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA20C\n",
                     "in green and COSMO HErZ shaded")
    histoPlot(era20c10, herz10, breaks, xlims=c(min.val, max.val), titname,
              xlabname.empty, ylabname, xaxis=axis.n, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA20C\n",
                     "in green and station data shaded")
    xlabname = "10m station wind speed [m/s]"
    histoPlot(era20c10, stat, breaks, xlims=c(min.val, max.val), titname,
              xlabname.full, ylabname, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA-Interim\n",
                     "in green and COSMO HErZ shaded")
    histoPlot(eraI, herz10, breaks, xlims=c(min.val, max.val), titname,
              xlabname.full, ylabname, addPlot=TRUE)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA-Interim\n",
                     "in green and station data shaded")
    histoPlot(eraI, stat, breaks, xlims=c(min.val, max.val), titname,
              xlabname.full, ylabname, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " COSMO HErZ\n",
                     "in green and station data shaded")
    histoPlot(herz10, stat, breaks, xlims=c(min.val, max.val), titname,
              xlabname.full, ylabname, addPlot=TRUE)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    dev.off()

  }

  if (plot.10m100m) {

    if (plot.cnt != 4 & plot.cnt != 6 & plot.cnt != 10) {
      CallStop(paste0("Depending on data to plot I expect 4, 6, or 10 plots to be ",
                      "plotted;\n", "   plot.cnt = ", plot.cnt, " for plot.10m: ",
                      plot.10m, ", plot.10m100m: ", plot.10m100m,
                      ", and plot.HerzProfile: ", plot.HerzProfile))
    }

    mtext.titname = paste0("Daily wind speed at 10m height at ", station.name)
    if (era.monthly) {
      mtext.titname = paste0("Monthly wind speed at 10m height at ", station.name)
    }

    fname = gsub('Histogram', 'Histogram_ERA20C-HErZ-100m', fname)
    pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height,
        onefile=TRUE, pointsize=13)

    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    min.val = floor(min(min(era20c100, na.rm=TRUE), min(herz116, na.rm=TRUE)))
    max.val = ceiling(max(max(era20c100, na.rm=TRUE), max(herz116, na.rm=TRUE)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(era20c100)) * NA

    titname = paste0("Frequency distribution of 100m ", monthly.ext,
                     " ERA20C wind speed")
    xlabname = "100m ERA20c wind speed [m/s]"
    histoPlot(era20c100, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 116m COSMO HErZ wind speed")
    xlabname = "116m HErZ wind speed [m/s]"
    histoPlot(herz116, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA20C wind speed",
                     " at 100m\n", "in green and COSMO HErZ at 116m shaded")
    xlabname = "wind speed [m/s]"
    histoPlot(era20c100, herz116, breaks, xlims=c(min.val, max.val),
              titname, xlabname, ylabname, addPlot=T)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    dev.off()

  }

  if (plot.HerzProfile) {

    if (plot.cnt != 4 & plot.cnt != 6 & plot.cnt != 10) {
      CallStop(paste0("Depending on data to plot I expect 4, 6, or 10 plots to be ",
                      "plotted;\n", "   plot.cnt = ", plot.cnt, " for plot.10m: ",
                      plot.10m, ", plot.10m100m: ", plot.10m100m,
                      ", and plot.HerzProfile: ", plot.HerzProfile))
    }

    mtext.titname = paste0("Daily wind speed of HErZ profile at ", station.name)
    if (era.monthly) {
      mtext.titname = paste0("Monthly wind speed of HErZ profile at ", station.name)
    }

    fname = gsub('Histogram', 'Histogram_HErZ-Profile', fname)
    pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height,
        onefile=TRUE, pointsize=13)

    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    min.val = floor(min(min(herz10, na.rm=TRUE), min(herz35, na.rm=TRUE),
                        min(herz69, na.rm=TRUE), min(herz116, na.rm=TRUE),
                        min(herz178, na.rm=TRUE), min(herz258, na.rm=TRUE)))
    max.val = ceiling(max(max(herz10, na.rm=TRUE), max(herz35, na.rm=TRUE),
                          max(herz69, na.rm=TRUE), max(herz116, na.rm=TRUE),
                          max(herz178, na.rm=TRUE), max(herz258, na.rm=TRUE)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(herz10)) * NA

    titname = paste0("Frequency distribution of 10m ", monthly.ext, " HErZ wind speed")
    xlabname = "10m HErZ wind speed [m/s]"
    histoPlot(herz10, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 35m ", monthly.ext, " HErZ wind speed")
    xlabname = "35m HErZ wind speed [m/s]"
    histoPlot(herz35, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 69m ", monthly.ext, " HErZ wind speed")
    xlabname = "69m HErZ wind speed [m/s]"
    histoPlot(herz69, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 116m ", monthly.ext, " HErZ wind speed")
    xlabname = "116m HErZ wind speed [m/s]"
    histoPlot(herz116, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    titname = paste0("Frequency distribution of 178m ", monthly.ext, " HErZ wind speed")
    xlabname = "178m HErZ wind speed [m/s]"
    histoPlot(herz178, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 258m ", monthly.ext, " HErZ wind speed")
    xlabname = "258m HErZ wind speed [m/s]"
    histoPlot(herz258, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    dev.off()

  }
}

#-----------------------------------------------------------------------------------

#' @title Plot histrograms for tower measurements.
#' @description Plot histograms for the Lindenberg, Cabauw, Fino1 and Fino2 tower
#'   tower measurements. Histograms are plotted for 10m and 100m heights; seperately
#'   and in comparison with reanalysis data.
#' @param outdir is a string containing the output path of the plot
#' @param fname is a string of the file name of the plot file
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
PlotHistogramsTower <- function(outdir, fname, era.monthly, tower.obj) {

  t.obj = tower.obj$climate_data_objects
  tower.date <- as.POSIXlt(t.obj$tower$data$date)

  Ylims = GetYlims(xts(t.obj$era20c100$data$wind_speed, order.by=tower.date),
                   xts(t.obj$herz116$data$wind_speed, order.by=tower.date),
                   xts(t.obj$herz10$data$wind_speed, order.by=tower.date),
                   xts(t.obj$herz10$data$wind_speed, order.by=tower.date))
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  axis.n = 'n'
  axis.y = 's'
  if (era.monthly) monthly.ext = 'Monthly'
  if (!era.monthly) monthly.ext = 'Daily'

  xlabname.empty = ""
  xlabname.full = "wind speed [m/s]"
  ylabname.full = "Density"
  ylabname.empty = ""

  if (t.obj$tower$data$StationName[1] == "Lindenberg" |
      t.obj$tower$data$StationName[1] == "Cabauw") {

    data.10.vals = t.obj$tower$data$wind_speed
    if (t.obj$tower$data$StationName[1] == "Lindenberg") {
      tit.100.ext = "Lindenberg at 98m"
      tit.10.ext = "Lindenberg at 10m"
      data.100.vals = t.obj$tower6$data$wind_speed
      min.t.val = min(t.obj$tower$data$wind_speed, na.rm=TRUE)
      max.t.val = max(t.obj$tower6$data$wind_speed, na.rm=TRUE)
    } else {
      tit.100.ext = "Cabauw at 80m"
      tit2.100.ext = "Cabauw at 140m"
      tit.10.ext = "Cabauw at 10m"
      data.100.vals = t.obj$tower4$data$wind_speed
      data2.100.vals = t.obj$tower5$data$wind_speed
      min.t.val = min(t.obj$tower4$data$wind_speed, na.rm=TRUE)
      max.t.val = max(t.obj$tower5$data$wind_speed, na.rm=TRUE)
    }
    min.val = floor(min(min(t.obj$era20c10$data$wind_speed, na.rm=TRUE),
                        min(t.obj$herz10$data$wind_speed, na.rm=TRUE),
                        min(data.10.vals, na.rm=TRUE)))
    max.val = ceiling(max(max(t.obj$era20c100$data$wind_speed, na.rm=TRUE),
                          max(data.100.vals, na.rm=TRUE),
                          max(t.obj$herz116$data$wind_speed, na.rm=TRUE))) + 1

    if (era.monthly) breaks = seq(min.val, max.val, 0.5)
    if (!era.monthly) breaks = seq(min.val, max.val, 0.75)

    dummy = numeric(length=length(t.obj$era20c10$data$wind_speed)) * NA

    fname = gsub('Histogram', 'Histogram_100m', fname)
    pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height/2.,
        onefile=TRUE, pointsize=13)
    par(mfrow=c(1,3), mar=c(3,3,2,1), cex=1.1)

    titname = paste0(monthly.ext, " wind speed in ", tit.100.ext)
    histoPlot(data.100.vals, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.full)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(data.100.vals))

    if (t.obj$tower$data$StationName[1] == "Cabauw") {
      titname = paste0(monthly.ext, " wind speed in ", tit2.100.ext)
      histoPlot(data2.100.vals, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.full, ylabname.full)
      plotLegendStats(xlims=c(min.val, max.val), as.numeric(data2.100.vals))
    }

    titname = paste0(monthly.ext, " wind speed of HErZ at 116m")
    histoPlot(t.obj$herz116$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$herz116$data$wind_speed))

    titname = paste0(monthly.ext, " wind speed of ERA20C at 100m")
    histoPlot(t.obj$era20c100$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$era20c100$data$wind_speed))
    dev.off()

    fname = gsub('Histogram_100m', 'Histogram_10m', fname)
    pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height/2.,
        onefile=TRUE, pointsize=13)
    par(mfrow=c(1,3), mar=c(3,3,2,1), cex=1.1)

    titname = paste0(monthly.ext, " wind speed of ", tit.10.ext)
    histoPlot(data.10.vals, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.full)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(data.10.vals))

    titname = paste0(monthly.ext, " wind speed of HErZ at 10m")
    histoPlot(t.obj$herz10$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$herz10$data$wind_speed))

    titname = paste0(monthly.ext, " wind speed of ERA20C at 10m")
    histoPlot(t.obj$era20c10$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$era20c10$data$wind_speed))
    dev.off()

    fname = gsub('Histogram_10m', 'HistogramComp_10m', fname)
    pdf(paste0(outdir, fname), width=land.a4width/0.75, height=land.a4height,
        onefile=TRUE, pointsize=13)
    par(mfrow=c(1,2), mar=c(3,3,2,1), cex=1.1)

    titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                     tit.10.ext, " in green and ERA20C shaded")
    histoPlot(data.10.vals, t.obj$era20c10$data$wind_speed, breaks,
              xlims=c(min.val, max.val), titname, xlabname.full, ylabname.full,
              xaxis=axis.y, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                     tit.10.ext, " in green and COSMO HErZ shaded")
    histoPlot(data.10.vals, t.obj$herz10$data$wind_speed, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty, xaxis=axis.y, addPlot=TRUE)
    dev.off()


    fname = gsub('HistogramComp_10m', 'HistogramComp_100m', fname)
    pdf(paste0(outdir, fname), width=land.a4width/0.75, height=land.a4height,
        onefile=TRUE, pointsize=13)
    par(mfrow=c(1,2), mar=c(3,3,3,1))

    titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                     tit.100.ext, " in green and ERA20C shaded")
    histoPlot(data.100.vals, t.obj$era20c100$data$wind_speed, breaks,
              xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.full, xaxis=axis.y, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                     tit.100.ext, " in green and COSMO HErZ shaded")
    histoPlot(data.100.vals, t.obj$herz116$data$wind_speed, breaks,
              xlims=c(min.val, max.val), titname, xlabname.full, ylabname.empty,
              xaxis=axis.y, addPlot=TRUE)

    if (t.obj$tower$data$StationName[1] == "Cabauw") {
      titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                       tit2.100.ext, " in green and ERA20C shaded")
      histoPlot(data2.100.vals, t.obj$era20c100$data$wind_speed, breaks,
                xlims=c(min.val, max.val), titname, xlabname.full, ylabname.full,
                xaxis=axis.y, addPlot=TRUE)

      titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                       tit2.100.ext, " in green and COSMO HErZ shaded")
      histoPlot(data2.100.vals, t.obj$herz116$data$wind_speed, breaks,
                xlims=c(min.val, max.val), titname, xlabname.full, ylabname.empty,
                xaxis=axis.y, addPlot=TRUE)
    }
    dev.off()

  } else if (t.obj$tower$data$StationName[1] == "Fino1" |
             t.obj$tower$data$StationName[1] == "Fino2") {

    tit.ext = paste0(t.obj$tower$data$StationName[1], " at ",
                     t.obj$tower$data$height[1], "m ")
    data.vals = t.obj$tower$data$wind_speed

    min.val = floor(min(min(t.obj$era20c10$data$wind_speed, na.rm=TRUE),
                        min(t.obj$herz10$data$wind_speed, na.rm=TRUE)))
    max.val = ceiling(max(max(t.obj$era20c100$data$wind_speed, na.rm=TRUE),
                          max(data.vals, na.rm=TRUE),
                          max(t.obj$herz116$data$wind_speed, na.rm=TRUE))) + 1

    if (era.monthly) breaks = seq(min.val, max.val, 0.5)
    if (!era.monthly) breaks = seq(min.val, max.val, 0.75)

    dummy = numeric(length=length(t.obj$era20c10$data$wind_speed)) * NA

    fname = gsub('Histogram', 'Histogram_100m', fname)
    pdf(paste0(outdir, fname), width=land.a4width, height=land.a4height/2.,
        onefile=TRUE, pointsize=13)
    par(mfrow=c(1,3), mar=c(3,3,2,1), cex=1.1)
    titname = paste0(monthly.ext, " wind speed in ", tit.ext)
    histoPlot(data.vals, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.full)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(data.vals))

    titname = paste0(monthly.ext, " wind speed of HErZ at 116m")
    histoPlot(t.obj$herz116$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$herz116$data$wind_speed))

    titname = paste0(monthly.ext, " wind speed of ERA20C at 100m")
    histoPlot(t.obj$era20c100$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$era20c100$data$wind_speed))
    dev.off()

    tit.ext = paste0(t.obj$tower$data$height[1], "m wind speed\nat ",
                     t.obj$tower$data$StationName[1])

    fname = gsub('Histogram_100m', 'HistogramComp_100m', fname)
    pdf(paste0(outdir, fname), width=land.a4width/0.75, height=land.a4height,
        onefile=TRUE, pointsize=13)
    par(mfrow=c(1,2), mar=c(3,3,3,1))

    titname = paste0("Frequency distribution of ", monthly.ext, tit.ext,
                     "in green and ERA20C shaded")
    histoPlot(data.vals, t.obj$era20c100$data$wind_speed, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.full, xaxis=axis.y, addPlot=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, tit.ext,
                     "in green and COSMO HErZ shaded")
    histoPlot(data.vals, t.obj$herz116$data$wind_speed, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty, xaxis=axis.y, addPlot=TRUE)
    dev.off()

  }
}

#-----------------------------------------------------------------------------------

#' @title Calculate the S_score as described in Mayer et al., 2015.
#' @description Mayer et al., 2015: "Identifying added value in high-resolution
#'   climate simulations over Scandinavia" describe the S_score based on the
#'   examination of two PDFs and checking their overlap. Here, these two PDFs are
#'   built off one pixel of a reanalysis time series and the corresponding station
#'   time series. Here, the S_score is plotted as January through December values,
#'   displaying the annual cycle.  The plot is saved in pdf format and there is no
#'   return value.
#' @param era.xts is the reanalysis time series of one pixel
#' @param station.xts is the corresponding station time series
#' @param outdir is a string containing the output path of the plot
#' @param fname is a string of the file name of the plot file
#' @param titname is a string containig the title name of the plot
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
PlotPDFScore <- function(era.xts, station.xts, outdir, fname, titname,
                         era.monthly=FALSE) {

  pdf(paste(outdir, fname, sep=""), width=land.a4width, height=land.a4height,
      onefile=TRUE, pointsize=13)

  date.era  <- as.POSIXlt(index(era.xts))
  date.stat <- as.POSIXlt(index(station.xts))
  PDF.score.anncycle = vector(mode="numeric", length=12)
  PDF.score.ann = vector(mode="numeric", length=12)
  months = c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep",
             "Okt", "Nov", "Dez")
  max.abs.val = ceiling(max(max(era.xts), max(station.xts)))

  if (era.monthly) {
    titname.hist = paste0("Monthly ", titname)
    titname.pdfs = paste0("PDF score of monthly ", titname)
    titname.viol = paste0("Violine plot of monthly ", titname)
  } else {
    titname.hist = paste0("Daily ", titname)
    titname.pdfs = paste0("PDF score of daily ", titname)
    titname.viol = paste0("Violine plot of daily ", titname)
  }

  for (month in seq(0,11)) {
    monthly.era  <- era.xts[which(date.era$mon==month)]
    monthly.stat <- station.xts[which(date.stat$mon==month)]

    min.val = floor(min(min(monthly.era, na.rm=TRUE), min(monthly.stat, na.rm=TRUE)))
    max.val = ceiling(max(max(monthly.era, na.rm=TRUE), max(monthly.stat, na.rm=TRUE)))

    breaks = seq(min.val, max.val, 0.25)

    xlabname = "wind speed [m/s]"
    ylabname = "Density"
    histoPlot(monthly.era, monthly.stat, breaks, xlims=c(min.val, max.val),
              titname.hist, xlabname, ylabname, addPlot=T)

    # par("usr") prvides the currently set axis limits
    text(min.val+0.1, par("usr")[4]-0.05, months[month+1], cex=2.)

    PDF.score.anncycle[month+1] = PDFscore(monthly.era, monthly.stat)
  }
  PDF.score.ann[] = PDFscore(era.xts, station.xts)

  plot(PDF.score.anncycle, main=titname.pdfs, ylab="pdf score",
       xlab="months of the year", type="b", pch=16, col="blue")
  lines(PDF.score.ann, type="b", lty=2, pch=20, col="red")

  vioplot(era.xts, station.xts, horizontal=TRUE,
          names=c("reanalysis", "station data"))
  mtext(titname.viol, line=1, font=2, cex=1.2)
  mtext(paste0("wind speed [m/s]"), side=1, line=2)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot wind speed profile as box plots of tower measurements and ERA.
#' @description This function plots the wind speed profile of tower measurements and
#'   data from reanalyses as box plots. Tower measurements of Lindenberg, Cabauw,
#'   Fino1, and Fino2 are supported.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname string of the file name of the plot
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
PlotTowerERAprofileBP <- function(tower.obj, fname, era.monthly) {

  t.obj = tower.obj$climate_data_objects
  dummy = numeric(length=length(t.obj$herz10$data$wind_speed)) * NA
  x.labs = "wind speed [m/s]"
  swex = 0.4
  bwex = 0.3
  hori=TRUE
  nch = TRUE
  oline = FALSE
  if (era.monthly) {
    wind.range = c(0,15)
    plot.ext = " Monthly "
  }
  else {
    wind.range = c(0,20)
    plot.ext = " Daily "
  }

  pdf(fname, width=port.a4width, height=port.a4height/0.8, onefile=TRUE, pointsize=13)
  par(mar=c(4,7,3,0.5), cex=1.5)

  if (t.obj$tower$data$StationName[1] == "Lindenberg") {
    boxplot.default(t.obj$tower$data$wind_speed,
                    t.obj$herz10$data$wind_speed,
                    t.obj$era20c10$data$wind_speed,
                    t.obj$tower2$data$wind_speed,
                    t.obj$herz35$data$wind_speed,
                    t.obj$tower3$data$wind_speed,
                    t.obj$tower4$data$wind_speed,
                    t.obj$herz69$data$wind_speed,
                    t.obj$tower5$data$wind_speed,
                    t.obj$tower6$data$wind_speed,
                    t.obj$era20c100$data$wind_speed,
                    t.obj$herz116$data$wind_speed,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("Lind 10m", "HErZ 10m", "ERA20C 10m", "Lind 20m",
                            "HErZ 35m", "Lind 40m", "Lind 60m", "HErZ 69m",
                            "Lind 80m", "Lind 98m", "ERA20C 100m", "HErZ 116m"),
                    col=c("red", "blue", "green", "red", "blue", "red", "red",
                          "blue", "red", "red", "green", "blue"))
  } else if (t.obj$tower$data$StationName[1] == "Cabauw") {
    boxplot.default(t.obj$tower$data$wind_speed,
                    t.obj$herz10$data$wind_speed,
                    t.obj$era20c10$data$wind_speed,
                    t.obj$tower2$data$wind_speed,
                    t.obj$herz35$data$wind_speed,
                    t.obj$tower3$data$wind_speed,
                    t.obj$herz69$data$wind_speed,
                    t.obj$tower4$data$wind_speed,
                    t.obj$era20c100$data$wind_speed,
                    t.obj$herz116$data$wind_speed,
                    t.obj$tower5$data$wind_speed,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("Cabauw 10m", "HErZ 10m", "ERA20C 10m", "Cabauw 20m",
                            "HErZ 35m", "Cabauw 40m", "HErZ 69m", "Cabauw 80m",
                            "ERA20C 100m", "HErZ 116m", "Cabauw 140m"),
                    col=c("red", "blue", "green", "red", "blue", "red",
                          "blue", "red", "green", "blue", "red"))
  } else if (t.obj$tower$data$StationName[1] == "Fino1") {
    boxplot.default(dummy,
                    t.obj$herz10$data$wind_speed,
                    t.obj$era20c10$data$wind_speed,
                    dummy,
                    t.obj$herz35$data$wind_speed,
                    dummy,
                    dummy,
                    t.obj$herz69$data$wind_speed,
                    dummy,
                    t.obj$era20c100$data$wind_speed,
                    t.obj$tower$data$wind_speed,
                    t.obj$herz116$data$wind_speed,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("", "HErZ 10m", "ERA20C 10m", "", "HErZ 35m", "", "",
                            "HErZ 69m", "", "ERA20C 100m", "Fino1 100m", "HErZ 116m"),
                    col=c("", "blue", "green", "", "blue", "", "", "blue", "",
                          "green", "red", "blue"))
  } else if (t.obj$tower$data$StationName[1] == "Fino2") {
    boxplot.default(dummy,
                    t.obj$herz10$data$wind_speed,
                    t.obj$era20c10$data$wind_speed,
                    dummy,
                    t.obj$herz35$data$wind_speed,
                    dummy,
                    dummy,
                    t.obj$herz69$data$wind_speed,
                    dummy,
                    t.obj$era20c100$data$wind_speed,
                    t.obj$tower$data$wind_speed,
                    t.obj$herz116$data$wind_speed,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("", "HErZ 10m", "ERA20C 10m", "", "HErZ 35m", "", "",
                            "HErZ 69m", "", "ERA20C 100m", "Fino2 102m", "HErZ 116m"),
                    col=c("", "blue", "green", "", "blue", "", "", "blue", "",
                          "green", "red", "blue"))
  } else {
    CallStop(paste0("Unexpected tower name: ",
                    tower.obj$climate_data_objects$tower$data$StationName[1], " "))
  }
  title(main=paste0("Monthly wind speed profile at ",
                    t.obj$tower$data$StationName[1]), line=1, cex=1.5)
  mtext(x.labs, side=1, line=2, cex=1.5)
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot relative difference between tower measurements and reanalysis data.
#' @description Plot the relative difference between tower measurements at 10m and
#'   100m compared to reanalysis data. Tower measurements of Lindenberg, Cabauw,
#'   Fino1, and Fino2 are supported.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname string of the file name of the plot
PlotTowerERAprofileRelDiff <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  legend.cex = 0.8
  tower.date <- as.POSIXlt(t.obj$tower$data$date)

  yliml.rel = -0.75
  ylimh.rel = 0.75
  color = list(tower="blue", herz="red", era20="green", black="black")

  if (t.obj$tower$data$StationName[1] == "Lindenberg") {
    plot.10.ext = paste0(t.obj$tower$data$StationName[1], " at ",
                         t.obj$tower$data$height[1], "m ")
    plot.100.ext = paste0(t.obj$tower6$data$StationName[1], " at ",
                          t.obj$tower6$data$height[1], "m ")
  } else if (t.obj$tower$data$StationName[1] == "Cabauw") {
    plot.10.ext = paste0(t.obj$tower$data$StationName[1], " at ",
                         t.obj$tower$data$height[1], "m ")
    plot.100.ext = paste0(t.obj$tower4$data$StationName[1], " at ",
                          t.obj$tower4$data$height[1], "m ")
  } else if (t.obj$tower$data$StationName[1] == "Fino1" |
             t.obj$tower$data$StationName[1] == "Fino2") {
    plot.100.ext = paste0(t.obj$tower$data$StationName[1], " at ",
                          t.obj$tower$data$height[1], "m ")
  } else {
    CallStop(paste0("Unexpected tower name: ", t.obj$tower$data$StationName[1], " "))
  }

  pdf(fname, width=port.a4width/0.67, height=port.a4height, onefile=TRUE, pointsize=13)
  par(mfrow=c(3,1), mar=c(0,4,0,0), oma=c(4,0,3,0.5), cex=1.3)

  # == relative TS in 100m height ==
  dummy = numeric(length=length(tower.date)) * NA
  dummy = xts(dummy, order.by=tower.date)

  h.xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  if (t.obj$tower$data$StationName[1] == "Lindenberg")
    l.xts = xts(t.obj$tower6$data$wind_speed, order.by=tower.date)
  if (t.obj$tower$data$StationName[1] == "Cabauw")
    l.xts = xts(t.obj$tower4$data$wind_speed, order.by=tower.date)
  if (t.obj$tower$data$StationName[1] == "Fino1" |
      t.obj$tower$data$StationName[1] == "Fino2")
    l.xts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)

  plot(dummy, main=NULL, xaxt="n", ylim=c(yliml.rel, ylimh.rel), las=1)
  title(ylab="relative difference", line=2.5)
  lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
  lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
  corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
  legend("bottomleft", legend=c("HErZ at 116m", plot.100.ext,
                                paste0("correlation = ", round(corr$estimate, 2))),
         text.col=c(color$herz, color$tower, color$black))


  h.xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  #l.xts from above
  plot(dummy, main=NULL, xaxt="n", ylim=c(yliml.rel, ylimh.rel), las=1)
  title(ylab="relative difference", line=2.5)
  lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$era20)
  lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
  corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
  legend("bottomleft", legend=c("ER20C at 100m", plot.100.ext,
                                paste0("correlation = ", round(corr$estimate, 2))),
         text.col=c(color$era20, color$tower, color$black))

  h.xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
  if (t.obj$tower$data$StationName[1] == "Lindenberg" |
      t.obj$tower$data$StationName[1] == "Cabauw") {
    l.xts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), las=1)
    title(ylab="relative difference", line=2.5)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    legend("bottomleft", legend=c("HErZ at 10m", plot.10.ext,
                                  paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$herz, color$tower, color$black))
  }
  mtext(paste0("Monthly relative wind speed differences of ",
               t.obj$tower$data$StationName[1], " against HErZ and ERA20C"),
        outer=TRUE, line=1, cex=1.6)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot annual variability between tower measurements and reanalysis data.
#' @description Plot the annual variability between tower measurements and reanalysis
#'   data at the heights of 10m and 100m where applicable. Two months are selected
#'   to visualize the annual variability; January and August are predifined and may
#'   be changed but only locally so far. Tower measurements of Lindenberg, Cabauw,
#'   Fino1, and Fino2 are supported.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname string of the file name of the plot
PlotTowerERAprofileAnnualVar <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  legend.cex = 0.75
  tower.date <- as.POSIXlt(t.obj$tower$data$date)

  months=list(1,8)
  all.months = c("January","February","March","April","May","June","July",
                 "August","September","October","November","December")

  colorH = c("red", "black")
  colorE = c("green", "black")
  colorT = c("blue", "black")

  mon.Era20c100 = list()
  mon.Herz116 = list()
  mon.Herz10 = list()

  Era20c100Xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  Herz116Xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  Herz10Xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)

  for (cnt in seq(12)) {
    mon.Era20c100[[cnt]] = Era20c100Xts[which( tower.date$mon==cnt-1 )]
    mon.Herz116[[cnt]] = Herz116Xts[which( tower.date$mon==cnt-1 )]
    mon.Herz10[[cnt]] = Herz10Xts[which( tower.date$mon==cnt-1 )]
  }

  if (t.obj$tower$data$StationName[1] == "Lindenberg" |
      t.obj$tower$data$StationName[1] == "Cabauw") {

    if (t.obj$tower$data$StationName[1] == "Lindenberg") {
      plot.ext.10 = "Lindenberg at 10m"
      plot.ext.100 = "Lindenberg at 98m"
      mon.tower.100 = list()
      mon.tower.10 = list()
      Lind98Xts = xts(t.obj$tower6$data$wind_speed, order.by=tower.date)
      Lind10Xts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)
      for (cnt in seq(12)) {
        mon.tower.100[[cnt]] = Lind98Xts[which( tower.date$mon==cnt-1 )]
        mon.tower.10[[cnt]] = Lind10Xts[which( tower.date$mon==cnt-1 )]
      }
    } else if (t.obj$tower$data$StationName[1] == "Cabauw") {
      plot.ext.100 = "Cabauw at 80m"
      plot.ext2.100 = "Cabauw at 140m"
      plot.ext.10 = "Cabauw at 10m"
      mon.tower.100 = list()
      mon.tower2.100 = list()
      mon.tower.10 = list()
      Cabauw10Xts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)
      Cabauw80Xts = xts(t.obj$tower4$data$wind_speed, order.by=tower.date)
      Cabauw140Xts = xts(t.obj$tower5$data$wind_speed, order.by=tower.date)
      for (cnt in seq(12)) {
        mon.tower.100[[cnt]] = Cabauw80Xts[which( tower.date$mon==cnt-1 )]
        mon.tower2.100[[cnt]] = Cabauw140Xts[which( tower.date$mon==cnt-1 )]
        mon.tower.10[[cnt]] = Cabauw10Xts[which( tower.date$mon==cnt-1 )]
      }
    }

    yliml = vector(mode="numeric", length=length(months))
    ylimh = vector(mode="numeric", length=length(months))
    for (cnt in seq(months)) {
      Ylims = GetYlims(mon.tower.100[[months[[cnt]]]], mon.Herz116[[months[[cnt]]]],
                       mon.tower.10[[months[[cnt]]]], mon.Herz10[[months[[cnt]]]])
      yliml[cnt] = Ylims$yll
      ylimh[cnt] = Ylims$ylh
    }
    yliml = 0 #min(yliml)
    ylimh = max(ylimh)

    pdf(fname, width=port.a4width, height=port.a4height, onefile=TRUE, pointsize=13)
    if (t.obj$tower$data$StationName[1] == "Lindenberg") {
      par(mfrow=c(5,1), mar=c(0,2,0,0), oma=c(4,1,3,0.5), cex=0.9)
    } else if (t.obj$tower$data$StationName[1] == "Cabauw") {
      par(mfrow=c(6,1), mar=c(0,2,0,0), oma=c(4,1,3,0.5), cex=0.9)
    }
    dummy = numeric(length=length(Era20c100Xts)) * NA
    dummy = xts(dummy, order.by = index(Era20c100Xts))

    if (t.obj$tower$data$StationName[1] == "Cabauw") {
      plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
      for (cnt in seq(months)) {
        lines(mon.tower2.100[[months[[cnt]]]], type="b", pch=16, col=colorT[cnt],
              bg=rgb(0,0,0,1./cnt), lw=2)
      }
      legend("bottomleft", legend=c(paste0(plot.ext2.100, " ", all.months[months[[1]]]),
                                    paste0(plot.ext2.100, " ", all.months[months[[2]]])),
             text.col=colorT, cex=legend.cex)
    }

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Herz116[[months[[cnt]]]], type="b", pch=16, col=colorH[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0("HErZ at 116m ", all.months[months[[1]]]),
                                  paste0("HErZ at 116m ", all.months[months[[2]]])),
           text.col=colorH, cex=legend.cex)

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Era20c100[[months[[cnt]]]], type="b", pch=16, col=colorE[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0("Era20C at 100m ", all.months[months[[1]]]),
                                  paste0("Era20C at 100m ", all.months[months[[2]]])),
           text.col=colorE, cex=legend.cex)

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.tower.100[[months[[cnt]]]], type="b", pch=16, col=colorT[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0(plot.ext.100, " ", all.months[months[[1]]]),
                                  paste0(plot.ext.100, " ", all.months[months[[2]]])),
           text.col=colorT, cex=legend.cex)

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.tower.10[[months[[cnt]]]], type="b", pch=16, col=colorT[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("topleft", legend=c(paste0(plot.ext.10 ," ", all.months[months[[1]]]),
                               paste0(plot.ext.10 ," ", all.months[months[[2]]])),
           text.col=colorT, cex=legend.cex)

    plot(dummy, main=NULL, ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Herz10[[months[[cnt]]]], type="b", pch=16, col=colorH[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("topleft", legend=c(paste0("HErZ at 10m ", all.months[months[[1]]]),
                               paste0("HErZ at 10m ", all.months[months[[2]]])),
           text.col=colorH, cex=legend.cex)

    if (t.obj$tower$data$StationName[1] == "Lindenberg") {
      mtext("Wind speed at Lindenberg for different months",
            outer=TRUE, line=1, cex=1.0)
    } else if (t.obj$tower$data$StationName[1] == "Cabauw") {
      mtext("Wind speed at Cabauw for different months",
            outer=TRUE, line=1, cex=1.0)
    }
    mtext("wind speed [m/s]", line=0, side=2, outer=T)

    dev.off()

  } else if (t.obj$tower$data$StationName[1] == "Fino1" |
             t.obj$tower$data$StationName[1] == "Fino2") {

    plot.ext = paste0(t.obj$tower$data$StationName[1], " at ",
                      t.obj$tower$data$height[1], "m ")
    mon.tower = list()
    towerXts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)

    for (cnt in seq(12)) {
      mon.tower[[cnt]] = towerXts[which( tower.date$mon==cnt-1 )]
    }

    yliml = vector(mode="numeric", length=length(months))
    ylimh = vector(mode="numeric", length=length(months))
    for (cnt in seq(months)) {
      Ylims = GetYlims(mon.Era20c100[[months[[cnt]]]], mon.tower[[months[[cnt]]]],
                       mon.Herz116[[months[[cnt]]]], mon.Herz10[[months[[cnt]]]])
      yliml[cnt] = Ylims$yll
      ylimh[cnt] = Ylims$ylh
    }
    yliml = min(yliml)
    ylimh = max(ylimh)

    pdf(fname, width=land.a4width/2., height=land.a4height, onefile=TRUE, pointsize=13)
    par(mfrow=c(3,1), mar=c(0,2,0,0), oma=c(4,1,3,0.5), cex=0.9)

    dummy = numeric(length=length(Era20c100Xts)) * NA
    dummy = xts(dummy, order.by = index(Era20c100Xts))

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.tower[[months[[cnt]]]], type="b", pch=16, col=colorT[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0(plot.ext, all.months[months[[1]]]),
                                  paste0(plot.ext, all.months[months[[2]]])),
           text.col=colorT, cex=legend.cex)

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Herz116[[months[[cnt]]]], type="b", pch=16, col=colorH[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0("HErZ at 116m ", all.months[months[[1]]]),
                                  paste0("HErZ at 116m ", all.months[months[[2]]])),
           text.col=colorH, cex=legend.cex)

    plot(dummy, main=NULL, ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Era20c100[[months[[cnt]]]], type="b", pch=16, col=colorE[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0("Era20C at 100m ", all.months[months[[1]]]),
                                  paste0("Era20C at 100m ", all.months[months[[2]]])),
           text.col=colorE, cex=legend.cex)

    mtext(paste0("wind speed at ", t.obj$tower$data$StationName[1],
                 " at 100m for different months"),
          outer=TRUE, line=1, cex=1.0)
    mtext("wind speed [m/s]", line=0, side=2, outer=TRUE)

    dev.off()

  } else {
    CallStop(paste0("Unexpected tower name: ", t.obj$tower$data$StationName[1], " "))
  }
}


#-----------------------------------------------------------------------------------

#' @title Plot annual cycle between tower measurements and reanalysis data.
#' @description Plot the annual cycle between tower measurements and reanalysis data
#'   at all available heights for absolute and relative values.  Tower measurements
#'   of Lindenberg, Cabauw, Fino1, and Fino2 are supported.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname string of the file name of the plot
PlotTowerERAprofileAnnualCycle <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  legend.cex = 0.75
  tower.date <- as.POSIXlt(t.obj$tower$data$date)

  all.months = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                 "Aug","Sep","Oct","Nov","Dec")

  date.ancycle = as.yearmon(2000 + seq(0, 11)/12)

  mon.Era20c100 = vector(mode="numeric", length=12)
  mon.Herz116 = vector(mode="numeric", length=12)
  mon.Herz69 = vector(mode="numeric", length=12)
  mon.Herz35 = vector(mode="numeric", length=12)
  mon.Herz10 = vector(mode="numeric", length=12)
  Era20c100Xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  Herz116Xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  Herz69Xts = xts(t.obj$herz69$data$wind_speed, order.by=tower.date)
  Herz35Xts = xts(t.obj$herz35$data$wind_speed, order.by=tower.date)
  Herz10Xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)

  for (cnt in seq(12)) {
    mon.Era20c100[cnt] = mean(Era20c100Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz116[cnt] = mean(Herz116Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz69[cnt] = mean(Herz69Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz35[cnt] = mean(Herz35Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz10[cnt] = mean(Herz10Xts[which( tower.date$mon==cnt-1 )])
  }

  if (t.obj$tower$data$StationName[1] == "Lindenberg") {

    yliml.rel = -0.2
    ylimh.rel = 0.2

    mon.Lind98 = vector(mode="numeric", length=12)
    mon.Lind60 = vector(mode="numeric", length=12)
    mon.Lind40 = vector(mode="numeric", length=12)
    mon.Lind10 = vector(mode="numeric", length=12)
    Lind98Xts = xts(t.obj$tower6$data$wind_speed, order.by=tower.date)
    Lind60Xts = xts(t.obj$tower4$data$wind_speed, order.by=tower.date)
    Lind40Xts = xts(t.obj$tower3$data$wind_speed, order.by=tower.date)
    Lind10Xts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)
    for (cnt in seq(12)) {
      mon.Lind98[cnt] = mean(Lind98Xts[which( tower.date$mon==cnt-1 )])
      mon.Lind60[cnt] = mean(Lind60Xts[which( tower.date$mon==cnt-1 )])
      mon.Lind40[cnt] = mean(Lind40Xts[which( tower.date$mon==cnt-1 )])
      mon.Lind10[cnt] = mean(Lind10Xts[which( tower.date$mon==cnt-1 )])
    }

    Ylims = GetYlims(xts(mon.Era20c100, order.by=date.ancycle),
                     xts(mon.Lind98, order.by=date.ancycle),
                     xts(mon.Herz116, order.by=date.ancycle),
                     xts(mon.Herz10, order.by=date.ancycle))
    yliml = Ylims$yll
    ylimh = Ylims$ylh

    dummy = numeric(length=length(mon.Herz116)) * NA

    pdf(fname, width=land.a4width/2., height=land.a4height, onefile=TRUE, pointsize=13)
    par(mfrow=c(4,1), oma=c(3,3,3,0.5), mar=c(0,0,0,0), cex=1.1)
    # !!! col.axis = "white" is a hack to suppress the tick labels
    # !!! in order to set them manually
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    title("Annual cycle of wind speed at Lindenberg", outer=TRUE, line=1, cex=0.9)
    title(ylab="wind speed [m/s]", outer=TRUE, line=2)
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind98, type="b", pch=16, col="blue", lw=2)
    lines(mon.Era20c100, type="b", pch=16, col="green", lw=2)
    lines(mon.Herz116, type="b", pch=16, col="red", lw=2)
    arrows(1:12, mon.Lind98-sd(mon.Lind98), 1:12, mon.Lind98+sd(mon.Lind98),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Era20c100-sd(mon.Era20c100), 1:12, mon.Era20c100+sd(mon.Era20c100),
           length=0.05, angle=90, code=3, col="green")
    arrows(1:12, mon.Herz116-sd(mon.Herz116), 1:12, mon.Herz116+sd(mon.Herz116),
           length=0.05, angle=90, code=3, col="red")
    # corr1 = cor.test(as.numeric(Era20c100Xts), as.numeric(Lind98Xts))
    # corr2 = cor.test(as.numeric(Herz116Xts), as.numeric(Lind98Xts))
    # legend("top", legend=c("Lind at 98m ", paste0("Era20C at 100m with corr = ",
    #                                         round(corr1$estimate, 2)),
    #          paste0("Herz at 116m with corr = ",round(corr2$estimate, 2))),
    #    text.col=c("blue", "green", "red"), cex=legend.cex)
    legend("top", legend=c("Lind at 98m ", "Era20C at 100m", "Herz at 116m"),
           text.col=c("blue", "green", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind60, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz69, type="b", pch=16, col="red", lw=2)
    arrows(1:12, mon.Lind60-sd(mon.Lind60), 1:12, mon.Lind60+sd(mon.Lind60),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz69-sd(mon.Herz69), 1:12, mon.Herz69+sd(mon.Herz69),
           length=0.05, angle=90, code=3, col="red")
    legend("top", legend=c("Lind at 60m", "Herz at 69m"),
           text.col=c("blue", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind40, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz35, type="b", pch=16, col="red", lw=2)
    arrows(1:12, mon.Lind40-sd(mon.Lind40), 1:12, mon.Lind40+sd(mon.Lind40),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz35-sd(mon.Herz35), 1:12, mon.Herz35+sd(mon.Herz35),
           length=0.05, angle=90, code=3, col="red")
    legend("top", legend=c("Lind at 40m", "Herz at 35m"),
           text.col=c("blue", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind10, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz10, type="b", pch=16, col="red", lw=2)
    arrows(1:12, mon.Lind10-sd(mon.Lind10), 1:12, mon.Lind10+sd(mon.Lind60),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="red")
    legend("top", legend=c("Lind at 10m", "Herz at 10m"),
           text.col=c("blue", "red"), cex=legend.cex)

    axis(1, labels=all.months, at = 1:12)

    dev.off()
    ylimh = ylimh - 1


    fname = gsub("annualCycle", "annualCycleSingleArrow", fname)
    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", ylab="", main="")
    title(main="Annual cycle of wind speed at Lindenberg", line=1, cex=1.1)
    title(ylab="wind speed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12, las=1)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh, las=1)

    lines(mon.Herz116, type="b", pch=16, col="violetred",lw=2)
    arrows(1:12, mon.Herz116-sd(mon.Herz116), 1:12, mon.Herz116+sd(mon.Herz116),
           length=0.05, angle=90, code=3, col="violetred")
    lines(mon.Lind98, type="b", pch=16, col="purple2",lw=2)
    arrows(1:12, mon.Lind98-sd(mon.Lind98), 1:12, mon.Lind98+sd(mon.Lind98),
           length=0.05, angle=90, code=3, col="purple2")
    lines(mon.Era20c100, type="b", pch=16, col="green",lw=2)
    arrows(1:12, mon.Era20c100-sd(mon.Era20c100), 1:12, mon.Era20c100+sd(mon.Era20c100),
           length=0.05, angle=90, code=3, col="green")
    lines(mon.Herz69, type="b", pch=16, col="chocolate",lw=2)
    #     arrows(1:12, as.numeric(Herz69Xts)-sd(Herz69Xts), 1:12,
    #            as.numeric(Herz69Xts)+sd(Herz69Xts), length=0.05, angle=90,
    #            code=3, col="chocolate")
    lines(mon.Lind60, type="b", pch=16, col="deepskyblue",lw=2)
    #     arrows(1:12, as.numeric(Lind60Xts)-sd(Lind60Xts), 1:12,
    #            as.numeric(Lind60Xts)+sd(Lind60Xts), length=0.05, angle=90,
    #            code=3, col="deepskyblue")
    lines(mon.Lind40, type="b", pch=16, col="blue",lw=2)
    #     arrows(1:12, as.numeric(Lind40Xts)-sd(Lind40Xts), 1:12,
    #            as.numeric(Lind40Xts)+sd(Lind40Xts), length=0.05, angle=90,
    #            code=3, col="blue")
    lines(mon.Herz35, type="b", pch=16, col="red",lw=2)
    #     arrows(1:12, as.numeric(Herz35Xts)-sd(Herz35Xts), 1:12,
    #            as.numeric(Herz35Xts)+sd(Herz35Xts), length=0.05, angle=90,
    #            code=3, col="red")
    lines(mon.Lind10, type="b", pch=16, col="darkturquoise",lw=2)
    arrows(1:12, mon.Lind10-sd(mon.Lind10), 1:12, mon.Lind10+sd(mon.Lind10),
           length=0.05, angle=90, code=3, col="darkturquoise")
    lines(mon.Herz10, type="b", pch=16, col="orange",lw=2)
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="orange")
    legend("top", legend=c("Herz at 116m", "Lind at 98m ", "Era20C at 100m",
                           "Herz at 69m", "Lind at 60m", "Lind at 40m", "Herz at 35m",
                           "Lind at 10m", "Herz at 10m"),
           text.col=c("violetred", "purple2", "green", "chocolate", "deepskyblue",
                      "blue", "red", "darkturquoise", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingleArrow", "annualCycleRelDiffSingle", fname)
    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.8)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         xlab="", ylab="", main="")
    title(main="Annual cycle of relative wind speed at Lindenberg", line=1, cex=3)
    title(ylab="relative difference", line=2, cex=1.8)
    axis(1, labels=all.months, at = 1:12, las=1, cex=1.8)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel), las=1)
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16,
          col="violetred",lw=2)
    lines(RelDiff(mon.Lind98, mean(mon.Lind98)), type="b", pch=16,
          col="purple2",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16,
          col="green",lw=2)
    lines(RelDiff(mon.Herz69, mean(mon.Herz69)), type="b", pch=16,
          col="chocolate",lw=2)
    lines(RelDiff(mon.Lind60, mean(mon.Lind60)), type="b", pch=16,
          col="deepskyblue",lw=2)
    lines(RelDiff(mon.Lind40, mean(mon.Lind40)), type="b", pch=16,
          col="blue",lw=2)
    lines(RelDiff(mon.Herz35, mean(mon.Herz35)), type="b", pch=16,
          col="red",lw=2)
    lines(RelDiff(mon.Lind10, mean(mon.Lind10)), type="b", pch=16,
          col="darkturquoise",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16,
          col="orange",lw=2)
    legend("top", legend=c("Herz at 116m", "Lind at 98m ", "Era20C at 100m",
                           "Herz at 69m", "Lind at 60m", "Lind at 40m", "Herz at 35m",
                           "Lind at 10m", "Herz at 10m"),
           text.col=c("violetred", "purple2", "green", "chocolate", "deepskyblue",
                      "blue", "red", "darkturquoise", "orange"), cex=0.9)

    dev.off()

  } else   if (t.obj$tower$data$StationName[1] == "Cabauw") {

    yliml.rel = -0.2
    ylimh.rel = 0.2

    mon.Cabauw140 = vector(mode="numeric", length=12)
    mon.Cabauw80 = vector(mode="numeric", length=12)
    mon.Cabauw40 = vector(mode="numeric", length=12)
    mon.Cabauw20 = vector(mode="numeric", length=12)
    mon.Cabauw10 = vector(mode="numeric", length=12)
    Cabauw140Xts = xts(t.obj$tower5$data$wind_speed, order.by=tower.date)
    Cabauw80Xts = xts(t.obj$tower4$data$wind_speed, order.by=tower.date)
    Cabauw40Xts = xts(t.obj$tower3$data$wind_speed, order.by=tower.date)
    Cabauw20Xts = xts(t.obj$tower2$data$wind_speed, order.by=tower.date)
    Cabauw10Xts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)
    for (cnt in seq(12)) {
      mon.Cabauw140[cnt] = mean(Cabauw140Xts[which( tower.date$mon==cnt-1 )])
      mon.Cabauw80[cnt] = mean(Cabauw80Xts[which( tower.date$mon==cnt-1 )])
      mon.Cabauw40[cnt] = mean(Cabauw40Xts[which( tower.date$mon==cnt-1 )])
      mon.Cabauw20[cnt] = mean(Cabauw20Xts[which( tower.date$mon==cnt-1 )])
      mon.Cabauw10[cnt] = mean(Cabauw10Xts[which( tower.date$mon==cnt-1 )])
    }

    Ylims = GetYlims(xts(mon.Cabauw10, order.by=date.ancycle),
                     xts(mon.Cabauw140, order.by=date.ancycle),
                     xts(mon.Herz116, order.by=date.ancycle),
                     xts(mon.Herz10, order.by=date.ancycle))
    yliml = Ylims$yll
    ylimh = Ylims$ylh

    dummy = numeric(length=length(mon.Herz116)) * NA

    pdf(fname, width=land.a4width/2., height=land.a4height, onefile=TRUE, pointsize=13)
    par(mfrow=c(3,1), oma=c(3,3,3,0.5), mar=c(0,0,0,0), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    title("Annual cycle of wind speed at Cabauw", outer=TRUE, line=1, cex=0.9)
    title(ylab="wind speed [m/s]", outer=TRUE, line=2)
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Cabauw140, type="b", pch=16, col="blue", lw=2)
    lines(mon.Cabauw80, type="b", pch=16, col="lightblue", lw=2)
    lines(mon.Era20c100, type="b", pch=16, col="green", lw=2)
    lines(mon.Herz116, type="b", pch=16, col="red", lw=2)
    arrows(1:12, mon.Cabauw140-sd(mon.Cabauw140), 1:12, mon.Cabauw140+sd(mon.Cabauw140),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Cabauw80-sd(mon.Cabauw80), 1:12, mon.Cabauw80+sd(mon.Cabauw80),
           length=0.05, angle=90, code=3, col="lightblue")
    arrows(1:12, mon.Era20c100-sd(mon.Era20c100), 1:12, mon.Era20c100+sd(mon.Era20c100),
           length=0.05, angle=90, code=3, col="green")
    arrows(1:12, mon.Herz116-sd(mon.Herz116), 1:12, mon.Herz116+sd(mon.Herz116),
           length=0.05, angle=90, code=3, col="red")
    legend("top", legend=c("Cabauw at 140m ", "Cabauw at 80m", "Era20C at 100m",
                           "Herz at 116m"),
           text.col=c("blue", "lightblue", "green", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Cabauw40, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz35, type="b", pch=16, col="red", lw=2)
    lines(mon.Cabauw20, type="b", pch=16, col="lightblue", lw=2)
    arrows(1:12, mon.Cabauw40-sd(mon.Cabauw40), 1:12, mon.Cabauw40+sd(mon.Cabauw40),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz35-sd(mon.Herz35), 1:12, mon.Herz35+sd(mon.Herz35),
           length=0.05, angle=90, code=3, col="red")
    arrows(1:12, mon.Cabauw20-sd(mon.Cabauw20), 1:12, mon.Cabauw20+sd(mon.Cabauw20),
           length=0.05, angle=90, code=3, col="lightblue")
    legend("top", legend=c("Cabauw at 40m", "Herz at 35m", "Cabauw at 20m"),
           text.col=c("blue", "red", "lightblue"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Cabauw10, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz10, type="b", pch=16, col="red", lw=2)
    arrows(1:12, mon.Cabauw10-sd(mon.Cabauw10), 1:12, mon.Cabauw10+sd(mon.Cabauw10),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="red")
    legend("top", legend=c("Cabauw at 10m", "Herz at 10m"),
           text.col=c("blue", "red"), cex=legend.cex)

    axis(1, labels=all.months, at = 1:12)

    dev.off()
    ylimh = ylimh - 1


    fname = gsub("annualCycle", "annualCycleSingleArrow", fname)
    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", ylab="", main="")
    title(main="Annual cycle of wind speed at Cabauw", line=1, cex=1.1)
    title(ylab="wind speed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12, las=1)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh, las=1)

    lines(mon.Cabauw140, type="b", pch=16, col="black",lw=2)
    lines(mon.Herz116, type="b", pch=16, col="violetred",lw=2)
    arrows(1:12, mon.Herz116-sd(mon.Herz116), 1:12, mon.Herz116+sd(mon.Herz116),
           length=0.05, angle=90, code=3, col="violetred")
    lines(mon.Era20c100, type="b", pch=16, col="green",lw=2)
    arrows(1:12, mon.Era20c100-sd(mon.Era20c100), 1:12, mon.Era20c100+sd(mon.Era20c100),
           length=0.05, angle=90, code=3, col="green")
    lines(mon.Cabauw80, type="b", pch=16, col="purple2",lw=2)
    arrows(1:12, mon.Cabauw80-sd(mon.Cabauw80), 1:12, mon.Cabauw80+sd(mon.Cabauw80),
           length=0.05, angle=90, code=3, col="purple2")
    lines(mon.Herz69, type="b", pch=16, col="chocolate",lw=2)
    lines(mon.Cabauw40, type="b", pch=16, col="deepskyblue",lw=2)
    lines(mon.Herz35, type="b", pch=16, col="red",lw=2)
    lines(mon.Cabauw20, type="b", pch=16, col="blue",lw=2)
    lines(mon.Cabauw10, type="b", pch=16, col="darkturquoise",lw=2)
    arrows(1:12, mon.Cabauw10-sd(mon.Cabauw10), 1:12, mon.Cabauw10+sd(mon.Cabauw10),
           length=0.05, angle=90, code=3, col="darkturquoise")
    lines(mon.Herz10, type="b", pch=16, col="orange",lw=2)
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="orange")
    legend("top", legend=c("Cabauw at 140m", "Herz at 116m", "Era20C at 100m",
                           "Cabauw at 80m ", "Herz at 69m", "Cabauw at 40m",
                           "Herz at 35m", "Cabauw at 20m", "Cabauw at 10m",
                           "Herz at 10m"),
           text.col=c("black", "violetred", "green", "purple2", "chocolate",
                      "deepskyblue", "red", "blue", "darkturquoise", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingleArrow", "annualCycleRelDiffSingle", fname)
    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.8)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         xlab="", ylab="", main="")
    title(main="Annual cycle of relative wind speed at Cabauw", line=1, cex=3)
    title(ylab="relative difference", line=2, cex=1.8)
    axis(1, labels=all.months, at = 1:12, las=1, cex=1.8)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel), las=1)
    lines(RelDiff(mon.Cabauw140, mean(mon.Cabauw140)), type="b", pch=16,
          col="black",lw=2)
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16,
          col="violetred",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16,
          col="green",lw=2)
    lines(RelDiff(mon.Cabauw80, mean(mon.Cabauw80)), type="b", pch=16,
          col="purple2",lw=2)
    lines(RelDiff(mon.Herz69, mean(mon.Herz69)), type="b", pch=16,
          col="chocolate",lw=2)
    lines(RelDiff(mon.Cabauw40, mean(mon.Cabauw40)), type="b", pch=16,
          col="deepskyblue",lw=2)
    lines(RelDiff(mon.Herz35, mean(mon.Herz35)), type="b", pch=16,
          col="red",lw=2)
    lines(RelDiff(mon.Cabauw20, mean(mon.Cabauw20)), type="b", pch=16,
          col="blue",lw=2)
    lines(RelDiff(mon.Cabauw10, mean(mon.Cabauw10)), type="b", pch=16,
          col="darkturquoise",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16,
          col="orange",lw=2)
    legend("top", legend=c("Cabauw at 140m", "Herz at 116m", "Era20C at 100m",
                           "Cabauw at 80m ", "Herz at 69m", "Cabauw at 40m",
                           "Herz at 35m", "Cabauw at 20m", "Cabauw at 10m",
                           "Herz at 10m"),
           text.col=c("black", "violetred", "green", "purple2", "chocolate",
                      "deepskyblue", "red", "blue", "darkturquoise", "orange"), cex=0.9)

    dev.off()

  } else if ( t.obj$tower$data$StationName[1] == "Fino1" |
              t.obj$tower$data$StationName[1] == "Fino2") {

    yliml.rel = -0.3
    ylimh.rel = 0.3

    plot.ext = paste0(t.obj$tower$data$StationName[1], " at ",
                      t.obj$tower$data$height[1], "m ")

    mon.tower = vector(mode="numeric", length=12)
    towerXts = xts(t.obj$tower$data$wind_speed, order.by=tower.date)
    for (cnt in seq(12)) {
      mon.tower[cnt] = mean(towerXts[which( tower.date$mon==cnt-1 )])
    }

    Ylims = GetYlims(xts(mon.Era20c100, order.by=date.ancycle),
                     xts(mon.tower, order.by=date.ancycle),
                     xts(mon.Herz116, order.by=date.ancycle),
                     xts(mon.Herz116, order.by=date.ancycle))
    yliml = Ylims$yll
    ylimh = Ylims$ylh

    dummy = numeric(length=length(mon.Herz116)) * NA

    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)


    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", ylab="", main="")
    yliml = yliml - 2
    title(main=paste0("Annual cycle of wind speed at ",
                      t.obj$tower$data$StationName[1]), line=1, cex=1.1)
    title(ylab="wind speed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12, las=1)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh, las=1)
    lines(mon.tower, type="b", pch=16, col="blue",lw=2)
    lines(mon.Era20c100, type="b", pch=16, col="green",lw=2)
    lines(mon.Herz116, type="b", pch=16, col="red",lw=2)
    legend("top", legend=c(plot.ext, "Era20C at 100m ", "Herz at 116m"),
           text.col=c("blue", "green", "red"),
           cex=legend.cex)

    dev.off()


    fname = gsub("annualCycle", "annualCycleSingleArrow", fname)
    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", ylab="", main="")
    title(main=paste0("Annual cycle of wind speed at ",
                      t.obj$tower$data$StationName[1]),
          line=1, cex=1.1)
    title(ylab="wind speed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12, las=1)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh, las=1)

    lines(mon.Herz116, type="b", pch=16, col="violetred",lw=2)
    arrows(1:12, mon.Herz116-sd(mon.Herz116), 1:12, mon.Herz116+sd(mon.Herz116),
           length=0.05, angle=90, code=3, col="violetred")
    lines(mon.tower, type="b", pch=16, col="purple2",lw=2)
    arrows(1:12, mon.tower-sd(mon.tower), 1:12, mon.tower+sd(mon.tower),
           length=0.05, angle=90, code=3, col="purple2")
    lines(mon.Era20c100, type="b", pch=16, col="green",lw=2)
    arrows(1:12, mon.Era20c100-sd(mon.Era20c100), 1:12, mon.Era20c100+sd(mon.Era20c100),
           length=0.05, angle=90, code=3, col="green")
    lines(mon.Herz69, type="b", pch=16, col="chocolate",lw=2)
    lines(mon.Herz35, type="b", pch=16, col="red",lw=2)
    lines(mon.Herz10, type="b", pch=16, col="orange",lw=2)
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="orange")
    legend("top", legend=c("Herz at 116m", plot.ext, "Era20C at 100m",
                           "Herz at 69m", "Herz at 35m",
                           "Herz at 10m"),
           text.col=c("violetred", "purple2", "green", "red", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingleArrow", "annualCycleRelDiffSingle", fname)
    pdf(fname, width=land.a4width, height=land.a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.8)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         xlab="", ylab="", main="")
    title(main=paste0("Annual cycle of relative wind speed at ",
                      t.obj$tower$data$StationName[1]),
          line=1, cex=1.1, cex=3)
    title(ylab="relative difference", line=2, cex=1.8)
    axis(1, labels=all.months, at = 1:12, las=1, cex=1.8)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel), las=1)
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16,
          col="violetred",lw=2)
    lines(RelDiff(mon.tower, mean(mon.tower)), type="b", pch=16,
          col="purple2",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16,
          col="green",lw=2)
    lines(RelDiff(mon.Herz69, mean(mon.Herz69)), type="b", pch=16,
          col="chocolate",lw=2)
    lines(RelDiff(mon.Herz35, mean(mon.Herz35)), type="b", pch=16,
          col="red",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16,
          col="orange",lw=2)
    legend("top", legend=c("Herz at 116m", plot.ext, "Era20C at 100m",
                           "Herz at 69m", "Herz at 35m",
                           "Herz at 10m"),
           text.col=c("violetred", "purple2", "green", "chocolate", "red", "orange"),
           cex=0.9)

    dev.off()

  } else {
    CallStop(paste0("Unexpected tower name: ", t.obj$tower$data$StationName[1], " "))
  }
}

#-----------------------------------------------------------------------------------

#' @title Plot extreme value analysis
#' @description Plot different scores and skill scores of extreme value analysis.
#'   TODO: Not yet clear which plots of which scores and skill scores.
#'   The input to this function are a ClimObject holding measurements at one
#'   location (if tower also at different heights) and the corresponding reanalysis
#'   data (at different heights), the file name of the resulting plot, and a
#'   benchmark value against which is tested.
#'   TODO: the benchmark may be a vector holding a sequence of benchmarks in order
#'   to plot the dependence on its value.
#'   This function does not have a return value.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname string of the file name of the plot
#' @param benchmark numeric between 0 and 1 as a percentile
#'   as percentile ???
PlotTowerExtremes <- function(tower.obj, fname, benchmark) {

  t.obj = tower.obj$climate_data_objects
  obs = t.obj$tower$data$wind_speed
  forec = t.obj$herz10$data$wind_speed

  Cont.Table.cnt = CalcContTable(obs, forec, benchmark)
  scores = ContTableScores(Cont.Table.cnt$a, Cont.Table.cnt$b,
                           Cont.Table.cnt$c, Cont.Table.cnt$d)

  CallStop("and thanks for all the fish")
}

#-----------------------------------------------------------------------------------

#' @title Produce a scatter plot
#' @description Prodcue a scatter plot of two data set samples.
#' @param X first data sample for the scatter plot of class numeric
#' @param Y second data sample
#' @param yliml lower numeric bond of the y-axis limits
#' @param ylimh high numeric bond of the y-axis limits
#' @param titname string of the title name of the plot
#' @param xlabname string for the name of the x-axis label
#' @param ylabname string for the name of the y-axis label
#' @param text.str named variable of type string which will be written into the
#'   plot with \code{text}.
#' @param xaxis,yaxis are two named variables which set whether to plot the x- and
#'   y-axis (xaxis='s', yaxis='s') or omit either one (either set to 'n'). It not
#'   set the default value will be used which is set to 's' meaning that the axis
#'   will be plotted.
scatterPlot <- function(X, Y, yliml, ylimh, titname, xlabname, ylabname,
                        text.str=NULL, xaxis='s', yaxis='s') {

  if ((!xaxis=='n' & !xaxis=='s') & (!yaxis=='n' & !yaxis=='s')) {
    CallStop(paste0("Either xaxis or yaxis are not set as expected.\n",
                    "   The allowed values are 's' or 'n'\n",
                    "   xaxis = ", xaxis, " yaxis = ", yaxis))
  }

  plot(X, Y, pch=19,
       xlim=c(yliml,ylimh), ylim=c(yliml, ylimh),
       main=titname, xlab=xlabname, ylab=ylabname, col="blue",
       xaxt=xaxis, yaxt=yaxis)
  lines(c(yliml-1,ylimh), c(yliml-1,ylimh))
  abline(lm(Y ~ X), col="blue")
  if(!is.null(text.str)) {
    text(ceiling(0.1*(ylimh-yliml)), floor(0.9*(ylimh-yliml)),
         paste(text.str), adj=c(0, 0.5))
  }
}

#-----------------------------------------------------------------------------------

#' @title Produce one or two overlapping histogram plot(s)
#' @description Procudes a standard histogram plot of one or two data samples. If
#'   two data samples are to be plotted overlapping into one plot, then the same
#'   breaks are used and the y-limits of the plot are determined befor plotting.
#'   Density, rather than frequency, distribution is plotted.
#' @param X first data sample for the histogram
#' @param Y second sample; may be empty, NULL, NA, or anything else if
#'   addPlot is set to FALSE (see below)
#' @param breaks as described in the hist documentation
#' @param xlims limits of the x-axis in the format c(x,y)
#' @param titname string of the title name of the plot
#' @param xlabname string for the name of the x-axis label
#' @param ylabname string for the name of the y-axis label
#' @param addPlot optional boolean to determine whether to plot one (F) are two (T)
#'   data samples; default is to plot one data sample (addPlot=FALSE)
#' @param xaxis,yaxis are two named variables which set whether to plot the x- and
#'   y-axis (xaxis='s', yaxis='s') or omit either one (either set to 'n'). It not
#'   set the default value will be used which is set to 's' meaning that the axis
#'   will be plotted.
histoPlot <- function(X, Y, breaks, xlims, titname, xlabname, ylabname,
                      xaxis='s', yaxis='s', addPlot=FALSE) {

  if (addPlot) {
    # get high ylim for overplotting histograms
    hist1 = hist(X, breaks=breaks, plot=F)
    hist2 = hist(Y, breaks=breaks, plot=F)
    ylimh = ceiling(10.*max(hist1$density, hist2$density))/10.
    # plot both histograms
    hist(X, freq=F, breaks=breaks, xlim=xlims, ylim=c(0.0, ylimh),
         col="green", border="blue", main="", xlab="", ylab="",
         xaxt=xaxis, yaxt=yaxis)
    hist(Y, freq=F, add=T, breaks=breaks, border="blue", density=10, angle=45)
  } else {
    hist(X, freq=F, breaks=breaks, xlim=xlims, col="green", border="blue",
         main="", xlab="", ylab="", xaxt=xaxis, yaxt=yaxis)
    #     lines(density(X, na.rm=TRUE), col="red", lw=1.5)
  }
  if (xaxis == 's') {
    mtext(xlabname, side=1, line=2, cex=1.2)
  } else {
    mtext(xlabname, side=1, line=0, cex=1.2)
  }
  if(yaxis == 's') {
    mtext(ylabname, side=2, line=2, cex=1.2)
  } else {
    mtext(ylabname, side=2, line=0, cex=1.2)
  }
  mtext(titname, side=3, line=0, font=2, cex=1.1)
}

#-----------------------------------------------------------------------------------

#' @title Create QQ plot of two data samples
#' @description Create a simple Quantile-Quantile plot of two data samples.
#' @param X first data samples for the QQ plot
#' @param Y second data sample
#' @param yliml numeric low value of the y-limits of the plot
#' @param ylimh numeric high value of the y-limits
#' @param titname string of the title name
#' @param xlabname string for the name of the x-axis label
#' @param ylabname string for the name of the y-axis label
#' @param text.str named variable of type string which will be written into the
#'   plot with \code{text}.
#' @param xaxis,yaxis are two named variables which set whether to plot the x- and
#'   y-axis (xaxis='s', yaxis='s') or omit either one (either set to 'n'). It not
#'   set the default value will be used which is set to 's' meaning that the axis
#'   will be plotted.
qqPlot <- function(X, Y, yliml, ylimh,
                   titname, xlabname, ylabname,
                   text.str=NULL, xaxis='s', yaxis='s') {

  if ((!xaxis=='n' & !xaxis=='s') & (!yaxis=='n' & !yaxis=='s')) {
    CallStop(paste0("Either xaxis or yaxis are not set as expected.\n",
                    "   The allowed values are 's' or 'n'\n",
                    "   xaxis = ", xaxis, " yaxis = ", yaxis))
  }

  qqplot(X, Y, pch=19,
         xlim=c(yliml-1,ylimh), ylim=c(yliml-1, ylimh),
         main=titname, xlab=xlabname, ylab=ylabname,
         xaxt=xaxis, yaxt=yaxis)
  abline(0,1)
  if(!is.null(text.str)) {
    text(ceiling(0.1*(ylimh-yliml)), floor(0.9*(ylimh-yliml)),
         paste(text.str), adj=c(0, 0.5))
  }
}

#-----------------------------------------------------------------------------------

#' @title Plot legend with statistics for histogram plot.
#' @description Plot the legend for histogram plots with statistics (mean, median,
#'   etc) and weibull parameters which are calculated.
#' @param xlims the limits of the x-axis in the form c(low,high)
#' @param vals vector of the plotted values of which the statistics are calculated
plotLegendStats <- function(xlims, vals) {

  weibull = TRUE
  if (any(!is.finite(vals))) weibull = FALSE
  if (weibull) {
    daily.weibull <- fitdist(vals, distr="weibull")
    SshapeCI <- daily.weibull$estimate[1] + c(-1.96, 1.96) *
      sqrt(vcov(daily.weibull)["shape", "shape"])
    SscaleCI <- daily.weibull$estimate[2] + c(-1.96, 1.96) *
      sqrt(vcov(daily.weibull)["scale", "scale"])
    x <- seq(xlims[1], xlims[2], by = 0.01)
    y <- dweibull(x, shape = daily.weibull$estimate[1], scale = daily.weibull$estimate[2])
    lines(y ~ x, lwd = 1, col = "red", type = "l")
  }

  if (weibull) {
    legend("topright", bty = "n",
           legend = c(paste0("n = ", length(vals), " d",
                             "\nmean = ", round(mean(vals, na.rm = TRUE), 2),
                             "\nmedian = ", round(median(vals, na.rm = TRUE), 2),
                             "\n1% = ", round(quantile(vals, 0.01, na.rm = TRUE), 2),
                             "\n99% = ", round(quantile(vals, 0.99, na.rm = TRUE), 2)),
                      paste0("Weibull fit with", "\n  k =",
                             format(round(daily.weibull$estimate[1], 2)),
                             " (", round(SshapeCI[1],2), "-", round(SshapeCI[2], 2), ")",
                             "\n  c =", format(round(daily.weibull$estimate[2],2)),
                             " (", round(SscaleCI[1], 2), "-", round(SscaleCI[2],2), ")")),
           cex = 0.8)
  } else {
    legend("topright", bty = "n",
           legend = c(paste0("n = ", length(vals), " d"),
                      paste0("mean = ", round(mean(vals, na.rm = TRUE), 2)),
                      paste0("median = ", round(median(vals, na.rm = TRUE), 2)),
                      paste0("1% = ", round(quantile(vals, 0.01, na.rm = TRUE), 2)),
                      paste0("99% = ", round(quantile(vals, 0.99, na.rm = TRUE), 2))),
           cex = 0.8)
  }
}
