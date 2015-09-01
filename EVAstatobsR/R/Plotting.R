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
#' @param width,height of the plot in inches
#' @param monthly is an optional parameter which determines to plot the monthly
#'   values of the above time series
#' @param anomaly is an optional parameter which determines whether to plot anomalies
PlotStationEra <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                           titname, outdir, fname, width, height,
                           monthly=TRUE, anomaly=FALSE) {

  roll.mean = TRUE
  roll.time = 12

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
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
  plot(dummy, main=titname, ylab="windspeed [m/s]", ylim=c(yliml, ylimh))

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
#' @param width,height of the plot in inches
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
                           width, height, length.plot,
                           era.months, plot.diff=FALSE) {

  if (era.months) { # months will be plotted
    all.months = c("January","February","March","April","May","June","July",
                   "August","September","October","November","December")
  } else { # seasons will be plotted
    all.seasons = c("Winter", "Spring", "Summer", "Autumn")
  }

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
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
  mtext("windspeed [m/s]", side=2, line=3, outer=TRUE)

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
#' @param width,height of the plot in inches
#' @param seasons is an optional parameter which determines whetther to plot the
#'   monthly (F) or seasonal (T) values of the above time series
#' @param anomaly is an optional parameter which determines whether to plot anomalies
PlotStationEraSelSeasons <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                     titname, outdir, fname, width, height,
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
                 width, height, seasons, era.months=FALSE)

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
                   width, height, seasons, era.months=FALSE, station.diff)
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
#' @param width,height of the plot in inches
#' @param anomaly is an optional parameter which determines whether to plot anomalies
#' @note need to adopt titname to months; need to plot into four different panals
PlotStationEraSelMonths <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                    titname, outdir, fname, width, height,
                                    anomaly=FALSE) {

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
                 width, height, months, era.months=TRUE)

  station.diff = TRUE
  if (station.diff & !anomaly) {
    for (cnt in seq(12)) {
      mon.Era20c[[cnt]] = mon.Era20c[[cnt]] - mon.Stat[[cnt]]
      mon.EraI[[cnt]] = mon.EraI[[cnt]] - mon.Stat[[cnt]]
      mon.Herz[[cnt]] = mon.Herz[[cnt]] - mon.Stat[[cnt]]
      mon.Stat[[cnt]] = mon.Stat[[cnt]] - mon.Stat[[cnt]]
    }
    fname = gsub(".pdf", "_diff.pdf", fname)
    titname = gsub("Windspeed", "Monthly difference in windspeed", titname)
    PlotMultiPanel(outdir, fname, titname,
                   mon.Era20c, mon.EraI, mon.Herz, mon.Stat,
                   width, height, months, era.months=TRUE, station.diff)
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
#' @param width,height of the plot in inches
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
PlotStationEraSQ <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                             titname, outdir, fname, width, height,
                             era.monthly=FALSE) {

  Era20  = as.numeric(Era20cXts)
  EraI = as.numeric(EraIXts)
  Herz = as.numeric(HerzXts)
  Stat = as.numeric(StatXts)

  Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  axis.n = 'n'
  axis.y = 's'
  mtext.titname = "Daily windspeed at 10m height"
  if (era.monthly) {
    mtext.titname = "Monthly windspeed at 10m height"
  }

  fname.scatter = gsub(".pdf", "_scatterQQ-Plots.pdf", fname)
  pdf(paste0(outdir, fname.scatter), width=height, height=width,
      onefile=TRUE, pointsize=13)

  par(mfrow=c(3,2), mar=c(0,0,0,0), oma=c(5,5,4,0.5))

  xlabname = "10m ERA20C windspeed [m/s]"
  ylabname = "10m ERA-I windspeed [m/s]"
  if (era.monthly) {
    titname.scatter = gsub("windspeed", "Scatter and QQ-plot of monthly windspeed",
                           titname)
  } else {
    titname.scatter = gsub("windspeed", "Scatter and QQ-plot of daily windspeed",
                           titname)
  }
  text.str = "Era20c vs ERA-Interim"
  scatterPlot(Era20, EraI, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(Era20, EraI, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m ERA20C windspeed [m/s]"
  ylabname = "10m HErZ windspeed [m/s]"
  text.str = "Era20c vs HErZ"
  scatterPlot(Era20, Herz, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(Era20, Herz, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m ERA-I windspeed [m/s]"
  ylabname = "10m HErZ windspeed [m/s]"
  text.str = "ERA-Interim vs HErZ"
  scatterPlot(EraI, Herz, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.y)
  qqPlot(EraI, Herz, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.n)

  mtext(titname.scatter, line=1, outer=TRUE)
  mtext("windspeed [m/s]", side=2, line=3, outer=TRUE)
  mtext("windspeed [m/s]", side=1, line=3, outer=TRUE)

  xlabname = "10m ERA20C windspeed [m/s]"
  ylabname = "10m Station windspeed [m/s]"
  text.str = "ERA20C vs station data"
  scatterPlot(Era20, Stat, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(Era20, Stat, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m ERA-I windspeed [m/s]"
  ylabname = "10m Station windspeed [m/s]"
  text.str = "ERA-Interim vs station data"
  scatterPlot(EraI, Stat, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  qqPlot(EraI, Stat, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.n)

  xlabname = "10m HErZ windspeed [m/s]"
  ylabname = "10m Station windspeed [m/s]"
  text.str = "HErZ vs station data"
  scatterPlot(Herz, Stat, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.y)
  qqPlot(Herz, Stat, yliml, ylimh, "",
         xlabname, ylabname, text.str=text.str, xaxis=axis.y, yaxis=axis.n)

  mtext(titname.scatter, line=1, outer=TRUE)
  mtext("windspeed [m/s]", side=2, line=3, outer=TRUE)
  mtext("windspeed [m/s]", side=1, line=3, outer=TRUE)

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
#' @param width,height of the plot in inches
Plot100mEraHerz <- function(Era20cXts, HerzXts,
                            titname, statname, outdir, fname,
                            era.monthly, width, height) {

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

  pdf(paste0(outdir, fname), width=width, height=height,
      onefile=TRUE, pointsize=13)

  dummy = numeric(length=length(Era20cXts)) * NA
  dummy = xts(dummy, order.by = index(Era20cXts))

  Ylims = GetYlims(Era20cXts, HerzXts, dummy, dummy)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  plot(dummy, main=titname, ylab="windspeed [m/s]", ylim=c(yliml, ylimh))

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

    xlabname = "100m ERA20C windspeed [m/s]"
    ylabname = "116m HErZ windspeed [m/s]"
    text.str = ""

    scatterPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname,
                text.str=text.str)

    titname = "Quantile-quantile plot"
    qqPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname, text.str=text.str)

    min.val = floor(min(min(Era, na.rm=TRUE), min(Herz, na.rm=TRUE)))
    max.val = ceiling(max(max(Era, na.rm=TRUE), max(Herz, na.rm=TRUE)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(Era)) * NA

    titname = paste0("Frequency distribution of ", titname.ext, " windspeed\nof ",
                     "ERA20C at 100m")
    ylabname = "Density"
    histoPlot(Era, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = paste0("Frequency distribution of ", titname.ext, " windspeed\nof ",
                     "COSMO HErZ at 116m")
    xlabname = ylabname
    histoPlot(Herz, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = paste0("Frequency distribution of ", titname.ext,
                     " ERA20C windspeed at 100m\n",
                     "in green and COSMO HErZ at 116m shaded")
    xlabname = "windspeed [m/s]"
    histoPlot(Era, Herz, breaks, xlims=c(min.val, max.val),
              titname, xlabname, ylabname, addPlot=T)

  }
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title
#' @description
#' @param
PlotHistograms <- function(outdir, fname, station.name, era.monthly, width, height,
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
  xlabname.full = "10m windspeed [m/s]"
  ylabname = "Density"

  if (plot.10m) {
    if (plot.cnt != 4 & plot.cnt != 6 & plot.cnt != 10) {
      CallStop(paste0("Depending on data to plot I expect 4, 6, or 10 plots to be ",
                      "plotted;\n", "   plot.cnt = ", plot.cnt, " for plot.10m: ",
                      plot.10m, ", plot.10m100m: ", plot.10m100m,
                      ", and plot.HerzProfile: ", plot.HerzProfile))
    }

    mtext.titname = "Daily windspeed at 10m height"
    if (era.monthly) {
      mtext.titname = "Monthly windspeed at 10m height"
    }

    fname = gsub('Histogram', 'Histogram_ERA-Station-10m', fname)
    pdf(paste0(outdir, fname), width=width, height=height,
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
    xlabname = "10m station windspeed [m/s]"
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

    mtext.titname = "Daily windspeed at 100m height"
    if (era.monthly) {
      mtext.titname = "Monthly windspeed at 100m height"
    }

    fname = gsub('Histogram', 'Histogram_ERA20C-HErZ-100m', fname)
    pdf(paste0(outdir, fname), width=width, height=height,
        onefile=TRUE, pointsize=13)

    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    min.val = floor(min(min(era20c100, na.rm=TRUE), min(herz116, na.rm=TRUE)))
    max.val = ceiling(max(max(era20c100, na.rm=TRUE), max(herz116, na.rm=TRUE)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(era20c100)) * NA

    titname = paste0("Frequency distribution of 100m ", monthly.ext,
                     " ERA20C windspeed")
    xlabname = "100m ERA20c windspeed [m/s]"
    histoPlot(era20c100, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 116m COSMO HErZ windspeed")
    xlabname = "116m HErZ windspeed [m/s]"
    histoPlot(herz116, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of ", monthly.ext, " ERA20C windspeed",
                     " at 100m\n", "in green and COSMO HErZ at 116m shaded")
    xlabname = "windspeed [m/s]"
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

    mtext.titname = "Daily windspeed of HErZ profile"
    if (era.monthly) {
      mtext.titname = "Monthly windspeed of HErZ profile"
    }

    fname = gsub('Histogram', 'Histogram_HErZ-Profile', fname)
    pdf(paste0(outdir, fname), width=width, height=height,
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

    titname = paste0("Frequency distribution of 10m ", monthly.ext, " HErZ windspeed")
    xlabname = "10m HErZ windspeed [m/s]"
    histoPlot(herz10, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 35m ", monthly.ext, " HErZ windspeed")
    xlabname = "35m HErZ windspeed [m/s]"
    histoPlot(herz35, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 69m ", monthly.ext, " HErZ windspeed")
    xlabname = "69m HErZ windspeed [m/s]"
    histoPlot(herz69, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 116m ", monthly.ext, " HErZ windspeed")
    xlabname = "116m HErZ windspeed [m/s]"
    histoPlot(herz116, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    titname = paste0("Frequency distribution of 178m ", monthly.ext, " HErZ windspeed")
    xlabname = "178m HErZ windspeed [m/s]"
    histoPlot(herz178, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    titname = paste0("Frequency distribution of 258m ", monthly.ext, " HErZ windspeed")
    xlabname = "258m HErZ windspeed [m/s]"
    histoPlot(herz258, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

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
#' @param width,height of the plot in inches
#' @param era.monthly is an optional boolean which determines whether data passed
#'   is monthly (T) or daily (F) data. The default value is to use daily data
#'   (era.monthly=FALSE).
PlotPDFScore <- function(era.xts, station.xts, outdir, fname, titname,
                         width, height, era.monthly=FALSE) {

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
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

    xlabname = "windspeed [m/s]"
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
  mtext(paste0("windspeed [m/s]"), side=1, line=2)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title
#' @description
#' @param
PlotTowerERAprofileBP <- function(tower.df, tower.name, fname, era.mon) {

  dummy = numeric(length=length(tower.df$herz10)) * NA
  x.labs = "windspeed [m/s]"
  swex = 0.4
  bwex = 0.3
  hori=TRUE
  nch = TRUE
  oline = FALSE
  if (era.mon) {wind.range = c(0,15)}
  else {wind.range = c(0,20)}

  if (tower.name == "Lindenberg") {
    pdf(fname, width=a4height, height=a4width, onefile=TRUE, pointsize=13)
    par(mar=c(4,7,2,0.5))
    boxplot.default(tower.df$Lind10, tower.df$herz10, tower.df$Lind20, tower.df$herz35,
                    tower.df$Lind40, tower.df$Lind60, tower.df$herz69, tower.df$Lind80,
                    tower.df$Lind98, tower.df$era20c100, tower.df$herz116,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("Lind 10m", "HErZ 10m", "Lind 20m", "HErZ 35m",
                            "Lind 40m", "Lind 60m", "HErZ 69m", "Lind 80m",
                            "Lind 98m", "ERA20C 100m", "HErZ 116m"),
                    main="Boxplots of Lindenberg and HErZ profile",
                    col=c("red", "blue", "red", "blue", "red", "red",
                          "blue", "red", "red", "green", "blue"))
    mtext(x.labs, side=1, line=2)
    dev.off()
  } else if (tower.name == "Fino1") {
    pdf(fname, width=a4height, height=a4width, onefile=TRUE, pointsize=13)
    par(mar=c(4,7,2,0.5))
    boxplot.default(dummy, tower.df$herz10, dummy, tower.df$herz35, dummy, dummy,
                    tower.df$herz69, dummy, tower.df$era20c100, tower.df$Fino1,
                    tower.df$herz116,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("", "HErZ 10m", "", "HErZ 35m", "", "", "HErZ 69m",
                            "", "ERA20C 100m", "Fino1 100m", "HErZ 116m"),
                    main="Boxplots of Fino1 at 100m and HErZ profile",
                    col=c("", "blue", "", "blue", "", "", "blue", "", "green",
                          "red", "blue"))
    mtext(x.labs, side=1, line=2)
    dev.off()
  } else if (tower.name == "Fino2") {
    pdf(fname, width=a4height, height=a4width, onefile=TRUE, pointsize=13)
    par(mar=c(4,7,2,0.5))
    boxplot.default(dummy, tower.df$herz10, dummy, tower.df$herz35, dummy, dummy,
                    tower.df$herz69, dummy, tower.df$era20c100, tower.df$Fino2,
                    tower.df$herz116,
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                    names=c("", "HErZ 10m", "", "HErZ 35m", "", "", "HErZ 69m",
                            "", "ERA20C 100m", "Fino2 102m", "HErZ 116m"),
                    main="Boxplots of Fino2 at 102m and HErZ profile",
                    col=c("", "blue", "", "blue", "", "", "blue", "", "green",
                          "red", "blue"))
    mtext(x.labs, side=1, line=2)
    dev.off()
  } else {
    CallStop(paste0("Unexpected tower name: ", tower.name, " "))
  }
}

#-----------------------------------------------------------------------------------

#' @title
#' @description
#' @param
PlotTowerERAprofileRelDiff <- function(tower.df, tower.name, fname) {

  legend.cex = 0.9
  tower.date <- as.POSIXlt(tower.df$date)

  yliml.rel = -0.75
  ylimh.rel = 0.75
  color = list(tower="blue", herz="red", era20="green", black="black")

  if (tower.name == "Lindenberg") {

    pdf(fname, width=a4height, height=a4width, onefile=TRUE, pointsize=13)
    par(mfrow=c(3,1), mar=c(0,4,0,0), oma=c(4,0,3,0.5), cex=1.1)

    # == relative TS in 100m height ==
    dummy = numeric(length=length(tower.date)) * NA
    dummy = xts(dummy, order.by=tower.date)

    h.xts = xts(tower.df$herz116, order.by=tower.date)
    l.xts = xts(tower.df$Lind98, order.by=tower.date)
    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml.rel, ylimh.rel), las=1)
    title(ylab="relative difference", line=2.5)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    legend("topleft", legend=c("HErZ at 116m", "Lindenberg at 98m",
                               paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$herz, color$tower, color$black))


    h.xts = xts(tower.df$era20c100, order.by=tower.date)
    l.xts = xts(tower.df$Lind98, order.by=tower.date)
    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml.rel, ylimh.rel), las=1)
    title(ylab="relative difference", line=2.5)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$era20)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    legend("topleft", legend=c("ER20C at 100m", "Lindenberg at 98m",
                               paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$era20, color$tower, color$black))

    h.xts = xts(tower.df$herz10, order.by=tower.date)
    l.xts = xts(tower.df$Lind10, order.by=tower.date)
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), las=1)
    title(ylab="relative difference", line=2.5)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    legend("topleft", legend=c("HErZ at 10m", "Lindenberg at 10m",
                               paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$herz, color$tower, color$black))

    mtext("Monthly relative differences of Lindenberg against HErZ and ERA20C",
          outer=TRUE, line=1, cex=1.2)

    dev.off()

  } else if (tower.name == "Fino1" | tower.name == "Fino2") {

    if (tower.name == "Fino1") plot.ext = "Fino1 at 100m "
    if (tower.name == "Fino2") plot.ext = "Fino2 at 102m "

    pdf(fname, width=a4height, height=a4width, onefile=TRUE, pointsize=13)
    par(mfrow=c(3,1), mar=c(0,4,0,0), oma=c(4,0,3,0.5), cex=1.1)

    # == absolute, relative and normalized TS of each height ==
    dummy = numeric(length=length(tower.date)) * NA
    dummy = xts(dummy, order.by=tower.date)

    h.xts = xts(tower.df$herz116, order.by=tower.date)
    if (tower.name == "Fino1") l.xts = xts(tower.df$Fino1, order.by=tower.date)
    if (tower.name == "Fino2") l.xts = xts(tower.df$Fino2, order.by=tower.date)
    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml.rel, ylimh.rel), las=1)
    title(ylab="relative difference", line=2.5)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    legend("topleft", legend=c("HErZ at 116m", plot.ext,
                               paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$herz, color$tower, color$black))


    h.xts = xts(tower.df$era20c100, order.by=tower.date)
    if (tower.name == "Fino1") l.xts = xts(tower.df$Fino1, order.by=tower.date)
    if (tower.name == "Fino2") l.xts = xts(tower.df$Fino2, order.by=tower.date)
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), las=1)
    title(ylab="relative difference", line=2.5)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$era20)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$tower)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    legend("topleft", legend=c("ER20C at 100m", plot.ext,
                               paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$era20, color$tower, color$black))

    mtext(paste0("Monthly relative differences of ", tower.name,
                 " at 100m against HErZ and ERA20C"), outer=TRUE, line=1, cex=1.2)

    dev.off()

  } else {
    CallStopp(paste0("Unexpected tower.name: ", tower.name, " "))
  }
}

#-----------------------------------------------------------------------------------

PlotTowerERAprofileAnnualVar <- function(tower.df, tower.name, fname) {

  legend.cex = 0.9
  tower.date <- as.POSIXlt(tower.df$date)

  months=list(1,8)
  all.months = c("January","February","March","April","May","June","July",
                 "August","September","October","November","December")

  colorH = c("red", "black")
  colorE = c("green", "black")
  colorT = c("blue", "black")

  mon.Era20c100 = list()
  mon.Herz116 = list()
  mon.Herz10 = list()

  Era20c100Xts = xts(tower.df$era20c100, order.by=tower.date)
  Herz116Xts = xts(tower.df$herz116, order.by=tower.date)
  Herz10Xts = xts(tower.df$herz10, order.by=tower.date)

  for (cnt in seq(12)) {
    mon.Era20c100[[cnt]] = Era20c100Xts[which( tower.date$mon==cnt-1 )]
    mon.Herz116[[cnt]] = Herz116Xts[which( tower.date$mon==cnt-1 )]
    mon.Herz10[[cnt]] = Herz10Xts[which( tower.date$mon==cnt-1 )]
  }

  if (tower.name == "Lindenberg") {

    mon.Lind98 = list()
    mon.Lind10 = list()
    Lind98Xts = xts(tower.df$Lind98, order.by=tower.date)
    Lind10Xts = xts(tower.df$Lind10, order.by=tower.date)
    for (cnt in seq(12)) {
      mon.Lind98[[cnt]] = Lind98Xts[which( tower.date$mon==cnt-1 )]
      mon.Lind10[[cnt]] = Lind10Xts[which( tower.date$mon==cnt-1 )]
    }

    yliml = vector(mode="numeric", length=length(months))
    ylimh = vector(mode="numeric", length=length(months))
    for (cnt in seq(months)) {
      Ylims = GetYlims(mon.Era20c100[[months[[cnt]]]], mon.Herz116[[months[[cnt]]]],
                       mon.Lind10[[months[[cnt]]]], mon.Herz10[[months[[cnt]]]])
      yliml[cnt] = Ylims$yll
      ylimh[cnt] = Ylims$ylh
    }
    yliml = 0 #min(yliml)
    ylimh = max(ylimh)

    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mfrow=c(5,1), mar=c(0,2,0,0), oma=c(4,1,3,0.5), cex=1.1)

    dummy = numeric(length=length(Era20c100Xts)) * NA
    dummy = xts(dummy, order.by = index(Era20c100Xts))

    plot(dummy, main=NULL, xaxt="n", ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Lind98[[months[[cnt]]]], type="b", pch=16, col=colorT[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0("Lind at 98m ", all.months[months[[1]]]),
                                  paste0("Lind at 98m ", all.months[months[[2]]])),
           text.col=colorT, cex=legend.cex)

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
      lines(mon.Lind10[[months[[cnt]]]], type="b", pch=16, col=colorT[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("topleft", legend=c(paste0("Lind at 10m ", all.months[months[[1]]]),
                               paste0("Lind at 10m ", all.months[months[[2]]])),
           text.col=colorT, cex=legend.cex)

    plot(dummy, main=NULL, ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Herz10[[months[[cnt]]]], type="b", pch=16, col=colorH[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("topleft", legend=c(paste0("HErZ at 10m ", all.months[months[[1]]]),
                               paste0("HErZ at 10m ", all.months[months[[2]]])),
           text.col=colorH, cex=legend.cex)

    mtext("Windspeed at Lindenberg for different months",
          outer=TRUE, line=1, cex=1.2)
    mtext("windspeed [m/s]", line=0, side=2, outer=T)

    dev.off()

  } else if (tower.name == "Fino1" | tower.name == "Fino2") {

    if (tower.name == "Fino1") plot.ext = "Fino1 at 100m "
    if (tower.name == "Fino2") plot.ext = "Fino2 at 102m "

    mon.tower = list()
    if (tower.name == "Fino1") {
      towerXts = xts(tower.df$Fino1, order.by=tower.date)
    } else {
      towerXts = xts(tower.df$Fino2, order.by=tower.date)
    }

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

    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mfrow=c(3,1), mar=c(0,4,0,0), oma=c(4,1,3,0.5), cex=1.1)

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

    mtext(paste0("Windspeed at ", tower.name, " at 100m for different months"),
          outer=TRUE, line=1, cex=1.2)
    mtext("windspeed [m/s]", line=0, side=2, outer=TRUE)

    dev.off()

  } else {
    CallStopp(paste0("Unexpected tower.name: ", tower.name, " "))
  }
}


#-----------------------------------------------------------------------------------

PlotTowerERAprofileAnnualCycle <- function(tower.df, tower.name, fname) {

  legend.cex = 0.9
  tower.date <- as.POSIXlt(tower.df$date)

  all.months = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                 "Aug","Sep","Oct","Nov","Dec")

  date.ancycle = as.yearmon(2000 + seq(0, 11)/12)

  mon.Era20c100 = vector(mode="numeric", length=12)
  mon.Herz116 = vector(mode="numeric", length=12)
  mon.Herz69 = vector(mode="numeric", length=12)
  mon.Herz35 = vector(mode="numeric", length=12)
  mon.Herz10 = vector(mode="numeric", length=12)
  Era20c100Xts = xts(tower.df$era20c100, order.by=tower.date)
  Herz116Xts = xts(tower.df$herz116, order.by=tower.date)
  Herz69Xts = xts(tower.df$herz69, order.by=tower.date)
  Herz35Xts = xts(tower.df$herz35, order.by=tower.date)
  Herz10Xts = xts(tower.df$herz10, order.by=tower.date)

  for (cnt in seq(12)) {
    mon.Era20c100[cnt] = mean(Era20c100Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz116[cnt] = mean(Herz116Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz69[cnt] = mean(Herz69Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz35[cnt] = mean(Herz35Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz10[cnt] = mean(Herz10Xts[which( tower.date$mon==cnt-1 )])
  }

  if (tower.name == "Lindenberg") {

    yliml.rel = -0.2
    ylimh.rel = 0.2

    mon.Lind98 = vector(mode="numeric", length=12)
    mon.Lind60 = vector(mode="numeric", length=12)
    mon.Lind40 = vector(mode="numeric", length=12)
    mon.Lind10 = vector(mode="numeric", length=12)
    Lind98Xts = xts(tower.df$Lind98, order.by=tower.date)
    Lind60Xts = xts(tower.df$Lind60, order.by=tower.date)
    Lind40Xts = xts(tower.df$Lind40, order.by=tower.date)
    Lind10Xts = xts(tower.df$Lind10, order.by=tower.date)
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

    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mfrow=c(4,1), oma=c(3,3,3,0.5), mar=c(0,0,0,0), cex=1.1)
    # !!! col.axis = "white" is a hack to suppress the tick labels
    # !!! in order to set them manually
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    title("Annual cycle of windspeed at Lindenberg", outer=TRUE, line=1, cex=1.1)
    title(ylab="windspeed [m/s]", outer=TRUE, line=2)
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind98, type="b", pch=16, col="blue", lw=2)
    lines(mon.Era20c100, type="b", pch=16, col="green", lw=2)
    lines(mon.Herz116, type="b", pch=16, col="red", lw=2)
    # corr1 = cor.test(as.numeric(Era20c100Xts), as.numeric(Lind98Xts))
    # corr2 = cor.test(as.numeric(Herz116Xts), as.numeric(Lind98Xts))
    # legend("top", legend=c("Lind at 98m ", paste0("Era20C at 100m with corr = ",
    #                                         round(corr1$estimate, 2)),
    #          paste0("Herz at 116m with corr = ",round(corr2$estimate, 2))),
    #    text.col=c("blue", "green", "red"), cex=legend.cex)
    legend("top", legend=c("Lind at 98m ", "Era20C at 100m", "Herz at 116m"),
           text.col=c("blue", "green", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         las=1, xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind60, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz69, type="b", pch=16, col="red", lw=2)
    legend("top", legend=c("Lind at 60m", "Herz at 69m"),
           text.col=c("blue", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         las=1, xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind40, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz35, type="b", pch=16, col="red", lw=2)
    legend("top", legend=c("Lind at 40m", "Herz at 35m"),
           text.col=c("blue", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         las=1, xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind10, type="b", pch=16, col="cornflowerblue", lw=2)
    lines(mon.Herz10, type="b", pch=16, col="coral1", lw=2)
    legend("top", legend=c("Lind at 10m", "Herz at 10m"),
           text.col=c("blue", "red"), cex=legend.cex)

    axis(1, labels=all.months, at = 1:12)

    dev.off()


    fname = gsub("annualCycle", "annualCycleSingle", fname)
    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel,ylimh.rel), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    title(main="Annual cycle of relative windspeed at Lindenberg", line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel))
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16, col="violetred",lw=2)
    lines(RelDiff(mon.Lind98, mean(mon.Lind98)), type="b", pch=16, col="purple2",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16, col="green",lw=2)
    lines(RelDiff(mon.Herz69, mean(mon.Herz69)), type="b", pch=16, col="chocolate",lw=2)
    lines(RelDiff(mon.Lind60, mean(mon.Lind60)), type="b", pch=16, col="deepskyblue",lw=2)
    lines(RelDiff(mon.Lind40, mean(mon.Lind40)), type="b", pch=16, col="blue",lw=2)
    lines(RelDiff(mon.Herz35, mean(mon.Herz35)), type="b", pch=16, col="red",lw=2)
    lines(RelDiff(mon.Lind10, mean(mon.Lind10)), type="b", pch=16, col="darkturquoise",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16, col="orange",lw=2)
    legend("top", legend=c("Herz at 116m", "Lind at 98m ", "Era20C at 100m",
                           "Herz at 69m", "Lind at 60m", "Lind at 40m", "Herz at 35m",
                           "Lind at 10m", "Herz at 10m"),
           text.col=c("violetred", "purple2", "green", "chocolate", "deepskyblue",
                      "blue", "red", "darkturquoise", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingle", "annualCycleSingleArrow", fname)
    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    title(main="Annual Cycle of windspeed at Lindenberg", line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh)

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
    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    title(main="Annual cycle of relative windspeed at Lindenberg", line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel))
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

  } else if ( tower.name == "Fino1" | tower.name == "Fino2") {

    yliml.rel = -0.3
    ylimh.rel = 0.3

    if (tower.name == "Fino1") plot.ext = "Fino1 at 100m "
    if (tower.name == "Fino2") plot.ext = "Fino2 at 102m "

    mon.tower = vector(mode="numeric", length=12)
    if (tower.name == "Fino1") {
      towerXts = xts(tower.df$Fino1, order.by=tower.date)
    } else {
      towerXts = xts(tower.df$Fino2, order.by=tower.date)
    }
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

    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)


    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    yliml = yliml - 2
    title(main=paste0("Annual cycle of windspeed at ", tower.name), line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh)
    lines(mon.tower, type="b", pch=16, col="blue",lw=2)
    lines(mon.Era20c100, type="b", pch=16, col="green",lw=2)
    lines(mon.Herz116, type="b", pch=16, col="red",lw=2)
    legend("top", legend=c(plot.ext, "Era20C at 100m ", "Herz at 116m"),
           text.col=c("blue", "green", "red"),
           cex=legend.cex)

    dev.off()


    fname = gsub("annualCycle", "annualCycleSingle", fname)
    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel,ylimh.rel), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    title(main=paste0("Annual cycle of relative windspeed at ", tower.name), line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel))
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16, col="violetred",lw=2)
    lines(RelDiff(mon.tower, mean(mon.tower)), type="b", pch=16, col="purple2",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16, col="green",lw=2)
    lines(RelDiff(mon.Herz69, mean(mon.Herz69)), type="b", pch=16, col="chocolate",lw=2)
    lines(RelDiff(mon.Herz35, mean(mon.Herz35)), type="b", pch=16, col="red",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16, col="orange",lw=2)
    legend("top", legend=c("Herz at 116m", plot.ext, "Era20C at 100m",
                           "Herz at 69m", "Herz at 35m", "Herz at 10m"),
           text.col=c("violetred", "purple2", "green", "chocolate",  "red", "orange"),
           cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingle", "annualCycleSingleArrow", fname)
    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    title(main=paste0("Annual Cycle of windspeed at ", tower.name), line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh)

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
    pdf(fname, width=a4width, height=a4height, onefile=TRUE, pointsize=13)
    par(mar=c(3,3,3,0.5), cex=1.1)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         las=1, xlab="", ylab="", main="")
    title(main=paste0("Annual cycle of relative windspeed at ", tower.name), line=1, cex=1.1)
    title(ylab="windspeed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel))
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
           text.col=c("violetred", "purple2", "green", "chocolate", "red", "orange"), cex=0.9)

    dev.off()

  } else {
    CallStopp(paste0("Unexpected tower.name: ", tower.name, " "))
  }
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
    lines(density(X, na.rm=TRUE), col="red", lw=1.5)
  }
  if (xaxis == 's') {
    mtext(xlabname, side=1, line=2, cex=0.9)
  } else {
    mtext(xlabname, side=1, line=0, cex=0.9)
  }
  if(yaxis == 's') {
    mtext(ylabname, side=2, line=2, cex=0.9)
  } else {
    mtext(ylabname, side=2, line=0, cex=0.9)
  }
  mtext(titname, side=3, line=0, font=2, cex=1.)
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
