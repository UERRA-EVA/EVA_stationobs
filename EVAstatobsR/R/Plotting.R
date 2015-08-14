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
    err = simpleError(paste0("\n   ***\n   \nXTS1 or XTS2 or XTS3 or XTS4 ",
                             "is not an xts, ABORTING!\n   ***\n"))
    tryCatch(stop(err))
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

  par(mfrow=c(2,2))
  par(mar=c(0,0,0,0), oma=c(3,5,3,0.5))
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

#' @title Prepare and plot daily time series of station data and ERA20C, ERA-I,
#'   and HErZ data.
#' @description This function plots daily station and reanalysis data.
#' @param Era20cXts daily mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts daily mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot/s is/are saved
#' @param fname string of the output file name
#' @param width,height of the plot in inches
#' @note
PlotStationEraDaily <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                titname, outdir, fname, width, height) {

  Era20  = as.numeric(Era20cXts)
  EraI = as.numeric(EraIXts)
  Herz = as.numeric(HerzXts)
  Stat = as.numeric(StatXts)

  Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  axis.n = 'n'
  axis.y = 's'

  fname.scatter = gsub(".pdf", "_scatterQQ-Plots.pdf", fname)
  pdf(paste0(outdir, fname.scatter), width=height, height=width,
      onefile=TRUE, pointsize=13)

  par(mfrow=c(3,2))
  par(mar=c(0,0,0,0), oma=c(5,5,4,0.5))

  xlabname = "10m ERA20C windspeed [m/s]"
  ylabname = "10m ERA-I windspeed [m/s]"
  titname.scatter = gsub("Daily", "Scatter plot of daily", titname)
  text.str = "Era20c vs ERA-Interim"
  scatterPlot(Era20, EraI, yliml, ylimh, "",
              xlabname, ylabname, text.str=text.str, xaxis=axis.n, yaxis=axis.y)
  titname.qq = gsub("Daily", "Quantile-quantile plot of daily", titname)
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


  fname.histo = gsub(".pdf", "_histoPlots.pdf", fname)
  pdf(paste0(outdir, fname.histo), width=width, height=height,
      onefile=TRUE, pointsize=13)

  par(mfrow=c(2,2))
  par(mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

  min.val = floor(min(min(Era20), min(EraI), min(Herz), min(Stat)))
  max.val = ceiling(max(max(Era20), max(EraI), max(Herz), max(Stat)))
  breaks = seq(min.val, max.val, 0.25)
  dummy = numeric(length=length(Era20)) * NA
  xlabname.empty = ""
  xlabname.full = "10m windspeed [m/s]"
  ylabname = "Density"

  titname = "Frequency distribution of ERA20C"
  histoPlot(Era20, dummy, breaks, xlims=c(min.val, max.val),
            titname, xlabname.empty, ylabname)

  titname = "Frequency distribution of ERA-I"
  histoPlot(EraI, dummy, breaks, xlims=c(min.val, max.val),
            titname, xlabname.empty, ylabname)

  titname = "Frequency distribution of HErZ"
  histoPlot(Herz, dummy, breaks, xlims=c(min.val, max.val),
            titname, xlabname.full, ylabname)

  titname = "Frequency distribution of station data"
  histoPlot(Stat, dummy, breaks, xlims=c(min.val, max.val),
            titname, xlabname.full, ylabname)
  mtext("Daily windspeed at 10m height", font=2, cex=1.2, outer=TRUE)

  titname = paste0("Frequency distribution of ERA20C\n",
                   "in green and ERA-Interim shaded")
  histoPlot(Era20, EraI, breaks, xlims=c(min.val, max.val), titname,
            xlabname.empty, ylabname, xaxis=axis.n, addPlot=TRUE)

  titname = paste0("Frequency distribution of ERA20C\n",
                   "in green and COSMO HErZ shaded")
  histoPlot(Era20, Herz, breaks, xlims=c(min.val, max.val), titname,
            xlabname.empty, ylabname, xaxis=axis.n, addPlot=TRUE)

  titname = paste0("Frequency distribution of ERA20C\n",
                   "in green and station data shaded")
  xlabname = "10m station windspeed [m/s]"
  histoPlot(Era20, Stat, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname, addPlot=TRUE)

  titname = paste0("Frequency distribution of ERA-Interim\n",
                   "in green and COSMO HErZ shaded")
  histoPlot(EraI, Herz, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname, addPlot=TRUE)
  mtext("Daily windspeed at 10m height", font=2, cex=1.2, outer=TRUE)

  titname = paste0("Frequency distribution of ERA-Interim\n",
                   "in green and station data shaded")
  histoPlot(EraI, Stat, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname, addPlot=TRUE)

  titname = paste0("Frequency distribution of COSMO HErZ\n",
                   "in green and station data shaded")
  histoPlot(Herz, Stat, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname, addPlot=TRUE)
  mtext("Daily windspeed at 10m height", font=2, cex=1.2, outer=TRUE)

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
                            width, height) {

  same.length = F
  if (length(Era20cXts) == length(HerzXts)) {same.length = T}

  axis.n = 'n'
  axis.y = 's'

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
                                      round(Corr.vals$c.20c.H, 2))),
           text.col=c("blue"))
  }

  if (same.length) {
    Herz = as.numeric(HerzXts)
    Era = as.numeric(Era20cXts)

    xlabname = "100m ERA20C windspeed [m/s]"
    ylabname = "116m HErZ windspeed [m/s]"
    text.str = "100m ERA20C vs 116m HErZ windspeed"
    scatterPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname,
                text.str=text.str)

    titname = "Quantile-quantile plot"
    qqPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname, text.str=text.str)

    min.val = floor(min(min(Era), min(Herz)))
    max.val = ceiling(max(max(Era), max(Herz)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(Era)) * NA

    titname = "Frequency distribution of ERA20C"
    ylabname = "Density"
    histoPlot(Era, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = "Frequency distribution of COSMO HErZ"
    xlabname = ylabname
    histoPlot(Herz, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = paste0("Frequency distribution of ERA20C windspeed at 100m\n",
                     "in green and COSMO HErZ at 116m shaded")
    xlabname = "windspeed [m/s]"
    histoPlot(Era, Herz, breaks, xlims=c(min.val, max.val),
              titname, xlabname, ylabname, addPlot=T)

  }
  dev.off()
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
  for (month in seq(0,11)) {
    monthly.era  <- era.xts[which(date.era$mon==month)]
    monthly.stat <- station.xts[which(date.stat$mon==month)]

    min.val = floor(min(min(monthly.era), min(monthly.stat)))
    max.val = ceiling(max(max(monthly.era), max(monthly.stat)))

    breaks = seq(min.val, max.val, 0.25)
    if (era.monthly) {
      titname.ext = "monthly"

    } else {
      titname.ext = "daily"
    }
    if (length(titname) > 1) {
      titname.new = paste0("Histogram of ", titname.ext, "\n", titname[1],
                           " (green) and ",titname[2]," station data (shaded)")
    } else {
      titname.new = titname
    }
    xlabname = "windspeed [m/s]"
    ylabname = "Density"
    histoPlot(monthly.era, monthly.stat, breaks, xlims=c(min.val, max.val),
              titname.new, xlabname, ylabname, addPlot=T)

    # par("usr") prvides the currently set axis limits
    #     text(min.val+0.1, par("usr")[4]-0.05, months[month+1], cex=2.)
    text(1+0.1, par("usr")[4]-0.05, months[month+1], cex=2.)

    PDF.score.anncycle[month+1] = PDFscore(monthly.era, monthly.stat)
  }
  PDF.score.ann[] = PDFscore(era.xts, station.xts)

  if (length(titname) > 1) {
    titname.new = paste0("PDF score of ", titname.ext, " ", titname[1], " and ",
                       titname[2], " station data")
  } else {
    titname.new = titname
  }
  plot(PDF.score.anncycle, main=titname.new, ylab="pdf score",
       xlab="months of the year", type="b", pch=16, col="blue")
  lines(PDF.score.ann, type="b", lty=2, pch=20, col="red")

  vioplot(era.xts, station.xts, horizontal=TRUE,
          names=c("reanalysis", "station data"))
  if (length(titname) > 1) {
    mtext(paste0("Violin plot of ", titname.ext, " ", titname[1], " and ",
                 titname[2], " station data"), line=1, font=2, cex=1.2)
  } else {
    mtext(titname, line=1, font=2, cex=1.2)
  }
  mtext(paste0("windspeed [m/s]"), side=1, line=2)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Produce a scatter plot
#' @description Prodcue a scatter plot of two data set samples.
#' @param X first data sample for the scatter plot
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
    lines(density(X), col="red", lw=1.5)
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

#-----------------------------------------------------------------------------------
