#' @title Plot station measurements together with ERA20C, ERA-I, and COSMO-REA6.
#' @description \code{PlotStationEra} plots the station values together with the
#'   corresponding ERA20C, ERA-I, and COSMO-REA6 pixel and provides the correlation
#'   between these time series. Optionally, it is possible to plot the anomaly.
#'   The plot is saved in pdf format and there is no return value.
#' @param s.obj is a ClimObject which holds the station data and corresponding
#'   reanalysis data at the location of the station data.
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param anomaly is an optional parameter which determines whether to plot anomalies
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
PlotStationEra <- function(s.obj, titname, outdir, fname, anomaly=FALSE) {

  obs.date = as.POSIXlt(s.obj$obs$data$date)

  PS = PlottingSettings(s.obj$obs$data)
  pdf(paste(outdir, fname, sep=""), width=PS$land.a4width, height=PS$land.a4height)

  if (is.null(s.obj$era20c10$data$wind_speed)) {
    Era20cXts = NULL
  } else {
    Era20cXts = xts(s.obj$era20c10$data$wind_speed, order.by=obs.date)
  }
  if (is.null(s.obj$eraI10$data$wind_speed)) {
    EraIXts = NULL
  } else {
    EraIXts = xts(s.obj$eraI10$data$wind_speed, order.by=obs.date)
  }
  HerzXts = xts(s.obj$herz10$data$wind_speed, order.by=obs.date)
  StatXts = xts(s.obj$obs$data$wind_speed, order.by=obs.date)
  if (anomaly) {
    if (!is.null(Era20cXts)) Era20cXts = Era20cXts - mean(Era20cXts)
    if (!is.null(EraIXts)) EraIXts = EraIXts - mean(EraIXts)
    HerzXts = HerzXts - mean(HerzXts, na.rm=T)
    StatXts = StatXts - mean(StatXts, na.rm=T)
  }
  if (!is.null(Era20cXts) & !is.null(EraIXts)) {
    Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  } else {
    dummy = numeric(length=length(StatXts)) * NA
    dummy = xts(dummy, order.by = index(StatXts))
    Ylims = GetYlims(HerzXts, StatXts, dummy, dummy)
  }
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  # ERA20C
  dummy = numeric(length=length(StatXts)) * NA
  dummy = xts(dummy, order.by = index(StatXts))
  plot(dummy, main=titname, ylab="wind speed [m/s]", ylim=c(yliml, ylimh))

  if (!is.null(Era20cXts)) lines(Era20cXts, type="b", pch=16, col="blue", lw=1.5)

  # ERA-I
  if (!is.null(EraIXts)) lines(EraIXts, type="b", pch=16, col="red", lw=1.5)

  # COSMO-REA6
  lines(HerzXts, type="b", pch=16, col="green3", lw=1.5)

  # Station
  lines(StatXts, type="b", pch=16, col="black", lw=1.5)

  Corr.vals = GetCorrXts(era20c=Era20cXts, eraI=EraIXts, herz=HerzXts, stat=StatXts)

  legend("topleft", legend=c(paste0("Corr(ERA20C, Stat) = ",
                                    round(Corr.vals$c.20c.S, 2)),
                             paste0("Corr(ERAI, Stat) = ",
                                    round(Corr.vals$c.I.S, 2)),
                             paste0("Corr(COSMO-REA6, Stat) = ",
                                    round(Corr.vals$c.H.S, 2)),
                             paste0("Corr(ERA20C, ERAI)= ",
                                    round(Corr.vals$c.20c.I, 2)),
                             paste0("Corr(ERA20C, COSMO-REA6)= ",
                                    round(Corr.vals$c.20c.H, 2)),
                             paste0("Corr(ERAI, COSMO-REA6)= ",
                                    round(Corr.vals$c.I.H, 2))),
         text.col=c("blue", "red", "green", "black", "black", "black"))
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot a 2-by-2 multi panel plot.
#' @description This is supposed to be a rather generic routine to plot a 2-by-2
#'   multi panel plot. So far, it is used to plot the four different data sources
#'   (ERA20C, ERA-Interim, COSMO-REA6, station data) for monthly and seasonal values. The
#'   plot is saved into a pdf file, and the function does not have a return value.
#' @param outdir string of the output directory into which the plot is saved
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param titname string of the plot title name
#' @param Era20c extended time series of monthly or seasonal ERA20C data
#' @param EraI extended time series of momthly or seasonal ERA-Interim data
#' @param Herz extended time series of monthly or seasonal COSMO-REA6 data
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
#' @importFrom xts xts
#' @importFrom zoo index
PlotMultiPanel <- function(outdir, fname, titname, Era20c, EraI, Herz, Stat,
                           length.plot, era.months, plot.diff=FALSE) {

  if (era.months) { # months will be plotted
    all.months = c("January","February","March","April","May","June","July",
                   "August","September","October","November","December")
  } else { # seasons will be plotted
    all.seasons = c("Winter", "Spring", "Summer", "Autumn")
  }

  PS = PlottingSettings(Stat[[1]])
  pdf(paste(outdir, fname, sep=""), width=PS$land.a4width, height=PS$land.a4height)

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
    legend("topleft", legend=c(paste0("COSMO-REA6 ", all.months[length.plot[[1]]]),
                               paste0("COSMO-REA6 ", all.months[length.plot[[2]]])),
           text.col=color)
  } else {
    legend("topleft", legend=c(paste0("COSMO-REA6 ", all.seasons[length.plot[[1]]]),
                               paste0("COSMO-REA6 ", all.seasons[length.plot[[2]]])),
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
#'   and COSMO-REA6 data.
#' @description \code{PlotStationEraSelSeasons} prepares extended time series of
#'   seasonal means, and optionally their anomalies, of station data and locally
#'   corresponding global and regional reanalyses. The seasons for which data shall
#'   be prepared and plotted need to be set hard-coded within this function. Of
#'   course, this setting can be put into the Settings.R file if necessary.
#'   A generic plotting routine is called which actually performs the plotting.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for COSMO-REA6
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param seasons is an optional parameter which determines whetther to plot the
#'   monthly (F) or seasonal (T) values of the above time series
#' @param anomaly is an optional parameter which determines whether to plot anomalies
#' @export
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
#'   ERA-I, and COSMO-REA6 data.
#' @description \code{PlotStationEraSelMonths} prepares extended time series of
#'   station data, ERA20C, ERA-I, and COSMO-REA6 data - for specific months only. These
#'   months are set hard-coded within this function. Of course, this setting can be
#'   put into the Settings.R file if necessary.
#'   Optionally, it is possible to prepare the anomalies. A generic plotting routine
#'   is called which actually performs the plotting.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for COSMO-REA6
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param anomaly is an optional parameter which determines whether to plot anomalies
#' @note need to adopt titname to months; need to plot into four different panals
#' @importFrom zoo index
#' @export
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
#'   and COSMO-REA6 data.
#' @description This function plots station and reanalysis data into scatter plots
#'   and Quantile-quantile plots (QQ-plot).
#' @param Era20cXts extended time series of the ERA20C pixel corresponding to the
#'   station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for COSMO-REA6
#' @param StatXts extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot/s is/are saved
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
PlotStationEraSQ <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                             titname, outdir, fname, ana.time.res) {

  if (is.null(Era20cXts)) {
    Era20 = NULL
  } else {
    Era20  = as.numeric(Era20cXts)
  }
  if (is.null(EraIXts)) {
    EraI = NULL
  } else {
    EraI = as.numeric(EraIXts)
  }
  Herz = as.numeric(HerzXts)
  Stat = as.numeric(StatXts)

  if (!is.null(Era20cXts) & !is.null(EraIXts)) {
    Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  } else {
    dummy = numeric(length=length(StatXts)) * NA
    dummy = xts(dummy, order.by = index(StatXts))
    Ylims = GetYlims(HerzXts, StatXts, dummy, dummy)
  }
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  if (ana.time.res$time.res == ana.time.res$monthly) {
    mtext.titname = "Monthly wind speed at 10m height"
    titname.scatter = gsub("wind speed", "Scatter and QQ-plot of monthly wind speed",
                           titname)
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    mtext.titname = "Daily wind speed at 10m height"
    titname.scatter = gsub("wind speed", "Scatter and QQ-plot of daily wind speed",
                           titname)
  } else if (ana.time.res$time.res == ana.time.res$hourly) {
    mtext.titname = "Hourly wind speed at 10m height"
    titname.scatter = gsub("wind speed", "Scatter and QQ-plot of hourly wind speed",
                           titname)
  }

  PS = PlottingSettings(StatXts)
  fname.scatter = gsub(".pdf", "_scatterQQ-Plots.pdf", fname)
  pdf(paste0(outdir, fname.scatter),
      width=PS$port.a4width, height=PS$port.a4height)
  if (is.null(Era20) & is.null(EraI)) {
    par(mfrow=c(2,1), mar=c(0,0,0,0), oma=c(5,5,4,0.5))
  } else {
    par(mfrow=c(3,2), mar=c(0,0,0,0), oma=c(5,5,4,0.5))
  }

  titname = ""
  xlabname = ""
  ylabname = ""
  if (!is.null(Era20)) {
    text.str = "Era20c vs ERA-Interim"
    scatterPlot(Era20, EraI, yliml, ylimh, titname,
                xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.y)
    qqPlot(Era20, EraI, yliml, ylimh, "",
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.n)

    text.str = "Era20c vs COSMO-REA6"
    scatterPlot(Era20, Herz, yliml, ylimh, titname,
                xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.y)
    qqPlot(Era20, Herz, yliml, ylimh, "",
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.n)

    text.str = "ERA20C vs station data"
    scatterPlot(Era20, Stat, yliml, ylimh, titname,
                xlabname, ylabname, text.str=text.str, xaxis=PS$axis.y, yaxis=PS$axis.y)
    qqPlot(Era20, Stat, yliml, ylimh, "",
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.y, yaxis=PS$axis.n)

    mtext(titname.scatter, line=1, outer=TRUE)
    mtext("wind speed [m/s]", side=2, line=3, outer=TRUE)
    mtext("wind speed [m/s]", side=1, line=3, outer=TRUE)

  }

  if (!is.null(EraI)) {
    text.str = "ERA-Interim vs COSMO-REA6"
    scatterPlot(EraI, Herz, yliml, ylimh, titname,
                xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.y)
    qqPlot(EraI, Herz, yliml, ylimh, titname,
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.n)

    text.str = "ERA-Interim vs station data"
    scatterPlot(EraI, Stat, yliml, ylimh, titname,
                xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.y)
    qqPlot(EraI, Stat, yliml, ylimh, titname,
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.n, yaxis=PS$axis.n)
  }

  text.str = "COSMO-REA6 vs station data"
  scatterPlot(Herz, Stat, yliml, ylimh, titname,
              xlabname, ylabname, text.str=text.str, xaxis=PS$axis.y, yaxis=PS$axis.n)
  if (is.null(Era20) & is.null(EraI)) {
    qqPlot(Herz, Stat, yliml, ylimh, titname,
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.y, yaxis=PS$axis.y)
  } else {
    qqPlot(Herz, Stat, yliml, ylimh, titname,
           xlabname, ylabname, text.str=text.str, xaxis=PS$axis.y, yaxis=PS$axis.y)
  }

  mtext(titname.scatter, line=1, outer=TRUE)
  mtext("wind speed [m/s]", side=2, line=3, outer=TRUE)
  mtext("wind speed [m/s]", side=1, line=3, outer=TRUE)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Compare 100m wind speed of ERA20C and COSMO-REA6 pixel by pixel.
#' @description \code{Plot100mEraHerz} compares the 100m wind speed of the ERA20C
#'   global reanalysis with the 116m wind speed of the COSMO-REA6 regional reanalysis.
#'   This function performs a pixel wise comparison at the station locationto
#'   provided by the package. Scatter plots, QQplots, histogram plots, and the
#'   PDFscore are produced.
#' @param Era20cXts extended time series of an ERA20C pixel
#' @param HerzXts same as above for COSMO-REA6
#' @param titname string of the plot title name
#' @param statname string of the station name whose pixel is plotted
#' @param outdir string of the output directory into which the plot is saved
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
Plot100mEraHerz <- function(Era20cXts, HerzXts,
                            titname, statname, outdir, fname,
                            ana.time.res) {

  if (is.null(Era20cXts)) CallStop("Variable was unexpectadly NULL, aborting!")

  same.length = F
  if (length(Era20cXts) == length(HerzXts)) {same.length = T}

  if (ana.time.res$time.res == ana.time.res$monthly) {
    titname = paste0("Monthly ", titname)
    titname.ext = "monthly"
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    titname = paste0("Daily ", titname)
    titname.ext = "daily"
  }

  PS = PlottingSettings(HerzXts)
  pdf(paste0(outdir, fname), width=PS$land.a4width, height=PS$land.a4height)

  dummy = numeric(length=length(Era20cXts)) * NA
  dummy = xts(dummy, order.by = index(Era20cXts))

  Ylims = GetYlims(Era20cXts, HerzXts, dummy, dummy)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  plot(dummy, main=titname, ylab="wind speed [m/s]", ylim=c(yliml, ylimh))

  # ERA20C
  lines(Era20cXts, type="b", pch=16, col="blue", lw=1.5)

  # COSMO-REA6
  lines(HerzXts, type="b", pch=16, col="green3", lw=1.5)

  if (same.length) {
    Corr.vals = GetCorrXts(era20c=Era20cXts, herz=HerzXts, eraI=dummy, stat=dummy)

    legend("topleft", legend=c(paste0("Corr(ERA20C, COSMO-REA6)= ",
                                      round(Corr.vals$c.20c.H, 2)),
                               'ERA20C', 'COSMO-REA6'),
           text.col=c("black", "blue", "green3"))
  }

  if (same.length) {
    Herz = as.numeric(HerzXts)
    Era = as.numeric(Era20cXts)

    xlabname = "100m ERA20C wind speed [m/s]"
    ylabname = "116m COSMO-REA6 wind speed [m/s]"
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
                     "COSMO-REA6 at 116m")
    xlabname = ylabname
    histoPlot(Herz, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname,
              ylabname)
    titname = paste0("Frequency distribution of ", titname.ext,
                     " ERA20C wind speed at 100m\n",
                     "in green and COSMO-REA6 at 116m shaded")
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
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param station.name is a string holding the station name at which location the
#'   distribution data was extraced
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param Era20cXts10,Era20cXts100,EraIXts extended time series of ERA20C and ERA-I
#'   10m and 100m height
#' @param HerzXts10,HerzXts35,HerzXts69,HerzXts116,HerzXts178,HerzXts258 extended
#'   time series of available COSMO-REA6 data at different heights
#' @param StatXts extended time series of station data
#' @param plot.10m,plot.100m,plot.HerzProfile optional parameters setting whether
#'   to plot only 10m values, only 100m values, or the herz profile, respectively.
#'   The default value is FALSE for all three settings.
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
PlotHistograms <- function(outdir, fname, station.name, ana.time.res,
                           Era20cXts10=NULL, Era20cXts100=NULL, EraIXts=NULL,
                           HerzXts10=NULL, HerzXts35=NULL, HerzXts69=NULL,
                           HerzXts116=NULL, HerzXts178=NULL, HerzXts258=NULL,
                           StatXts=NULL, plot.10m=FALSE, plot.100m=FALSE,
                           plot.HerzProfile=FALSE) {

  if (is.null(Era20cXts10) & is.null(Era20cXts100)
      & is.null(EraIXts) & is.null(HerzXts10)
      & is.null(HerzXts35) & is.null(HerzXts69)
      & is.null(HerzXts116) & is.null(HerzXts178)
      & is.null(HerzXts258) & is.null(StatXts)) {
    CallStop("All passed data are NULL!")
  }

  dummy = numeric(length=length(StatXts)) * NA
  dummy = xts(dummy, order.by = index(StatXts))
  if(is.null(HerzXts258)) {
    if (is.null(Era20cXts10)) {
      Ylims = GetYlims(HerzXts10, StatXts, dummy, dummy)
    } else {
      Ylims = GetYlims(EraIXts, HerzXts10, Era20cXts100, StatXts)
    }
  } else {
    if (is.null(Era20cXts10)) {
      Ylims = GetYlims(HerzXts10, HerzXts258, StatXts, dummy)
    } else {
      Ylims = GetYlims(EraIXts, HerzXts10, HerzXts258, StatXts)
    }
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

  xlabname.empty = ""
  xlabname.full = "wind speed [m/s]"
  ylabname = "Density"

  PS = PlottingSettings(StatXts)
  if (plot.10m) {

    tit.height = "10m"
    mtext.titname = HistMultiPanelTitstr(tit.height, station.name, ana.time.res)

    fname = gsub('Histogram', 'Histogram_ERA-Station-10m', fname)
    pdf(paste0(outdir, fname), width=PS$land.a4width, height=PS$land.a4height)

    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    if (!is.null(Era20cXts10)) {
      min.val = floor(min(min(era20c10, na.rm=TRUE), min(eraI, na.rm=TRUE),
                          min(herz10, na.rm=TRUE), min(stat, na.rm=TRUE)))
      max.val = ceiling(max(max(era20c10, na.rm=TRUE), max(eraI, na.rm=TRUE),
                            max(herz10, na.rm=TRUE), max(stat, na.rm=TRUE)))
    } else {
      min.val = floor(min(min(herz10, na.rm=TRUE), min(stat, na.rm=TRUE)))
      max.val = ceiling(max(max(herz10, na.rm=TRUE), max(stat, na.rm=TRUE)))
    }
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(herz10)) * NA

    if (!is.null(Era20cXts10)) {
      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA20C")
      histoPlot(era20c10, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.empty, ylabname="", xaxis=PS$axis.n)
      plotLegendStats(xlims=c(min.val, max.val), era20c10)

      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA20C\n",
                       "in green and ERA-Interim shaded")
      histoPlot(era20c10, eraI, breaks, xlims=c(min.val, max.val), titname,
                xlabname.empty, ylabname="", xaxis=PS$axis.n, addPlot=TRUE)

      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA20C\n",
                       "in green and COSMO-REA6 shaded")
      histoPlot(era20c10, herz10, breaks, xlims=c(min.val, max.val), titname,
                xlabname.full, ylabname, addPlot=TRUE)

      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA20C\n",
                       "in green and station data shaded")
      xlabname = "10m station wind speed [m/s]"
      histoPlot(era20c10, stat, breaks, xlims=c(min.val, max.val), titname,
                xlabname.full, ylabname="", addPlot=TRUE)
      mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)
    }

    if (!is.null(EraIXts)) {
      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA-I")
      histoPlot(eraI, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.empty, ylabname, xaxis=PS$axis.n)
      plotLegendStats(xlims=c(min.val, max.val), eraI)

      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA-Interim\n",
                       "in green and COSMO-REA6 shaded")
      histoPlot(eraI, herz10, breaks, xlims=c(min.val, max.val), titname,
                xlabname.empty, ylabname="", xaxis=PS$axis.n, addPlot=TRUE)

      titname = paste0("Frequency distribution of ", ana.time.res$time.res, " ERA-Interim\n",
                       "in green and station data shaded")
      histoPlot(eraI, stat, breaks, xlims=c(min.val, max.val), titname,
                xlabname.full, ylabname, addPlot=TRUE)

      mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)
    }

    titname = paste0("Frequency distribution of ", ana.time.res$time.res, " COSMO-REA6")
    histoPlot(herz10, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname="")
    plotLegendStats(xlims=c(min.val, max.val), herz10)

    titname = paste0("Frequency distribution of ", ana.time.res$time.res, " station data")
    histoPlot(stat, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)
    plotLegendStats(xlims=c(min.val, max.val), stat)

    titname = paste0("Frequency distribution of ", ana.time.res$time.res, " COSMO-REA6\n",
                     "in green and station data shaded")
    histoPlot(herz10, stat, breaks, xlims=c(min.val, max.val), titname,
              xlabname.full, ylabname="", addPlot=TRUE)
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    dev.off()

  }

  if (plot.100m) {

    tit.height = "100m"
    mtext.titname = HistMultiPanelTitstr(tit.height, station.name, ana.time.res)

    fname = gsub('Histogram', 'Histogram_ERA20C-HErZ-100m', fname)
    pdf(paste0(outdir, fname), width=PS$land.a4width, height=PS$land.a4height)

    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    if (!is.null(Era20cXts100)) {
      min.val = floor(min(min(era20c100, na.rm=TRUE), min(herz116, na.rm=TRUE)))
      max.val = ceiling(max(max(era20c100, na.rm=TRUE), max(herz116, na.rm=TRUE)))
    } else {
      min.val = floor(min(herz116, na.rm=TRUE))
      max.val = ceiling(max(herz116, na.rm=TRUE))
    }
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(herz116)) * NA

    if (!is.null(Era20cXts100)) {
      titname = paste0("Frequency distribution of 100m ", ana.time.res$time.res,
                       " ERA20C wind speed")
      xlabname = "wind speed [m/s]"
      histoPlot(era20c100, dummy, breaks, xlims=c(min.val, max.val), titname,
                xlabname.empty, xaxis=PS$axis.n, ylabname)
      plotLegendStats(xlims=c(min.val, max.val), era20c100)

      titname = paste0("Frequency distribution of ", ana.time.res$time.res,
                       " ERA20C wind speed at 100m\n",
                       "in green and COSMO-REA6 at 116m shaded")
      xlabname = "wind speed [m/s]"
      histoPlot(era20c100, herz116, breaks, xlims=c(min.val, max.val),
                titname, xlabname, ylabname="", addPlot=T)
    }

    titname = paste0("Frequency distribution of 116m COSMO-REA6 wind speed")
    xlabname = "wind speed [m/s]"
    histoPlot(herz116, dummy, breaks, xlims=c(min.val, max.val), titname,
              xlabname, ylabname)

    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    dev.off()

  }

  if (plot.HerzProfile) {

    fname = gsub('Histogram', 'Histogram_HErZ-Profile', fname)
    PlotHistogramsHerzHeights(paste0(outdir, fname), PS, herz10, herz35, herz69,
                              herz116, herz178, herz258, ana.time.res, station.name)

  }
}

#-----------------------------------------------------------------------------------

#' @title Create overall title for histogram multi panel plots.
#' @description Create the overall title for histogram multi-panel plots depending
#'   on the time step, i.e., hourly, daily, monthly, which is provided by the
#'   variable ana.time.res. The height string for which the data is plot is provided
#'   and the title string returned.
#' @param tit.height is a string holding the height in meter of the data.
#' @param station.name is a string holding the station name.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @return mtext.titname is a string holding the finished title name.
HistMultiPanelTitstr <- function(tit.height, station.name, ana.time.res) {

  tit.ext = paste0(" wind speed at ", tit.height, " height at ", station.name)
  if (ana.time.res$time.res == ana.time.res$monthly) {
    mtext.titname = paste0("Monthly ", tit.ext)
  }
  if (ana.time.res$time.res == ana.time.res$daily) {
    mtext.titname = paste0("Daily ", tit.ext)
  }
  if (ana.time.res$time.res == ana.time.res$hourly) {
    mtext.titname = paste0("Hourly ", tit.ext)
  }

  return(mtext.titname)
}

#-----------------------------------------------------------------------------------

#' @title Plot histrograms for COSMO-REA6 of one pixel location at different model
#'   level heights.
#' @description Plot histograms for the lowest six model levels between 10m and 258m.
#' @param fname is a string holding the complete file name (including its path) of
#'   the pdf plot to be created.
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param herz10,herz35,herz69,herz116,herz178,herz258 are numeric vectors holding
#'   the windspeed data of the COSMO-REA6 regional reanalysis of the lowest six
#'   model levels.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param station.name is a string holding the station name.
PlotHistogramsHerzHeights <- function(fname, PS, herz10, herz35, herz69,
                                      herz116, herz178, herz258, ana.time.res,
                                      station.name) {

  pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
  par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

  min.val = floor(min(min(herz10, na.rm=TRUE), min(herz35, na.rm=TRUE),
                      min(herz69, na.rm=TRUE), min(herz116, na.rm=TRUE),
                      min(herz178, na.rm=TRUE), min(herz258, na.rm=TRUE)))
  max.val = ceiling(max(max(herz10, na.rm=TRUE), max(herz35, na.rm=TRUE),
                        max(herz69, na.rm=TRUE), max(herz116, na.rm=TRUE),
                        max(herz178, na.rm=TRUE), max(herz258, na.rm=TRUE))) + 1
  if (ana.time.res$time.res == ana.time.res$monthly) breaks = seq(min.val, max.val, 0.2)
  if (ana.time.res$time.res == ana.time.res$daily) breaks = seq(min.val, max.val, 0.5)
  if (ana.time.res$time.res == ana.time.res$hourly) breaks = seq(min.val, max.val, 0.75)
  dummy = numeric(length=length(herz10)) * NA

  if (ana.time.res$time.res == ana.time.res$monthly) {
    mtext.titname = paste0("Frequency distribution of monthly COSMO-REA6 wind speed for ",
                           station.name)
  }
  if (ana.time.res$time.res == ana.time.res$daily) {
    mtext.titname = paste0("Frequency distribution of daily COSMO-REA6 wind speed for ",
                           station.name)
  }
  if (ana.time.res$time.res == ana.time.res$hourly) {
    mtext.titname = paste0("Frequency distribution of hourly COSMO-REA6 wind speed for ",
                           station.name)
  }

  titname = paste0("Wind speed distribution at 10m")
  xlabname.empty = ""
  xlabname.full = "wind speed [m/s]"
  ylabname.empty = ""
  ylabname.full = "Density"
  histoPlot(herz10, dummy, breaks, xlims=c(min.val, max.val), titname,
            xlabname.empty, ylabname.full)
  plotLegendStats(xlims=c(min.val, max.val), herz10)

  titname = paste0("Wind speed distribution at 35m")
  xlabname = ""
  histoPlot(herz35, dummy, breaks, xlims=c(min.val, max.val), titname,
            xlabname.empty, ylabname.empty)
  plotLegendStats(xlims=c(min.val, max.val), herz35)

  titname = paste0("Wind speed distribution at 69m")
  xlabname = "wind speed [m/s]"
  histoPlot(herz69, dummy, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname.full)
  plotLegendStats(xlims=c(min.val, max.val), herz69)

  titname = paste0("Wind speed distribution at 116m")
  xlabname = "wind speed [m/s]"
  histoPlot(herz116, dummy, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname.empty)
  plotLegendStats(xlims=c(min.val, max.val), herz116)
  mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

  titname = paste0("Wind speed distribution at 178m")
  xlabname = "wind speed [m/s]"
  histoPlot(herz178, dummy, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname.full)
  plotLegendStats(xlims=c(min.val, max.val), herz178)

  titname = paste0("Wind speed distribution at 258m")
  xlabname = "wind speed [m/s]"
  histoPlot(herz258, dummy, breaks, xlims=c(min.val, max.val), titname,
            xlabname.full, ylabname.empty)
  plotLegendStats(xlims=c(min.val, max.val), herz258)
  mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot histrograms for tower measurements.
#' @description Plot histograms for the Lindenberg, Cabauw, Fino1 and Fino2 tower
#'   tower measurements. Histograms are plotted for 10m and 100m heights; seperately
#'   and in comparison with reanalysis data.
#' @param outdir is a string containing the output path of the plot
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @importFrom xts xts
#' @export
PlotHistogramsTower <- function(outdir, fname, ana.time.res, tower.obj,
                                plot.10m=FALSE, plot.100m=FALSE,
                                plot.HerzProfile=FALSE) {

  t.obj = tower.obj$climate_data_objects
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

  if (ana.time.res$time.res == ana.time.res$monthly) monthly.ext = 'Monthly'
  if (ana.time.res$time.res == ana.time.res$daily)   monthly.ext = 'Daily'
  if (ana.time.res$time.res == ana.time.res$hourly)  monthly.ext = 'Hourly'

  if (is.null(t.obj$era20c10$data$wind_speed)) {
    dummy = numeric(length=length(t.obj$herz10$data$wind_speed)) * NA
    dummy = xts(dummy, order.by = tower.date)
  } else {
    dummy = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
  }

  xlabname.empty = ""
  xlabname.full = "wind speed [m/s]"
  ylabname.empty = ""
  ylabname.full = "Density"

  PS = PlottingSettings(t.obj$herz116$data)

  if (plot.10m) {

    fname.new = fname
    if (t.obj$obs$data$StationName[1] == "Lindenberg" |
        t.obj$obs$data$StationName[1] == "Cabauw") {

      # -- preparations
      data.10.vals = t.obj$obs6$data$wind_speed
      if (is.null(t.obj$era20c10$data$wind_speed)) {
        dummy = NA
      } else {
        dummy = t.obj$era20c10$data$wind_speed
      }
      min.val = floor(min(min(dummy, na.rm=TRUE),
                          min(t.obj$herz10$data$wind_speed, na.rm=TRUE),
                          min(data.10.vals, na.rm=TRUE)))
      max.val = ceiling(max(max(dummy, na.rm=TRUE),
                            max(data.10.vals, na.rm=TRUE),
                            max(t.obj$herz10$data$wind_speed, na.rm=TRUE))) + 1

      if (ana.time.res$time.res == ana.time.res$monthly) breaks = seq(min.val, max.val, 0.2)
      if (ana.time.res$time.res == ana.time.res$daily) breaks = seq(min.val, max.val, 0.5)
      if (ana.time.res$time.res == ana.time.res$hourly) breaks = seq(min.val, max.val, 0.25)

      tit.10.ext = paste0(t.obj$obs$data$StationName[1])
      mtext.titname = HistMultiPanelTitstr(t.obj$herz10$data$height[1],
                                           t.obj$obs$data$StationName[1],
                                           ana.time.res)

      dummy = numeric(length=length(t.obj$herz10$data$wind_speed)) * NA

      # -- plotting
      fname.new = gsub('Histogram', 'Histogram_10m', fname.new)
      pdf(paste0(outdir, fname.new), width=PS$land.a4width/0.75, height=PS$land.a4height)
      par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

      if (ana.time.res$time.res == ana.time.res$hourly) {
        pl.xaxis = PS$axis.n
      } else {
        pl.xaxis = PS$axis.y
      }
      titname = paste0(monthly.ext, " wind speed of ", tit.10.ext)
      histoPlot(data.10.vals, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.empty, ylabname.full, xaxis=pl.xaxis)
      plotLegendStats(xlims=c(min.val, max.val), as.numeric(data.10.vals))

      titname = paste0(monthly.ext, " wind speed of COSMO-REA6")
      histoPlot(t.obj$herz10$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.empty, ylabname.empty)
      plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$herz10$data$wind_speed))

      if (ana.time.res$time.res != ana.time.res$hourly) {
        titname = paste0(monthly.ext, " wind speed of ERA20C")
        histoPlot(t.obj$era20c10$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
                  titname, xlabname.full, ylabname.full)
        plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$era20c10$data$wind_speed))

        titname = paste0(monthly.ext, " wind speed of ERA-Interim")
        histoPlot(t.obj$eraI10$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
                  titname, xlabname.full, ylabname.empty)
        plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$eraI10$data$wind_speed))

        mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

        titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                         tit.10.ext, " in green and ERA-Interim shaded")
        histoPlot(data.10.vals, t.obj$eraI10$data$wind_speed, breaks,
                  xlims=c(min.val, max.val), titname, xlabname.empty, ylabname.full,
                  xaxis=PS$axis.n, addPlot=TRUE)

        titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                         tit.10.ext, " in green and ERA20C shaded")
        histoPlot(data.10.vals, t.obj$era20c10$data$wind_speed, breaks,
                  xlims=c(min.val, max.val), titname, xlabname.empty, ylabname.empty,
                  xaxis=PS$axis.n, addPlot=TRUE)

        titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                         "COSMO-REA6 in green and ERA-Interim shaded")
        histoPlot(t.obj$herz10$data$wind_speed, t.obj$era20c10$data$wind_speed, breaks,
                  xlims=c(min.val, max.val), titname, xlabname.full, ylabname.full,
                  addPlot=TRUE)

        mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)
      }

      titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                       tit.10.ext, " in green and COSMO-REA6 shaded")
      histoPlot(data.10.vals, t.obj$herz10$data$wind_speed, breaks, xlims=c(min.val, max.val),
                titname, xlabname.full, ylabname.empty, addPlot=TRUE)

      mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)
      dev.off()

    }
  }

  if (plot.100m) {

    fname.new = fname
    # -- preparations
    data.100.vals = t.obj$obs$data$wind_speed
    if (is.null(t.obj$era20c100$data$wind_speed)) {
      dummy = NA
    } else {
      dummy = t.obj$era20c100$data$wind_speed
    }
    if (t.obj$obs$data$StationName[1] == "Lindenberg") {
      tit.100.ext = "Lindenberg at 98m"
      data.100.vals = t.obj$obs$data$wind_speed
      min.val = floor(min(min(dummy, na.rm=TRUE),
                          min(t.obj$herz116$data$wind_speed, na.rm=TRUE),
                          min(data.100.vals, na.rm=TRUE)))
      max.val = ceiling(max(max(dummy, na.rm=TRUE),
                            max(data.100.vals, na.rm=TRUE),
                            max(t.obj$herz116$data$wind_speed, na.rm=TRUE))) + 1
    } else if (t.obj$obs$data$StationName[1] == "Cabauw"){
      tit.100.ext = "Cabauw at 80m"
      tit2.100.ext = "Cabauw at 140m"
      data.100.vals = t.obj$obs3$data$wind_speed
      data2.100.vals = t.obj$obs2$data$wind_speed
      min.val = floor(min(min(dummy, na.rm=TRUE),
                          min(t.obj$herz116$data$wind_speed, na.rm=TRUE),
                          min(data.100.vals, na.rm=TRUE),
                          min(data2.100.vals, na.rm=TRUE)))
      max.val = ceiling(max(max(dummy, na.rm=TRUE),
                            max(data.100.vals, na.rm=TRUE),
                            max(data2.100.vals, na.rm=TRUE),
                            max(t.obj$herz116$data$wind_speed, na.rm=TRUE))) + 1
    } else if (t.obj$obs$data$StationName[1] == "Fino1" |
               t.obj$obs$data$StationName[1] == "Fino2") {

      tit.100.ext = paste0(t.obj$obs$data$StationName[1], " at ",
                           t.obj$obs$data$height[1])
      data.100.vals = t.obj$obs$data$wind_speed
      min.val = floor(min(min(dummy, na.rm=TRUE),
                          min(t.obj$herz116$data$wind_speed, na.rm=TRUE),
                          min(data.100.vals, na.rm=TRUE)))
      max.val = ceiling(max(max(dummy, na.rm=TRUE),
                            max(data.100.vals, na.rm=TRUE),
                            max(t.obj$herz116$data$wind_speed, na.rm=TRUE))) + 1

    }

    if (ana.time.res$time.res == ana.time.res$monthly) breaks = seq(min.val, max.val, 0.2)
    if (ana.time.res$time.res == ana.time.res$daily) breaks = seq(min.val, max.val, 0.5)
    if (ana.time.res$time.res == ana.time.res$hourly) breaks = seq(min.val, max.val, 0.25)

    mtext.titname = HistMultiPanelTitstr("100m", t.obj$obs$data$StationName[1],
                                         ana.time.res)

    dummy = numeric(length=length(t.obj$herz116$data$wind_speed)) * NA

    # -- plotting
    fname.new = gsub('Histogram', 'Histogram_100m', fname.new)
    pdf(paste0(outdir, fname.new), width=PS$land.a4width/0.75, height=PS$land.a4height)
    par(mfrow=c(2,2), mar=c(1,1,2,0.5), oma=c(2.5,3,3,0.5))

    if (ana.time.res$time.res == ana.time.res$hourly &
        t.obj$obs$data$StationName[1] != "Cabauw") {
      pl.xaxis = PS$axis.n
    } else {
      pl.xaxis = PS$axis.y
    }
    titname = paste0(monthly.ext, " wind speed in ", tit.100.ext)
    histoPlot(data.100.vals, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.empty, ylabname.full, xaxis=pl.xaxis)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(data.100.vals))

    if (t.obj$obs$data$StationName[1] == "Cabauw") {
      titname = paste0(monthly.ext, " wind speed in ", tit2.100.ext)
      histoPlot(data2.100.vals, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.empty, ylabname.empty)
      plotLegendStats(xlims=c(min.val, max.val), as.numeric(data2.100.vals))
    }

    titname = paste0(monthly.ext, " wind speed of COSMO-REA6 at 116m")
    histoPlot(t.obj$herz116$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.empty)
    plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$herz116$data$wind_speed))

    if (ana.time.res$time.res != ana.time.res$hourly) {
      titname = paste0(monthly.ext, " wind speed of ERA20C at 100m")
      histoPlot(t.obj$era20c100$data$wind_speed, dummy, breaks, xlims=c(min.val, max.val),
                titname, xlabname.full, ylabname.full)
      plotLegendStats(xlims=c(min.val, max.val), as.numeric(t.obj$era20c100$data$wind_speed))
    }

    if (t.obj$obs$data$StationName[1] != "Cabauw" &
        ana.time.res$time.res != ana.time.res$hourly) {
      plot.new()
    }
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                     tit.100.ext, " in green and COSMO-REA6 shaded")
    histoPlot(data.100.vals, t.obj$herz116$data$wind_speed, breaks, xlims=c(min.val, max.val),
              titname, xlabname.full, ylabname.full, addPlot=TRUE)

    if (t.obj$obs$data$StationName[1] == "Cabauw") {
      titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                       tit2.100.ext, " in green and COSMO-REA6 shaded")
      histoPlot(data2.100.vals, t.obj$herz116$data$wind_speed, breaks,
                xlims=c(min.val, max.val), titname, xlabname.full, ylabname.full,
                addPlot=TRUE)
    }

    if (ana.time.res$time.res != ana.time.res$hourly) {
      titname = paste0("Frequency distribution of ", monthly.ext, " wind speed of\n",
                       tit.100.ext, " in green and ERA20C shaded")
      histoPlot(data.100.vals, t.obj$era20c100$data$wind_speed, breaks,
                xlims=c(min.val, max.val), titname, xlabname.full, ylabname.empty,
                addPlot=TRUE)
    }
    mtext(mtext.titname, font=2, cex=1.2, line=1, outer=TRUE)

    dev.off()

  }

  if (plot.HerzProfile) {

    fname = gsub('Histogram', 'Histogram_HErZ-Profile', fname)
    PlotHistogramsHerzHeights(paste0(outdir, fname), PS,
                              t.obj$herz10$data$wind_speed,
                              t.obj$herz35$data$wind_speed,
                              t.obj$herz69$data$wind_speed,
                              t.obj$herz116$data$wind_speed,
                              t.obj$herz178$data$wind_speed,
                              t.obj$herz258$data$wind_speed,
                              ana.time.res, t.obj$herz10$data$StationName[1])

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
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param titname is a string containig the title name of the plot
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @import vioplot
#' @importFrom zoo index
#' @export
PlotPDFScore <- function(era.xts, station.xts, outdir, fname, titname,
                         ana.time.res) {

  if (is.null(era.xts)) return(NULL)

  PS = PlottingSettings(station.xts)
  pdf(paste(outdir, fname, sep=""), width=PS$land.a4width, height=PS$land.a4height)

  date.era  <- as.POSIXlt(index(era.xts))
  date.stat <- as.POSIXlt(index(station.xts))
  PDF.score.anncycle = vector(mode="numeric", length=12)
  PDF.score.ann = vector(mode="numeric", length=12)
  months = c("Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep",
             "Okt", "Nov", "Dez")
  max.abs.val = ceiling(max(max(era.xts), max(station.xts)))

  if (ana.time.res$time.res == ana.time.res$monthly) {
    titname.hist = paste0("Histogram of monthly wind speed [m/s] of ", titname)
    titname.pdfs = paste0("PDF score of monthly wind speed [m/s] of ", titname)
    titname.viol = paste0("Violine plot of monthly wind speed [m/s] of ", titname)
  } else if (ana.time.res$time.res == ana.time.res$daily) {
    titname.hist = paste0("Histogram of daily wind speed [m/s] of ", titname)
    titname.pdfs = paste0("PDF score of daily wind speed [m/s] of ", titname)
    titname.viol = paste0("Violine plot of daily wind speed [m/s] of ", titname)
  } else if (ana.time.res$time.res == ana.time.res$hourly) {
    titname.hist = paste0("Histogram of hourly wind speed [m/s] of ", titname)
    titname.pdfs = paste0("PDF score of hourly wind speed [m/s] of ", titname)
    titname.viol = paste0("Violine plot of hourly wind speed [m/s] of ", titname)
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

  era.xts = na.omit(era.xts)
  station.xts = na.omit(station.xts)
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
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @export
PlotTowerERAprofileBP <- function(tower.obj, fname, ana.time.res) {

  t.obj = tower.obj$climate_data_objects
  dummy = numeric(length=length(t.obj$herz10$data$wind_speed)) * NA
  x.labs = "wind speed [m/s]"
  swex = 0.4
  bwex = 0.3
  hori=TRUE
  nch = TRUE
  oline = FALSE
  wind.range = c(0,15)
  if (ana.time.res$time.res == ana.time.res$monthly) tit.ext = "Monthly"
  if (ana.time.res$time.res == ana.time.res$daily) tit.ext = "Daily"
  if (ana.time.res$time.res == ana.time.res$hourly) tit.ext = "Hourly"

  PS = PlottingSettings(t.obj$herz116$data)
  pdf(fname, width=PS$port.a4width*1.25, height=PS$port.a4height)
  par(mar=c(6,16,8,0.5), cex.axis=2.5, cex.lab=2.5, mgp=c(3,1.5,0))

  if (t.obj$obs$data$StationName[1] == "Lindenberg") {
    if (ana.time.res$time.res == ana.time.res$hourly) {
      boxplot.default(t.obj$obs6$data$wind_speed,
                      t.obj$herz10$data$wind_speed,
                      dummy,
                      t.obj$obs5$data$wind_speed,
                      t.obj$herz35$data$wind_speed,
                      t.obj$obs4$data$wind_speed,
                      t.obj$obs3$data$wind_speed,
                      t.obj$herz69$data$wind_speed,
                      t.obj$obs2$data$wind_speed,
                      t.obj$obs$data$wind_speed,
                      dummy,
                      t.obj$herz116$data$wind_speed,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("Lind 10m", "C-REA6 10m", "", "Lind 20m",
                              "C-REA6 35m", "Lind 40m", "Lind 60m", "C-REA6 69m",
                              "Lind 80m", "Lind 98m", "", "C-REA6 116m"),
                      col=c("red", "blue", "", "red", "blue", "red", "red",
                            "blue", "red", "red", "", "blue"))
    } else {
      boxplot.default(t.obj$obs6$data$wind_speed,
                      t.obj$herz10$data$wind_speed,
                      t.obj$eraI10$data$wind_speed,
                      t.obj$era20c10$data$wind_speed,
                      t.obj$obs5$data$wind_speed,
                      t.obj$herz35$data$wind_speed,
                      t.obj$obs4$data$wind_speed,
                      t.obj$obs3$data$wind_speed,
                      t.obj$herz69$data$wind_speed,
                      t.obj$obs2$data$wind_speed,
                      t.obj$obs$data$wind_speed,
                      t.obj$era20c100$data$wind_speed,
                      t.obj$eraI100$data$wind_speed,
                      t.obj$herz116$data$wind_speed,
                      dummy,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("Lind 10m", "C-REA6 10m", "ERA-I 10m", "ERA20C 10m",
                              "Lind 20m", "C-REA6 35m", "Lind 40m", "Lind 60m",
                              "C-REA6 69m", "Lind 80m", "Lind 98m", "ERA20C 100m",
                              "ERA-I 100m", "C-REA6 116m", ""),
                      col=c("red", "blue", "green", "green", "red", "blue", "red", "red",
                            "blue", "red", "red", "green", "green", "blue", ""))
    }
  } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
    if (ana.time.res$time.res == ana.time.res$hourly) {
      boxplot.default(t.obj$obs6$data$wind_speed,
                      t.obj$herz10$data$wind_speed,
                      dummy,
                      t.obj$obs5$data$wind_speed,
                      t.obj$herz35$data$wind_speed,
                      t.obj$obs4$data$wind_speed,
                      t.obj$herz69$data$wind_speed,
                      t.obj$obs3$data$wind_speed,
                      dummy,
                      t.obj$herz116$data$wind_speed,
                      t.obj$obs2$data$wind_speed,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("Cabauw 10m", "C-REA6 10m", "", "Cabauw 20m",
                              "C-REA6 35m", "Cabauw 40m", "C-REA6 69m", "Cabauw 80m",
                              "", "C-REA6 116m", "Cabauw 140m"),
                      col=c("red", "blue", "", "red", "blue", "red",
                            "blue", "red", "", "blue", "red"))
    } else {
      boxplot.default(t.obj$obs6$data$wind_speed,
                      t.obj$herz10$data$wind_speed,
                      t.obj$eraI10$data$wind_speed,
                      t.obj$era20c10$data$wind_speed,
                      t.obj$obs5$data$wind_speed,
                      t.obj$herz35$data$wind_speed,
                      t.obj$obs4$data$wind_speed,
                      dummy,
                      t.obj$herz69$data$wind_speed,
                      t.obj$obs3$data$wind_speed,
                      dummy,
                      t.obj$era20c100$data$wind_speed,
                      t.obj$eraI100$data$wind_speed,
                      t.obj$herz116$data$wind_speed,
                      t.obj$obs2$data$wind_speed,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("Cabauw 10m", "C-REA6 10m", "ERA-I 10m", "ERA20C 10m",
                              "Cabauw 20m", "C-REA6 35m", "Cabauw 40m", "",
                              "C-REA6 69m", "Cabauw 80m", "", "ERA20C 100m",
                              "ERA-I 100m", "C-REA6 116m", "Cabauw 140m"),
                      col=c("red", "blue", "green", "green", "red", "blue", "red",
                            "", "blue", "red", "", "green", "green", "blue", "red"))
    }
  } else if (t.obj$obs$data$StationName[1] == "Fino1") {
    if (ana.time.res$time.res == ana.time.res$hourly) {
      boxplot.default(dummy,
                      t.obj$herz10$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$herz35$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$herz69$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$obs$data$wind_speed,
                      t.obj$herz116$data$wind_speed,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("", "C-REA6 10m", "", "", "C-REA6 35m", "", "",
                              "C-REA6 69m", "", "", "Fino1 100m", "C-REA6 116m"),
                      col=c("", "blue", "", "", "blue", "", "", "blue", "",
                            "", "red", "blue"))
    } else {
      boxplot.default(dummy,
                      t.obj$herz10$data$wind_speed,
                      t.obj$eraI10$data$wind_speed,
                      t.obj$era20c10$data$wind_speed,
                      dummy,
                      t.obj$herz35$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$herz69$data$wind_speed,
                      dummy,
                      t.obj$era20c100$data$wind_speed,
                      t.obj$eraI100$data$wind_speed,
                      t.obj$obs$data$wind_speed,
                      t.obj$herz116$data$wind_speed,
                      dummy,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("", "C-REA6 10m", "ERA-I 10m", "ERA20C 10m", "",
                              "C-REA6 35m", "", "", "C-REA6 69m", "", "Fino1 100m",
                              "ERA20C 100m", "ERA-I 100m", "C-REA6 116m", ""),
                      col=c("", "blue", "green", "green", "", "blue", "", "", "blue", "",
                            "red", "green", "green", "blue", ""))
    }
  } else if (t.obj$obs$data$StationName[1] == "Fino2") {
    if (ana.time.res$time.res == ana.time.res$hourly) {
      boxplot.default(dummy,
                      t.obj$herz10$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$herz35$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$herz69$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$obs$data$wind_speed,
                      t.obj$herz116$data$wind_speed,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("", "C-REA6 10m", "", "", "C-REA6 35m", "", "",
                              "C-REA6 69m", "", "", "Fino2 102m", "C-REA6 116m"),
                      col=c("", "blue", "", "", "blue", "", "", "blue", "",
                            "", "red", "blue"))
    } else {
      boxplot.default(dummy,
                      t.obj$herz10$data$wind_speed,
                      t.obj$eraI10$data$wind_speed,
                      t.obj$era20c10$data$wind_speed,
                      dummy,
                      t.obj$herz35$data$wind_speed,
                      dummy,
                      dummy,
                      t.obj$herz69$data$wind_speed,
                      dummy,
                      t.obj$era20c100$data$wind_speed,
                      t.obj$eraI100$data$wind_speed,
                      t.obj$obs$data$wind_speed,
                      t.obj$herz116$data$wind_speed,
                      dummy,
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=wind.range,
                      names=c("", "C-REA6 10m", "ERA-I 10m", "ERA20C 10m", "",
                              "C-REA6 35m", "", "", "C-REA6 69m", "", "ERA20C 100m",
                              "ERA-I 100m", "Fino2 102m", "C-REA6 116m", ""),
                      col=c("", "blue", "green", "green", "", "blue", "", "", "blue", "",
                            "green", "green", "red", "blue", ""))
    }
  } else {
    CallStop(paste0("Unexpected tower name: ",
                    tower.obj$climate_data_objects$obs$data$StationName[1], " "))
  }
  mtext(paste0(tit.ext, " wind speed\nprofile ",
               t.obj$obs$data$StationName[1]), side=3, line=1, cex=4)
  mtext(x.labs, side=1, line=4, cex=2.75)
  dev.off()
}

#-----------------------------------------------------------------------------------

get_labels <- function(tower.date) {

  beg.mon = tower.date[1]$mon + 1
  beg.year = tower.date[1]$year + 1900
  if (beg.mon != 1) {
    beg.year = beg.year + 1
    beg.mon = 1
  }
  beg.mon.str = month.abb[beg.mon]

  end.mon = tail(tower.date, 1)$mon + 1
  end.year = tail(tower.date, 1)$year + 1900

  tower.labels = c(paste0(beg.mon.str, "\n", as.character(beg.year)))
  time.str = paste0(sprintf("%04d", beg.year), sprintf("%02d", beg.mon), "01")
  tower.times = c(as.integer(as.POSIXct(strptime(time.str, format="%Y%m%d"), tz="UTC")))
  for (cnt in seq(end.year - beg.year)) {
    year = beg.year + cnt
    tower.labels = append(tower.labels, paste0("Jan\n", as.character(year)))
    time.str = paste0(as.character(year), "0101")
    tower.times = append(tower.times, as.integer(as.POSIXct(strptime(time.str, format="%Y%m%d"), tz="UTC")))
  }
  tower.labels = append(tower.labels, paste0("Dec\n", as.character(year)))
  time.str = paste0(as.character(year), "1201")
  tower.times = append(tower.times, as.integer(as.POSIXct(strptime(time.str, format="%Y%m%d"), tz="UTC")))

  return(list(tower.labels=tower.labels, tower.times=tower.times))
}

#-----------------------------------------------------------------------------------

#' @title Plot relative difference between tower measurements and reanalysis data.
#' @description Plot the relative difference between tower measurements at 10m and
#'   100m compared to reanalysis data. Tower measurements of Lindenberg, Cabauw,
#'   Fino1, and Fino2 are supported.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname is a string holding file name of the pdf plot to be created.
#' @importFrom xts xts
#' @export
PlotTowerERAprofileRelDiff <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  cex.legend = 1.75
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

  yliml.rel = -0.75
  ylimh.rel = 0.75
  color = list(obs="blue", herz="red", era="green3", black="black")

  paper.axes = T

  if (t.obj$obs$data$StationName[1] == "Lindenberg") {
    plot.10.ext = paste0(t.obj$obs6$data$StationName[1], " at ",
                         t.obj$obs6$data$height[1])
    plot.100.ext = paste0(t.obj$obs$data$StationName[1], " at ",
                          t.obj$obs$data$height[1])
  } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
    plot.10.ext = paste0(t.obj$obs6$data$StationName[1], " at ",
                         t.obj$obs6$data$height[1])
    plot.100.ext = paste0(t.obj$obs3$data$StationName[1], " at ",
                          t.obj$obs3$data$height[1])
  } else if (t.obj$obs$data$StationName[1] == "Fino1" |
             t.obj$obs$data$StationName[1] == "Fino2") {
    plot.100.ext = paste0(t.obj$obs$data$StationName[1], " at ",
                          t.obj$obs$data$height[1])
  } else {
    CallStop(paste0("Unexpected tower name: ", t.obj$obs$data$StationName[1], " "))
  }

  PS = PlottingSettings(t.obj$herz116$data)
  dummy = numeric(length=length(tower.date)) * NA
  dummy = xts(dummy, order.by=tower.date)

  # == relative TS in 100m height ==
  fname.new = gsub("relativeDifferences", "relativeDifferences-100m", fname)
  pdf(fname.new, width=PS$port.a4width/0.67, height=PS$port.a4height)
  par(mfrow=c(3,1), mar=c(0,5.5,0,0.5), oma=c(4,0,5,0), cex=1.75, cex.lab=1.5, cex.axis=1.5)

  h.xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  if (t.obj$obs$data$StationName[1] == "Lindenberg")
    l.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
  if (t.obj$obs$data$StationName[1] == "Cabauw")
    l.xts = xts(t.obj$obs3$data$wind_speed, order.by=tower.date)
  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2")
    l.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)

  plot(dummy, main=NULL, xaxt="n", ylim=c(yliml.rel, ylimh.rel), las=1)
  lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
  lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$obs)
  corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
  cat(paste0("\n116m C-REA6 at station: ", t.obj$obs$data$StationName[1],
             "\n    correlation = ", round(corr$estimate, 4),
             "\n    conf interv = ", round(corr$conf.int[[1]], 4), "  ",
             round(corr$conf.int[[2]], 4), "\n"))
  legend("bottomleft", legend=c("COSMO-REA6 at 116m", plot.100.ext,
                                paste0("correlation = ", round(corr$estimate, 2))),
         text.col=c(color$herz, color$obs, color$black), horiz=T)

  h.xts = xts(t.obj$eraI100$data$wind_speed, order.by=tower.date)
  l.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
  plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), xaxt='n', las=1)
  lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$era)
  lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$obs)
  corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
  cat(paste0("\n100m ERA-I at station: ", t.obj$obs$data$StationName[1],
             "\n    correlation = ", round(corr$estimate, 4),
             "\n    conf interv = ", round(corr$conf.int[[1]], 4), "  ",
             round(corr$conf.int[[2]], 4), "\n"))
  legend("bottomleft", legend=c("Era-Interim at 100m", plot.100.ext,
                                paste0("correlation = ", round(corr$estimate, 2))),
         text.col=c(color$era, color$obs, color$black), horiz=T)
  mtext("relative difference", side=2, line=4, cex=3)

  h.xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  #l.xts from above
  if (paper.axes) {
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), col.axis="white")
    out.put = get_labels(tower.date)
    axis(side=1, labels=out.put$tower.labels, at=out.put$tower.times, mgp=c(3,3,0))
    axis(side=2, labels=c(-0.5, 0.0, 0.5), at=c(-0.5, 0.0, 0.5), las=1)
  } else {
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), las=1)
  }
  lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$era)
  lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$obs)
  corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
  cat(paste0("\n100m ERA20C at station: ", t.obj$obs$data$StationName[1],
             "\n    correlation = ", round(corr$estimate, 4),
             "\n    conf interv = ", round(corr$conf.int[[1]], 4), "  ",
             round(corr$conf.int[[2]], 4), "\n"))
  legend("bottomleft", legend=c("ER20C at 100m", plot.100.ext,
                                paste0("correlation = ", round(corr$estimate, 2))),
         text.col=c(color$era, color$obs, color$black), horiz=T)

  mtext(paste0("Monthly relative wind speed\ndifferences at ",
               t.obj$obs$data$StationName[1]),
        outer=TRUE, line=1, cex=3.75)

  dev.off()

  # == relative TS in 10m height ==
  if (t.obj$obs$data$StationName[1] == "Lindenberg" |
      t.obj$obs$data$StationName[1] == "Cabauw") {
    fname.new = gsub("relativeDifferences", "relativeDifferences-10m", fname)
    pdf(fname.new, width=PS$port.a4width/0.67, height=PS$port.a4height)
    # par(mfrow=c(3,1), mar=c(0,11,0,0.5), oma=c(9,0,15,0.5), cex.lab=4, cex.axis=4)
    par(mfrow=c(3,1), mar=c(0,5.5,0,0.5), oma=c(4,0,5,0), cex=1.75, cex.lab=1.5, cex.axis=1.5)

    h.xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
    l.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), xaxt='n', las=1)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$herz)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$obs)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    cat(paste0("\n10m C-REA6 at station: ", t.obj$obs$data$StationName[1],
               "\n    correlation = ", round(corr$estimate, 4),
               "\n    conf interv = ", round(corr$conf.int[[1]], 4), "  ",
               round(corr$conf.int[[2]], 4), "\n"))
    legend("bottomleft", legend=c("COSMO-REA6 at 10m", plot.10.ext,
                                  paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$herz, color$obs, color$black), horiz=T)

    h.xts = xts(t.obj$eraI10$data$wind_speed, order.by=tower.date)
    l.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
    plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), xaxt='n', las=1)
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col=color$era)
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$obs)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    cat(paste0("\n10m ERA-I at station: ", t.obj$obs$data$StationName[1],
               "\n    correlation = ", round(corr$estimate, 4),
               "\n    conf interv = ", round(corr$conf.int[[1]], 4), "  ",
               round(corr$conf.int[[2]], 4), "\n"))
    legend("bottomleft", legend=c("Era-Interim at 10m", plot.10.ext,
                                  paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c(color$era, color$obs, color$black), horiz=T)
    mtext("relative difference", side=2, line=4, cex=3)

    h.xts = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
    l.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
    if (paper.axes) {
      plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), col.axis="white")
      out.put = get_labels(tower.date)
      axis(side=1, labels=out.put$tower.labels, at=out.put$tower.times, mgp=c(3,3,0))
      axis(side=2, labels=c(-0.5, 0.0, 0.5), at=c(-0.5, 0.0, 0.5), las=1)
    } else {
      plot(dummy, main=NULL, ylim=c(yliml.rel, ylimh.rel), las=1)
    }
    lines(RelDiff(h.xts, mean(h.xts)), type="b", pch=16, col="cyan2")
    lines(RelDiff(l.xts, mean(l.xts)), type="b", pch=16, col=color$obs)
    corr = cor.test(as.numeric(h.xts), as.numeric(l.xts))
    cat(paste0("\n10m ERA20C at station: ", t.obj$obs$data$StationName[1],
               "\n    correlation = ", round(corr$estimate, 4),
               "\n    conf interv = ", round(corr$conf.int[[1]], 4), "  ",
               round(corr$conf.int[[2]], 4), "\n"))
    legend("bottomleft", legend=c("Era20C at 10m", plot.10.ext,
                                  paste0("correlation = ", round(corr$estimate, 2))),
           text.col=c("cyan2", color$obs, color$black), horiz=T)

    mtext(paste0("Monthly relative wind speed\ndifferences at ",
                 t.obj$obs$data$StationName[1]), outer=TRUE, line=1, cex=3.75)
    dev.off()
  }
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
#' @param fname is a string holding file name of the pdf plot to be created.
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
PlotTowerERAprofileAnnualVar <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  legend.cex = 0.75
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

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

  PS = PlottingSettings(t.obj$herz116$data)

  for (cnt in seq(12)) {
    mon.Era20c100[[cnt]] = Era20c100Xts[which( tower.date$mon==cnt-1 )]
    mon.Herz116[[cnt]] = Herz116Xts[which( tower.date$mon==cnt-1 )]
    mon.Herz10[[cnt]] = Herz10Xts[which( tower.date$mon==cnt-1 )]
  }

  if (t.obj$obs$data$StationName[1] == "Lindenberg" |
      t.obj$obs$data$StationName[1] == "Cabauw") {

    if (t.obj$obs$data$StationName[1] == "Lindenberg") {
      plot.ext.10 = "Lindenberg at 10m"
      plot.ext.100 = "Lindenberg at 98m"
      mon.tower.100 = list()
      mon.tower.10 = list()
      Lind98Xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
      Lind10Xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
      for (cnt in seq(12)) {
        mon.tower.100[[cnt]] = Lind98Xts[which( tower.date$mon==cnt-1 )]
        mon.tower.10[[cnt]] = Lind10Xts[which( tower.date$mon==cnt-1 )]
      }
    } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
      plot.ext.100 = "Cabauw at 80m"
      plot.ext2.100 = "Cabauw at 140m"
      plot.ext.10 = "Cabauw at 10m"
      mon.tower.100 = list()
      mon.tower2.100 = list()
      mon.tower.10 = list()
      Cabauw10Xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
      Cabauw80Xts = xts(t.obj$obs3$data$wind_speed, order.by=tower.date)
      Cabauw140Xts = xts(t.obj$obs2$data$wind_speed, order.by=tower.date)
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

    pdf(fname, width=PS$port.a4width, height=PS$port.a4height)
    if (t.obj$obs$data$StationName[1] == "Lindenberg") {
      par(mfrow=c(5,1), mar=c(0,2,0,0), oma=c(4,1,3,0.5), cex=0.9)
    } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
      par(mfrow=c(6,1), mar=c(0,2,0,0), oma=c(4,1,3,0.5), cex=0.9)
    }
    dummy = numeric(length=length(Era20c100Xts)) * NA
    dummy = xts(dummy, order.by = index(Era20c100Xts))

    if (t.obj$obs$data$StationName[1] == "Cabauw") {
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
    legend("bottomleft", legend=c(paste0("COSMO-REA6 at 116m ", all.months[months[[1]]]),
                                  paste0("COSMO-REA6 at 116m ", all.months[months[[2]]])),
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
    legend("topleft", legend=c(paste0("COSMO-REA6 at 10m ", all.months[months[[1]]]),
                               paste0("COSMO-REA6 at 10m ", all.months[months[[2]]])),
           text.col=colorH, cex=legend.cex)

    if (t.obj$obs$data$StationName[1] == "Lindenberg") {
      mtext("Wind speed at Lindenberg for different months",
            outer=TRUE, line=1, cex=1.0)
    } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
      mtext("Wind speed at Cabauw for different months",
            outer=TRUE, line=1, cex=1.0)
    }
    mtext("wind speed [m/s]", line=0, side=2, outer=T)

    dev.off()

  } else if (t.obj$obs$data$StationName[1] == "Fino1" |
             t.obj$obs$data$StationName[1] == "Fino2") {

    plot.ext = paste0(t.obj$obs$data$StationName[1], " at ",
                      t.obj$obs$data$height[1], "m ")
    mon.tower = list()
    towerXts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)

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

    pdf(fname, width=PS$land.a4width/2., height=PS$land.a4height)
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
    legend("bottomleft", legend=c(paste0("COSMO-REA6 at 116m ", all.months[months[[1]]]),
                                  paste0("COSMO-REA6 at 116m ", all.months[months[[2]]])),
           text.col=colorH, cex=legend.cex)

    plot(dummy, main=NULL, ylim=c(yliml, ylimh), las=1)
    for (cnt in seq(months)) {
      lines(mon.Era20c100[[months[[cnt]]]], type="b", pch=16, col=colorE[cnt],
            bg=rgb(0,0,0,1./cnt), lw=2)
    }
    legend("bottomleft", legend=c(paste0("Era20C at 100m ", all.months[months[[1]]]),
                                  paste0("Era20C at 100m ", all.months[months[[2]]])),
           text.col=colorE, cex=legend.cex)

    mtext(paste0("wind speed at ", t.obj$obs$data$StationName[1],
                 " at 100m for different months"),
          outer=TRUE, line=1, cex=1.0)
    mtext("wind speed [m/s]", line=0, side=2, outer=TRUE)

    dev.off()

  } else {
    CallStop(paste0("Unexpected tower name: ", t.obj$obs$data$StationName[1], " "))
  }
}


#-----------------------------------------------------------------------------------

#' @title Plot annual cycle between tower measurements and reanalysis data.
#' @description Plot the annual cycle between tower measurements and reanalysis data
#'   at all available heights for absolute and relative values.  Tower measurements
#'   of Lindenberg, Cabauw, Fino1, and Fino2 are supported.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname is a string holding file name of the pdf plot to be created.
#' @importFrom xts xts
#' @importFrom zoo as.yearmon
#' @export
PlotTowerERAprofileAnnualCycle <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  legend.cex = 1.75
  xylab.cex = 1.7
  title.cex = 2.75
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

  all.months = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                 "Aug","Sep","Oct","Nov","Dec")

  date.ancycle = as.yearmon(2000 + seq(0, 11)/12)

  mon.Era20c100 = vector(mode="numeric", length=12)
  mon.EraI100 = vector(mode="numeric", length=12)
  mon.Herz116 = vector(mode="numeric", length=12)
  mon.Herz69 = vector(mode="numeric", length=12)
  mon.Herz35 = vector(mode="numeric", length=12)
  mon.Herz10 = vector(mode="numeric", length=12)
  mon.Era20c10 = vector(mode="numeric", length=12)
  mon.EraI10 = vector(mode="numeric", length=12)
  Era20c100Xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  EraI100Xts = xts(t.obj$eraI100$data$wind_speed, order.by=tower.date)
  Herz116Xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  Herz69Xts = xts(t.obj$herz69$data$wind_speed, order.by=tower.date)
  Herz35Xts = xts(t.obj$herz35$data$wind_speed, order.by=tower.date)
  Herz10Xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
  Era20c10Xts = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
  EraI10Xts = xts(t.obj$eraI10$data$wind_speed, order.by=tower.date)

  for (cnt in seq(12)) {
    mon.Era20c100[cnt] = mean(Era20c100Xts[which( tower.date$mon==cnt-1 )])
    mon.EraI100[cnt] = mean(EraI100Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz116[cnt] = mean(Herz116Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz69[cnt] = mean(Herz69Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz35[cnt] = mean(Herz35Xts[which( tower.date$mon==cnt-1 )])
    mon.Herz10[cnt] = mean(Herz10Xts[which( tower.date$mon==cnt-1 )])
    mon.Era20c10[cnt] = mean(Era20c10Xts[which( tower.date$mon==cnt-1 )])
    mon.EraI10[cnt] = mean(EraI10Xts[which( tower.date$mon==cnt-1 )])
  }

  PS = PlottingSettings(t.obj$herz116$data)

  if (t.obj$obs$data$StationName[1] == "Lindenberg") {

    yliml.rel = -0.25
    ylimh.rel = 0.25

    mon.Lind98 = vector(mode="numeric", length=12)
    mon.Lind60 = vector(mode="numeric", length=12)
    mon.Lind40 = vector(mode="numeric", length=12)
    mon.Lind10 = vector(mode="numeric", length=12)
    Lind98Xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
    Lind60Xts = xts(t.obj$obs3$data$wind_speed, order.by=tower.date)
    Lind40Xts = xts(t.obj$obs4$data$wind_speed, order.by=tower.date)
    Lind10Xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
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

    pdf(fname, width=PS$land.a4width/2., height=PS$land.a4height)
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
    #          paste0("C-REA6 at 116m with corr = ",round(corr2$estimate, 2))),
    #    text.col=c("blue", "green", "red"), cex=legend.cex)
    legend("top", legend=c("Lind at 98m ", "Era20C at 100m", "C-REA6 at 116m"),
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
    legend("top", legend=c("Lind at 60m", "C-REA6 at 69m"),
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
    legend("top", legend=c("Lind at 40m", "C-REA6 at 35m"),
           text.col=c("blue", "red"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, 6.0), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Lind10, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz10, type="b", pch=16, col="red", lw=2)
    lines(mon.Era20c10, type="b", pch=16, col="green", lw=2)
    lines(mon.EraI10, type="b", pch=16, col="black", lw=2)
    arrows(1:12, mon.Lind10-sd(mon.Lind10), 1:12, mon.Lind10+sd(mon.Lind60),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="red")
    arrows(1:12, mon.Era20c10-sd(mon.Era20c10), 1:12, mon.Era20c10+sd(mon.Era20c10),
           length=0.05, angle=90, code=3, col="green")
    arrows(1:12, mon.EraI10-sd(mon.EraI10), 1:12, mon.EraI10+sd(mon.EraI10),
           length=0.05, angle=90, code=3, col="black")
    legend("top", legend=c("Lind at 10m", "C-REA6 at 10m", "Era20C at 10m", "Era-I at 10m"),
           text.col=c("blue", "red", "green", "black"), cex=legend.cex)

    axis(1, labels=all.months, at = 1:12)

    dev.off()
    ylimh = ylimh - 1


    fname = gsub("annualCycle", "annualCycleSingleArrow", fname)
    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
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
    legend("top", legend=c("C-REA6 at 116m", "Lind at 98m ", "Era20C at 100m",
                           "C-REA6 at 69m", "Lind at 60m", "Lind at 40m", "C-REA6 at 35m",
                           "Lind at 10m", "C-REA6 at 10m"),
           text.col=c("violetred", "purple2", "green", "chocolate", "deepskyblue",
                      "blue", "red", "darkturquoise", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingleArrow", "annualCycleRelDiffSingle", fname)
    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
    par(mar=c(3,6,6.5,0.5), cex.lab=2, cex.axis=2)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         xlab="", ylab="", main="")
    title(ylab="relative difference", line=4, cex=xylab.cex)
    axis(1, labels=all.months, at = 1:12, las=1, cex=xylab.cex)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel), las=1)
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16,
          col="red",lw=2)
    lines(RelDiff(mon.Lind98, mean(mon.Lind98)), type="b", pch=16,
          col="purple2",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16,
          col="darkgreen",lw=2)
    lines(RelDiff(mon.EraI100, mean(mon.EraI100)), type="b", pch=16,
          col="magenta",lw=2)
    lines(RelDiff(mon.Lind10, mean(mon.Lind10)), type="b", pch=16,
          col="blue",lw=2)
    lines(RelDiff(mon.EraI10, mean(mon.EraI10)), type="b", pch=16,
          col="green",lw=2)
    lines(RelDiff(mon.Era20c10, mean(mon.Era20c10)), type="b", pch=16,
          col="black",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16,
          col="darkorange",lw=2)
    legend("top", legend=c("C-REA6 at 116m", "Era20C at 100m", "Era-I at 100m", "Lind at 98m ",
                           # "C-REA6 at 69m", "Lind at 60m", "Lind at 40m", "C-REA6 at 35m",
                           "C-REA6 at 10m", "Era20C at 10m", "Era-I at 10m", "Lind at 10m"),
           text.col=c("red", "darkgreen", "magenta", "purple2",
                      # "chocolate", "deepskyblue", "blue", "red",
                      "darkorange", "black", "green", "blue"), cex=legend.cex)
    mtext("Annual cycle of relative\nwind speed at Lindenberg", line=1, cex=title.cex)

    dev.off()

  } else if (t.obj$obs$data$StationName[1] == "Cabauw") {

    yliml.rel = -0.25
    ylimh.rel = 0.25

    mon.Cabauw140 = vector(mode="numeric", length=12)
    mon.Cabauw80 = vector(mode="numeric", length=12)
    mon.Cabauw40 = vector(mode="numeric", length=12)
    mon.Cabauw20 = vector(mode="numeric", length=12)
    mon.Cabauw10 = vector(mode="numeric", length=12)
    Cabauw140Xts = xts(t.obj$obs2$data$wind_speed, order.by=tower.date)
    Cabauw80Xts = xts(t.obj$obs3$data$wind_speed, order.by=tower.date)
    Cabauw40Xts = xts(t.obj$obs4$data$wind_speed, order.by=tower.date)
    Cabauw20Xts = xts(t.obj$obs5$data$wind_speed, order.by=tower.date)
    Cabauw10Xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
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

    pdf(fname, width=PS$land.a4width/2., height=PS$land.a4height)
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
                           "C-REA6 at 116m"),
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
    legend("top", legend=c("Cabauw at 40m", "C-REA6 at 35m", "Cabauw at 20m"),
           text.col=c("blue", "red", "lightblue"), cex=legend.cex)

    plot(dummy, xlim=c(1,12), ylim=c(yliml, 6), col.axis = "white",
         xlab="", xaxt="n", ylab="", main="")
    axis(2, labels=yliml:(ylimh-1), at=yliml:(ylimh-1), las=1)
    lines(mon.Cabauw10, type="b", pch=16, col="blue", lw=2)
    lines(mon.Herz10, type="b", pch=16, col="red", lw=2)
    lines(mon.Era20c10, type="b", pch=16, col="green", lw=2)
    lines(mon.EraI10, type="b", pch=16, col="black", lw=2)
    arrows(1:12, mon.Cabauw10-sd(mon.Cabauw10), 1:12, mon.Cabauw10+sd(mon.Cabauw10),
           length=0.05, angle=90, code=3, col="blue")
    arrows(1:12, mon.Herz10-sd(mon.Herz10), 1:12, mon.Herz10+sd(mon.Herz10),
           length=0.05, angle=90, code=3, col="red")
    arrows(1:12, mon.Era20c10-sd(mon.Era20c10), 1:12, mon.Era20c10+sd(mon.Era20c10),
           length=0.05, angle=90, code=3, col="green")
    arrows(1:12, mon.EraI10-sd(mon.EraI10), 1:12, mon.EraI10+sd(mon.EraI10),
           length=0.05, angle=90, code=3, col="black")
    legend("top", legend=c("Cabauw at 10m", "C-REA6 at 10m", "Era20C at 10m", "Era-I at 10m"),
           text.col=c("blue", "red", "green", "black"), cex=legend.cex)

    axis(1, labels=all.months, at = 1:12)

    dev.off()
    ylimh = ylimh - 1


    fname = gsub("annualCycle", "annualCycleSingleArrow", fname)
    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
    par(mar=c(3,3,3,0.5), cex=1.8)
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
    legend("top", legend=c("Cabauw at 140m", "C-REA6 at 116m", "Era20C at 100m",
                           "Cabauw at 80m ", "C-REA6 at 69m", "Cabauw at 40m",
                           "C-REA6 at 35m", "Cabauw at 20m", "Cabauw at 10m",
                           "C-REA6 at 10m"),
           text.col=c("black", "violetred", "green", "purple2", "chocolate",
                      "deepskyblue", "red", "blue", "darkturquoise", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingleArrow", "annualCycleRelDiffSingle", fname)
    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
    par(mar=c(3,6,6.5,0.5), cex.lab=2, cex.axis=2)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         xlab="", ylab="", main="")
    title(ylab="relative difference", line=4, cex=xylab.cex)
    axis(1, labels=all.months, at = 1:12, las=1, cex=xylab.cex)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel), las=1)
    lines(RelDiff(mon.Cabauw140, mean(mon.Cabauw140)), type="b", pch=16,
          col="cyan2",lw=2)
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16,
          col="red",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16,
          col="darkgreen",lw=2)
    lines(RelDiff(mon.EraI100, mean(mon.EraI100)), type="b", pch=16,
          col="magenta",lw=2)
    lines(RelDiff(mon.Cabauw80, mean(mon.Cabauw80)), type="b", pch=16,
          col="purple2",lw=2)
    lines(RelDiff(mon.Cabauw10, mean(mon.Cabauw10)), type="b", pch=16,
          col="darkorange",lw=2)
    lines(RelDiff(mon.EraI10, mean(mon.EraI10)), type="b", pch=16,
          col="black",lw=2)
    lines(RelDiff(mon.Era20c10, mean(mon.Era20c10)), type="b", pch=16,
          col="green",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16,
          col="blue",lw=2)
    legend("top", legend=c("Cabauw at 140m", "C-REA6 at 116m", "Era20C at 100m",
                           "Era-I at 100m", "Cabauw at 80m ",
                           "C-REA6 at 10m", "Era20C at 10m", "Era-I at 10m", "Cabauw at 10m"),
           text.col=c("cyan2", "red", "darkgreen", "magenta", "purple2",
                      "darkorange", "black", "green", "blue"), cex=legend.cex)
    mtext("Annual cycle of relative\nwind speed at Cabauw", line=1, cex=title.cex)

    dev.off()

  } else if ( t.obj$obs$data$StationName[1] == "Fino1" |
              t.obj$obs$data$StationName[1] == "Fino2") {

    yliml.rel = -0.3
    ylimh.rel = 0.3

    plot.ext = paste0(t.obj$obs$data$StationName[1], " at ",
                      t.obj$obs$data$height[1], "m ")

    mon.tower = vector(mode="numeric", length=12)
    towerXts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
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

    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
    par(mar=c(3,3,3,0.5), cex=1.1)


    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", ylab="", main="")
    yliml = yliml - 2
    title(main=paste0("Annual cycle of wind speed at ",
                      t.obj$obs$data$StationName[1]), line=1, cex=1.1)
    title(ylab="wind speed [m/s]", line=2)
    axis(1, labels=all.months, at = 1:12, las=1)
    axis(2, labels=yliml:ylimh, at=yliml:ylimh, las=1)
    lines(mon.tower, type="b", pch=16, col="blue",lw=2)
    lines(mon.Era20c100, type="b", pch=16, col="green",lw=2)
    lines(mon.Herz116, type="b", pch=16, col="red",lw=2)
    legend("top", legend=c(plot.ext, "Era20C at 100m ", "C-REA6 at 116m"),
           text.col=c("blue", "green", "red"),
           cex=legend.cex)

    dev.off()


    fname = gsub("annualCycle", "annualCycleSingleArrow", fname)
    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
    par(mar=c(3,3,3,0.5), cex=1.1)
    ylimh = ylimh + 1
    plot(dummy, xlim=c(1,12), ylim=c(yliml, ylimh), col.axis = "white",
         xlab="", ylab="", main="")
    title(main=paste0("Annual cycle of wind speed at ",
                      t.obj$obs$data$StationName[1]),
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
    legend("top", legend=c("C-REA6 at 116m", plot.ext, "Era20C at 100m",
                           "C-REA6 at 69m", "C-REA6 at 35m",
                           "C-REA6 at 10m"),
           text.col=c("violetred", "purple2", "green", "red", "orange"), cex=0.9)

    dev.off()


    fname = gsub("annualCycleSingleArrow", "annualCycleRelDiffSingle", fname)
    pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
    par(mar=c(3,6,6.5,0.5), cex.axis=2, cex.lab=2)
    plot(dummy, xlim=c(1,12), ylim=c(yliml.rel, ylimh.rel), col.axis = "white",
         xlab="", ylab="", main="")
    title(ylab="relative difference", line=4, cex=xylab.cex)
    axis(1, labels=all.months, at = 1:12, las=1, cex=xylab.cex)
    axis(2, labels=c(yliml.rel,0,ylimh.rel), at=c(yliml.rel,0,ylimh.rel), las=1)
    lines(RelDiff(mon.Herz116, mean(mon.Herz116)), type="b", pch=16,
          col="red",lw=2)
    lines(RelDiff(mon.tower, mean(mon.tower)), type="b", pch=16,
          col="purple2",lw=2)
    lines(RelDiff(mon.Era20c100, mean(mon.Era20c100)), type="b", pch=16,
          col="darkgreen",lw=2)
    lines(RelDiff(mon.EraI100, mean(mon.EraI100)), type="b", pch=16,
          col="magenta",lw=2)
    lines(RelDiff(mon.EraI10, mean(mon.EraI10)), type="b", pch=16,
          col="green",lw=2)
    lines(RelDiff(mon.Era20c10, mean(mon.Era20c10)), type="b", pch=16,
          col="black",lw=2)
    lines(RelDiff(mon.Herz10, mean(mon.Herz10)), type="b", pch=16,
          col="darkorange",lw=2)
    legend("top", legend=c("C-REA6 at 116m", "Era20C at 100m", "Era-I at 100m", plot.ext,
                           "C-REA6 at 10m", "Era20C at 10m", "Era-I at 10m"),
           text.col=c("red", "darkgreen", "magenta", "purple2", "darkorange", "black", "green"),
           cex=legend.cex)
    mtext(paste0("Annual cycle of relative\nwind speed at ",
                 t.obj$obs$data$StationName[1]), line=1, cex=title.cex)
    dev.off()

  } else {
    CallStop(paste0("Unexpected tower name: ", t.obj$obs$data$StationName[1], " "))
  }
}

#-----------------------------------------------------------------------------------

#' @title Prepare plotting daily cycle of tower measurements.
#' @description Preparing to plot the daily cycle of tower measurements by creating
#'   lists holding the mean and standard deviation of hourly tower data. This is done
#'   for (a) all data and (b) for selected months only.
#' @param tower.obj ClimObject holding tower and corresponding reanalysis data.
#' @param fname is a string holding the file name of the plot.
#' @importFrom xts xts
#' @export
PreparePlottingTowerDailyCycle <- function(tower.obj, fname) {

  # names include all months to be analysed; counts is the month of year
  month.names = list(names=c("January", "March", "June", "September"),
                     counts=as.numeric(c(1,3,6,9)))

  t.obj = tower.obj$climate_data_objects
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

  # extended time series COSMO-REA6 at tower location
  tower1.xts = NULL
  tower2.xts = NULL
  tower3.xts = NULL
  tower4.xts = NULL
  tower5.xts = NULL
  tower6.xts = NULL
  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {
    tower1.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
  } else if (t.obj$obs$data$StationName[1] == "Lindenberg" |
             t.obj$obs$data$StationName[1] == "Cabauw") {
    tower1.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
    tower2.xts = xts(t.obj$obs2$data$wind_speed, order.by=tower.date)
    tower3.xts = xts(t.obj$obs3$data$wind_speed, order.by=tower.date)
    tower4.xts = xts(t.obj$obs4$data$wind_speed, order.by=tower.date)
    tower5.xts = xts(t.obj$obs5$data$wind_speed, order.by=tower.date)
    tower6.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
  }

  # these lists are used for the complete time period
  dayhour.tower1 = list()
  dayhour.tower2 = list()
  dayhour.tower3 = list()
  dayhour.tower4 = list()
  dayhour.tower5 = list()
  dayhour.tower6 = list()
  for (cnt in seq(24)) {
    dayhour.tower1$vals[[cnt]] = tower1.xts[which( tower.date$hour==cnt-1 )]
    dayhour.tower1$mean[[cnt]] = mean(dayhour.tower1$vals[[cnt]], na.rm=T)
    dayhour.tower1$sd[[cnt]] = sd(dayhour.tower1$vals[[cnt]], na.rm=T)
    if (!is.null(tower6.xts)) {
      dayhour.tower2$vals[[cnt]] = tower2.xts[which( tower.date$hour==cnt-1 )]
      dayhour.tower2$mean[[cnt]] = mean(dayhour.tower2$vals[[cnt]], na.rm=T)
      dayhour.tower2$sd[[cnt]] = sd(dayhour.tower2$vals[[cnt]], na.rm=T)
      dayhour.tower3$vals[[cnt]] = tower3.xts[which( tower.date$hour==cnt-1 )]
      dayhour.tower3$mean[[cnt]] = mean(dayhour.tower3$vals[[cnt]], na.rm=T)
      dayhour.tower3$sd[[cnt]] = sd(dayhour.tower3$vals[[cnt]], na.rm=T)
      dayhour.tower4$vals[[cnt]] = tower4.xts[which( tower.date$hour==cnt-1 )]
      dayhour.tower4$mean[[cnt]] = mean(dayhour.tower4$vals[[cnt]], na.rm=T)
      dayhour.tower4$sd[[cnt]] = sd(dayhour.tower4$vals[[cnt]], na.rm=T)
      dayhour.tower5$vals[[cnt]] = tower5.xts[which( tower.date$hour==cnt-1 )]
      dayhour.tower5$mean[[cnt]] = mean(dayhour.tower5$vals[[cnt]], na.rm=T)
      dayhour.tower5$sd[[cnt]] = sd(dayhour.tower5$vals[[cnt]], na.rm=T)
      dayhour.tower6$vals[[cnt]] = tower6.xts[which( tower.date$hour==cnt-1 )]
      dayhour.tower6$mean[[cnt]] = mean(dayhour.tower6$vals[[cnt]], na.rm=T)
      dayhour.tower6$sd[[cnt]] = sd(dayhour.tower6$vals[[cnt]], na.rm=T)
    }
  }

  PST1 = PlottingSettings(t.obj$obs$data)
  if (!is.null(tower6.xts)) {
    PST2 = PlottingSettings(t.obj$obs2$data)
    PST3 = PlottingSettings(t.obj$obs3$data)
    PST4 = PlottingSettings(t.obj$obs4$data)
    PST5 = PlottingSettings(t.obj$obs5$data)
    PST6 = PlottingSettings(t.obj$obs6$data)
    PS = list(PST1=PST1, PST2=PST2, PST3=PST3, PST4=PST4, PST5=PST5, PST6=PST6)
  } else {
    PS = list(PST1=PST1)
  }
  fname.new = c(gsub("DailyCycle", "DailyCycle_allTime-line", fname),
                gsub("DailyCycle", "DailyCycle_allTime-boxPlot", fname))
  PlotDailyCycleTower(dayhour.tower1, dayhour.tower2, dayhour.tower3,
                      dayhour.tower4, dayhour.tower5, dayhour.tower6,
                      month.names, PS, fname.new)


  # these lists are used for specific months only time periods
  dayhour.month.tower1 = list()
  dayhour.month.tower2 = list()
  dayhour.month.tower3 = list()
  dayhour.month.tower4 = list()
  dayhour.month.tower5 = list()
  dayhour.month.tower6 = list()
  for (cnt in seq((month.names$names))) {
    dummyT1 = list()
    dummyT2 = list()
    dummyT3 = list()
    dummyT4 = list()
    dummyT5 = list()
    dummyT6 = list()
    for (tstep in seq(24)) {
      dummyT1$vals[[tstep]] = tower1.xts[which( tower.date$hour==tstep-1 &
                                                  tower.date$mon==month.names$counts[cnt])]
      dummyT1$mean[[tstep]] = mean(dummyT1$vals[[tstep]], na.rm=T)
      dummyT1$sd[[tstep]] = sd(dummyT1$vals[[tstep]], na.rm=T)

      if (!is.null(tower6.xts)) {
        dummyT2$vals[[tstep]] = tower2.xts[which( tower.date$hour==tstep-1 &
                                                    tower.date$mon==month.names$counts[cnt])]
        dummyT2$mean[[tstep]] = mean(dummyT2$vals[[tstep]], na.rm=T)
        dummyT2$sd[[tstep]] = sd(dummyT2$vals[[tstep]], na.rm=T)

        dummyT3$vals[[tstep]] = tower3.xts[which( tower.date$hour==tstep-1 &
                                                    tower.date$mon==month.names$counts[cnt])]
        dummyT3$mean[[tstep]] = mean(dummyT3$vals[[tstep]], na.rm=T)
        dummyT3$sd[[tstep]] = sd(dummyT3$vals[[tstep]], na.rm=T)

        dummyT4$vals[[tstep]] = tower4.xts[which( tower.date$hour==tstep-1 &
                                                    tower.date$mon==month.names$counts[cnt])]
        dummyT4$mean[[tstep]] = mean(dummyT4$vals[[tstep]], na.rm=T)
        dummyT4$sd[[tstep]] = sd(dummyT4$vals[[tstep]], na.rm=T)

        dummyT5$vals[[tstep]] = tower5.xts[which( tower.date$hour==tstep-1 &
                                                    tower.date$mon==month.names$counts[cnt])]
        dummyT5$mean[[tstep]] = mean(dummyT5$vals[[tstep]], na.rm=T)
        dummyT5$sd[[tstep]] = sd(dummyT5$vals[[tstep]], na.rm=T)

        dummyT6$vals[[tstep]] = tower6.xts[which( tower.date$hour==tstep-1 &
                                                    tower.date$mon==month.names$counts[cnt])]
        dummyT6$mean[[tstep]] = mean(dummyT6$vals[[tstep]], na.rm=T)
        dummyT6$sd[[tstep]] = sd(dummyT6$vals[[tstep]], na.rm=T)
      }
    }
    dayhour.month.tower1[[cnt]] = dummyT1
    if (!is.null(tower6.xts)) {
      dayhour.month.tower2[[cnt]] = dummyT2
      dayhour.month.tower3[[cnt]] = dummyT3
      dayhour.month.tower4[[cnt]] = dummyT4
      dayhour.month.tower5[[cnt]] = dummyT5
      dayhour.month.tower6[[cnt]] = dummyT6
    }
  }
  dayhour.month.tower1 = setNames(dayhour.month.tower1, month.names$names)
  if (!is.null(tower6.xts)) {
    dayhour.month.tower2 = setNames(dayhour.month.tower2, month.names$names)
    dayhour.month.tower3 = setNames(dayhour.month.tower3, month.names$names)
    dayhour.month.tower4 = setNames(dayhour.month.tower4, month.names$names)
    dayhour.month.tower5 = setNames(dayhour.month.tower5, month.names$names)
    dayhour.month.tower6 = setNames(dayhour.month.tower6, month.names$names)
  }

  fname.new = c(gsub("DailyCycle", "DailyCycle_selectMonths-line", fname),
                gsub("DailyCycle", "DailyCycle_selectMonths-boxPlot", fname))
  PlotDailyCycleTower(dayhour.month.tower1, dayhour.month.tower2, dayhour.month.tower3,
                      dayhour.month.tower4, dayhour.month.tower5, dayhour.month.tower6,
                      month.names, PS, fname.new)

}

#-----------------------------------------------------------------------------------

#' @title Prepare plotting daily cycle of COSMO-REA6 data.
#' @description Preparing to plot the daily cycle of the COSMO-REA6 RRA by creating
#'   lists holding the mean and standard deviation of hourly COSMO-REA6 data. This is done
#'   for (a) all data and (b) for selected months only.
#' @param tower.obj ClimObject holding tower and corresponding reanalysis data.
#' @param fname is a string holding the file name of the plot.
#' @importFrom xts xts
#' @export
PreparePlottingDifferenceDailyCycle <- function(tower.obj, fname) {

  t.obj = tower.obj$climate_data_objects
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

  # extended time series COSMO-REA6 at tower location
  tower10.xts = NULL
  tower100.xts = NULL
  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {
    tower100.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
  } else if (t.obj$obs$data$StationName[1] == "Lindenberg" |
             t.obj$obs$data$StationName[1] == "Cabauw") {
    tower10.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
    if (t.obj$obs$data$StationName[1] == "Lindenberg") {
      tower100.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
    }
    if (t.obj$obs$data$StationName[1] == "Cabauw") {
      tower100.xts = xts(t.obj$obs2$data$wind_speed, order.by=tower.date)
    }
  }
  Herz116Xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  Herz10Xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
  Era20C10Xts = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
  Era20C100Xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  EraI10Xts = xts(t.obj$eraI10$data$wind_speed, order.by=tower.date)
  EraI100Xts = xts(t.obj$eraI100$data$wind_speed, order.by=tower.date)

  # these lists are used for the complete time period
  dayhourTower100 = list()
  dayhourHerz116 = list()
  dayhourHerz10 = list()
  dayhourEra20c10 = list()
  dayhourEra20c100 = list()
  dayhourEraI10 = list()
  dayhourEraI100 = list()

  if (t.obj$obs$data$StationName[1] != "Fino1" |
      t.obj$obs$data$StationName[1] != "Fino2") {
    dayhourTower10 = list()
    for (cnt in seq(24)) {
      dayhourTower10$vals[[cnt]] = tower10.xts[which( tower.date$hour==cnt-1 )]
      dayhourTower10$mean[[cnt]] = mean(dayhourTower10$vals[[cnt]], na.rm=T)
      dayhourTower10$sd[[cnt]] = sd(dayhourTower10$vals[[cnt]], na.rm=T)
    }
  } else {dayhourTower10 = NULL}

  for (cnt in seq(24)) {
    dayhourTower100$vals[[cnt]] = tower100.xts[which( tower.date$hour==cnt-1 )]
    dayhourTower100$mean[[cnt]] = mean(dayhourTower100$vals[[cnt]], na.rm=T)
    dayhourTower100$sd[[cnt]] = sd(dayhourTower100$vals[[cnt]], na.rm=T)
    dayhourEra20c10$vals[[cnt]] = Era20C10Xts[which( tower.date$hour==cnt-1 )]
    dayhourEra20c10$mean[[cnt]] = mean(dayhourEra20c10$vals[[cnt]])
    dayhourEra20c10$sd[[cnt]] = sd(dayhourEra20c10$vals[[cnt]])
    dayhourEra20c100$vals[[cnt]] = Era20C100Xts[which( tower.date$hour==cnt-1 )]
    dayhourEra20c100$mean[[cnt]] = mean(dayhourEra20c100$vals[[cnt]])
    dayhourEra20c100$sd[[cnt]] = sd(dayhourEra20c100$vals[[cnt]])
    dayhourEraI10$vals[[cnt]] = EraI10Xts[which( tower.date$hour==cnt-1 )]
    dayhourEraI10$mean[[cnt]] = mean(dayhourEraI10$vals[[cnt]])
    dayhourEraI10$sd[[cnt]] = sd(dayhourEraI10$vals[[cnt]])
    dayhourEraI100$vals[[cnt]] = EraI100Xts[which( tower.date$hour==cnt-1 )]
    dayhourEraI100$mean[[cnt]] = mean(dayhourEraI100$vals[[cnt]])
    dayhourEraI100$sd[[cnt]] = sd(dayhourEraI100$vals[[cnt]])
    dayhourHerz116$vals[[cnt]] = Herz116Xts[which( tower.date$hour==cnt-1 )]
    dayhourHerz116$mean[[cnt]] = mean(dayhourHerz116$vals[[cnt]])
    dayhourHerz116$sd[[cnt]] = sd(dayhourHerz116$vals[[cnt]])
    dayhourHerz10$vals[[cnt]] = Herz10Xts[which( tower.date$hour==cnt-1 )]
    dayhourHerz10$mean[[cnt]] = mean(dayhourHerz10$vals[[cnt]])
    dayhourHerz10$sd[[cnt]] = sd(dayhourHerz10$vals[[cnt]])
  }

  PS116 = PlottingSettings(t.obj$herz116$data)
  PS10 = PlottingSettings(t.obj$herz10$data)
  PSEC10 = PlottingSettings(t.obj$era20c10$data)
  PSEC100 = PlottingSettings(t.obj$era20c100$data)
  PSEI10 = PlottingSettings(t.obj$eraI10$data)
  PSEI100 = PlottingSettings(t.obj$eraI100$data)
  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {
    PST100 = PlottingSettings(t.obj$obs$data)
    PS = list(PS10=PS10, PS116=PS116, PSEC10=PSEC10,
              PSEC100=PSEC100, PSEI10=PSEI10, PSEI100=PSEI100, PST100=PST100)
  } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {
    PST10 = PlottingSettings(t.obj$obs6$data)
    PST100 = PlottingSettings(t.obj$obs$data)
    PS = list(PS10=PS10, PS116=PS116, PSEC10=PSEC10,
              PSEC100=PSEC100, PSEI10=PSEI10, PSEI100=PSEI100, PST10=PST10,
              PST100=PST100)
  } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
    PST10 = PlottingSettings(t.obj$obs6$data)
    PST100 = PlottingSettings(t.obj$obs2$data)
    PS = list(PS10=PS10, PS116=PS116, PSEC10=PSEC10,
              PSEC100=PSEC100, PSEI10=PSEI10, PSEI100=PSEI100, PST10=PST10,
              PST100=PST100)
  }
  fname.new = c(gsub("Difference", "10m-Difference", fname),
                gsub("Difference", "100m-Difference", fname))
  PlotDifferenceDailyCycle(dayhourHerz10, dayhourHerz116,
                        dayhourTower10, dayhourTower100,
                        dayhourEra20c10, dayhourEra20c100,
                        dayhourEraI10, dayhourEraI100, PS, fname.new)
}

#-----------------------------------------------------------------------------------

#' @title Prepare plotting daily cycle of COSMO-REA6 data.
#' @description Preparing to plot the daily cycle of the COSMO-REA6 RRA by creating
#'   lists holding the mean and standard deviation of hourly COSMO-REA6 data. This is done
#'   for (a) all data and (b) for selected months only.
#' @param tower.obj ClimObject holding tower and corresponding reanalysis data.
#' @param fname is a string holding the file name of the plot.
#' @importFrom xts xts
#' @export
PreparePlottingHerzDailyCycle <- function(tower.obj, fname) {

  # names include all months to be analysed; counts is the month of year
  month.names = list(names=c("January", "March", "June", "September"),
                     counts=as.numeric(c(1,3,6,9)))

  t.obj = tower.obj$climate_data_objects
  tower.date <- as.POSIXlt(t.obj$obs$data$date)

  # extended time series COSMO-REA6 at tower location
  Herz116Xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
  Herz69Xts = xts(t.obj$herz69$data$wind_speed, order.by=tower.date)
  Herz35Xts = xts(t.obj$herz35$data$wind_speed, order.by=tower.date)
  Herz10Xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
  Era20C10Xts = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
  Era20C100Xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
  EraI10Xts = xts(t.obj$eraI10$data$wind_speed, order.by=tower.date)
  EraI100Xts = xts(t.obj$eraI100$data$wind_speed, order.by=tower.date)
  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {
    tower1.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
  }

  # these lists are used for the complete time period
  dayhourHerz116 = list()
  dayhourHerz69 = list()
  dayhourHerz35 = list()
  dayhourHerz10 = list()
  dayhourEra20c10 = list()
  dayhourEra20c100 = list()
  dayhourEraI10 = list()
  dayhourEraI100 = list()

  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {
    dayhour.tower1 = list()
    for (cnt in seq(24)) {
      dayhour.tower1$vals[[cnt]] = tower1.xts[which( tower.date$hour==cnt-1 )]
      dayhour.tower1$mean[[cnt]] = mean(dayhour.tower1$vals[[cnt]], na.rm=T)
      dayhour.tower1$sd[[cnt]] = sd(dayhour.tower1$vals[[cnt]], na.rm=T)
    }
  } else { dayhour.tower1= NULL }

  for (cnt in seq(24)) {
    dayhourEra20c10$vals[[cnt]] = Era20C10Xts[which( tower.date$hour==cnt-1 )]
    dayhourEra20c10$mean[[cnt]] = mean(dayhourEra20c10$vals[[cnt]])
    dayhourEra20c10$sd[[cnt]] = sd(dayhourEra20c10$vals[[cnt]])
    dayhourEra20c100$vals[[cnt]] = Era20C100Xts[which( tower.date$hour==cnt-1 )]
    dayhourEra20c100$mean[[cnt]] = mean(dayhourEra20c100$vals[[cnt]])
    dayhourEra20c100$sd[[cnt]] = sd(dayhourEra20c100$vals[[cnt]])
    dayhourEraI10$vals[[cnt]] = EraI10Xts[which( tower.date$hour==cnt-1 )]
    dayhourEraI10$mean[[cnt]] = mean(dayhourEraI10$vals[[cnt]])
    dayhourEraI10$sd[[cnt]] = sd(dayhourEraI10$vals[[cnt]])
    dayhourEraI100$vals[[cnt]] = EraI100Xts[which( tower.date$hour==cnt-1 )]
    dayhourEraI100$mean[[cnt]] = mean(dayhourEraI100$vals[[cnt]])
    dayhourEraI100$sd[[cnt]] = sd(dayhourEraI100$vals[[cnt]])
    dayhourHerz116$vals[[cnt]] = Herz116Xts[which( tower.date$hour==cnt-1 )]
    dayhourHerz116$mean[[cnt]] = mean(dayhourHerz116$vals[[cnt]])
    dayhourHerz116$sd[[cnt]] = sd(dayhourHerz116$vals[[cnt]])
    dayhourHerz69$vals[[cnt]] = Herz69Xts[which( tower.date$hour==cnt-1 )]
    dayhourHerz69$mean[[cnt]] = mean(dayhourHerz69$vals[[cnt]])
    dayhourHerz69$sd[[cnt]] = sd(dayhourHerz69$vals[[cnt]])
    dayhourHerz35$vals[[cnt]] = Herz35Xts[which( tower.date$hour==cnt-1 )]
    dayhourHerz35$mean[[cnt]] = mean(dayhourHerz35$vals[[cnt]])
    dayhourHerz35$sd[[cnt]] = sd(dayhourHerz35$vals[[cnt]])
    dayhourHerz10$vals[[cnt]] = Herz10Xts[which( tower.date$hour==cnt-1 )]
    dayhourHerz10$mean[[cnt]] = mean(dayhourHerz10$vals[[cnt]])
    dayhourHerz10$sd[[cnt]] = sd(dayhourHerz10$vals[[cnt]])
  }

  PS116 = PlottingSettings(t.obj$herz116$data)
  PS69 = PlottingSettings(t.obj$herz69$data)
  PS35 = PlottingSettings(t.obj$herz35$data)
  PS10 = PlottingSettings(t.obj$herz10$data)
  PSEC10 = PlottingSettings(t.obj$era20c10$data)
  PSEC100 = PlottingSettings(t.obj$era20c100$data)
  PSEI10 = PlottingSettings(t.obj$eraI10$data)
  PSEI100 = PlottingSettings(t.obj$eraI100$data)
  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {
    PSF100 = PlottingSettings(t.obj$obs$data)
    PS = list(PS10=PS10, PS35=PS35, PS69=PS69, PS116=PS116, PSEC10=PSEC10,
              PSEC100=PSEC100, PSEI10=PSEI10, PSEI100=PSEI100, PSF100=PSF100)
  } else {
    PS = list(PS10=PS10, PS35=PS35, PS69=PS69, PS116=PS116, PSEC10=PSEC10,
              PSEC100=PSEC100, PSEI10=PSEI10, PSEI100=PSEI100)
  }
  fname.new = c(gsub("DailyCycle", "DailyCycle_allTime-line", fname),
                gsub("DailyCycle", "DailyCycle_allTime-ERAline", fname),
                gsub("DailyCycle", "DailyCycle_allTime-boxPlot", fname))
  PlotDailyCycleHerz(dayhourHerz10, dayhourHerz35,
                     dayhourHerz69, dayhourHerz116, dayhour.tower1,
                     dayhourEra20c10, dayhourEra20c100,
                     dayhourEraI10, dayhourEraI100,
                     month.names, PS, fname.new)

  # these lists are used for specific months only time periods
  dayhour.month.Herz116 = list()
  dayhour.month.Herz69 = list()
  dayhour.month.Herz35 = list()
  dayhour.month.Herz10 = list()
  dayhour.month.Era20c10 = list()
  dayhour.month.Era20c100 = list()
  dayhour.month.EraI10 = list()
  dayhour.month.EraI100 = list()
  for (cnt in seq((month.names$names))) {
    dummy116 = list()
    dummy69 = list()
    dummy35 = list()
    dummy10 = list()
    dummyEC10 = list()
    dummyEC100 = list()
    dummyEI10 = list()
    dummyEI100 = list()
    for (tstep in seq(24)) {
      dummyEC10$vals[[tstep]] = Era20C10Xts[which( tower.date$hour==tstep-1 &
                                                   tower.date$mon==month.names$counts[cnt])]
      dummyEC10$mean[[tstep]] = mean(dummyEC10$vals[[tstep]])
      dummyEC10$sd[[tstep]] = sd(dummyEC10$vals[[tstep]])

      dummyEC100$vals[[tstep]] = Era20C100Xts[which( tower.date$hour==tstep-1 &
                                                   tower.date$mon==month.names$counts[cnt])]
      dummyEC100$mean[[tstep]] = mean(dummyEC100$vals[[tstep]])
      dummyEC100$sd[[tstep]] = sd(dummyEC100$vals[[tstep]])

      dummyEI10$vals[[tstep]] = EraI10Xts[which( tower.date$hour==tstep-1 &
                                                   tower.date$mon==month.names$counts[cnt])]
      dummyEI10$mean[[tstep]] = mean(dummyEI10$vals[[tstep]])
      dummyEI10$sd[[tstep]] = sd(dummyEI10$vals[[tstep]])

      dummyEI100$vals[[tstep]] = EraI100Xts[which( tower.date$hour==tstep-1 &
                                                   tower.date$mon==month.names$counts[cnt])]
      dummyEI100$mean[[tstep]] = mean(dummyEI100$vals[[tstep]])
      dummyEI100$sd[[tstep]] = sd(dummyEI100$vals[[tstep]])

      dummy116$vals[[tstep]] = Herz116Xts[which( tower.date$hour==tstep-1 &
                                                   tower.date$mon==month.names$counts[cnt])]
      dummy116$mean[[tstep]] = mean(dummy116$vals[[tstep]])
      dummy116$sd[[tstep]] = sd(dummy116$vals[[tstep]])

      dummy69$vals[[tstep]] = Herz69Xts[which( tower.date$hour==tstep-1 &
                                                 tower.date$mon==month.names$counts[cnt])]
      dummy69$mean[[tstep]] = mean(dummy69$vals[[tstep]])
      dummy69$sd[[tstep]] = sd(dummy69$vals[[tstep]])

      dummy35$vals[[tstep]] = Herz35Xts[which( tower.date$hour==tstep-1 &
                                                 tower.date$mon==month.names$counts[cnt])]
      dummy35$mean[[tstep]] = mean(dummy35$vals[[tstep]])
      dummy35$sd[[tstep]] = sd(dummy35$vals[[tstep]])

      dummy10$vals[[tstep]] = Herz10Xts[which( tower.date$hour==tstep-1 &
                                                 tower.date$mon==month.names$counts[cnt])]
      dummy10$mean[[tstep]] = mean(dummy10$vals[[tstep]])
      dummy10$sd[[tstep]] = sd(dummy10$vals[[tstep]])
    }
    dayhour.month.Herz116[[cnt]] = dummy116
    dayhour.month.Herz69[[cnt]] = dummy69
    dayhour.month.Herz35[[cnt]] = dummy35
    dayhour.month.Herz10[[cnt]] = dummy10
    dayhour.month.EraI10[[cnt]] = dummyEI10
    dayhour.month.EraI100[[cnt]] = dummyEI100
    dayhour.month.Era20c10[[cnt]] = dummyEC10
    dayhour.month.Era20c100[[cnt]] = dummyEC100
  }
  dayhour.month.Herz116 = setNames(dayhour.month.Herz116, month.names$names)
  dayhour.month.Herz69 = setNames(dayhour.month.Herz69, month.names$names)
  dayhour.month.Herz35 = setNames(dayhour.month.Herz35, month.names$names)
  dayhour.month.Herz10 = setNames(dayhour.month.Herz10, month.names$names)
  dayhour.month.EraI10 = setNames(dayhour.month.EraI10, month.names$names)
  dayhour.month.EraI100 = setNames(dayhour.month.EraI100, month.names$names)
  dayhour.month.Era20c10 = setNames(dayhour.month.Era20c10, month.names$names)
  dayhour.month.Era20c100 = setNames(dayhour.month.Era20c100, month.names$names)

  fname.new = c(gsub("DailyCycle", "DailyCycle_selectMonths-line", fname),
                gsub("DailyCycle", "DailyCycle_selectMonths-boxPlot", fname))
  PlotDailyCycleHerz(dayhour.month.Herz10, dayhour.month.Herz35,
                     dayhour.month.Herz69, dayhour.month.Herz116,
                     F100=NULL,
                     dayhour.month.EraI10, dayhour.month.EraI100,
                     dayhour.month.Era20c10, dayhour.month.Era20c100,
                     month.names, PS,
                     fname.new)

}

#-----------------------------------------------------------------------------------

#' @title Plotting the daily cycle of COSMO-REA6 reanalysis data.
#' @description This function actually performs the plotting of the daily cycle of
#'   the hourly HerZ reanalysis data. Two types of plots are created: line plots
#'   showing the evolution of the daily cycle of the mean; and box plots showing the
#'   variability at each hourly time step. This is done each for the complete time
#'   period and for selected months only.
#' @param Herz10,Herz35,Herz69,Herz116 are lists holding the COSMO-REA6 reanalysis data
#'   at diffferent height levels.
#' @param month.names is a named list holding the names of the month of year and
#'   their corresponding number within the year (list(names=, count=)).
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param fname is a string of the file name of the plot file
#' @importFrom zoo coredata
PlotDailyCycleHerz <- function(Herz10, Herz35, Herz69, Herz116, F100,
                               Era20c10, Era20c100, EraI10, EraI100,
                               month.names, PS, fname) {

  x.lab = "time of day [hours]"
  y.lab = "wind speed [m/s]"

  legend.cex = 0.95
  xylab.cex = 1.25
  title.cex = 3

  # determine min and max value for plotting range
  if (is.null(Herz10$mean)) { # there are more than one data value sets saved
    min.dummy.mean = vector(mode="numeric", length=length(month.names$names))
    max.dummy.mean = vector(mode="numeric", length=length(month.names$names))
    max.dummy10 = vector(mode="numeric", length=length(month.names$names))
    max.dummy100 = vector(mode="numeric", length=length(month.names$names))
    for (cnt in seq(month.names$names)) {
      min.dummy.mean[cnt] = floor(min(Herz10[[cnt]]$mean, na.rm=TRUE))
      max.dummy.mean[cnt] = ceiling(max(Herz116[[cnt]]$mean, na.rm=TRUE))
      max.dummy10[cnt] = ceiling(max(Herz10[[cnt]]$vals[[1]], na.rm=TRUE)) # see comment below
      max.dummy100[cnt] = ceiling(max(Herz116[[cnt]]$vals[[1]], na.rm=TRUE)) # see comment below
    }
    min.val.mean = min(min.dummy.mean)
    max.val.mean = max(max.dummy.mean)
    max.val.10 = max(max.dummy10)
    max.val.100 = max(max.dummy100)
  } else {  # there is only one data value set saved
    min.val.mean = floor(min(Herz10$mean, na.rm=TRUE))
    max.val.mean = ceiling(max(Herz116$mean, na.rm=TRUE))
    max.val.10 = ceiling(max(Herz10$vals[[1]], na.rm=TRUE)) # see comment below
    max.val.100 = ceiling(max(Herz116$vals[[1]], na.rm=TRUE)) # see comment below
  }
  # comment: I assume that one time series (vals[[1]]) is long enough to cover the
  # variability of all hours

  # plot line plots
  pdf(fname[1], width=PS$PS10$land.a4width, height=PS$PS10$land.a4height*1.25)
  par(mfrow=c(1,1), mar=c(3.5,3,3.5,0.5), mgp=c(3,1,0), cex=2,
      cex.lab=xylab.cex, cex.axis=xylab.cex)

  if (is.null(Herz10$mean)) {
    for (cnt in seq(month.names$names)) {
      plot(Herz116[[cnt]]$mean, col="blue", pch=16, type="b",
           ylim=c(min.val.mean,max.val.mean), xlab=x.lab, ylab=y.lab, las=1)
      lines(Herz69[[cnt]]$mean, col="red", pch=16, type="b")
      lines(Herz35[[cnt]]$mean, col="orange", pch=16, type="b")
      lines(Herz10[[cnt]]$mean, col="green", pch=16, type="b")
      legend("top", legend=c(paste0("COSMO-REA6 at ", as.character(PS$PS116$obs.height)),
                             paste0("COSMO-REA6 at ", as.character(PS$PS69$obs.height)),
                             paste0("COSMO-REA6 at ", as.character(PS$PS35$obs.height)),
                             paste0("COSMO-REA6 at ", as.character(PS$PS10$obs.height))),
             text.col=c("blue", "red", "orange", "green"), cex=legend.cex)
      mtext(paste0("Daily cycle of COSMO-REA6 wind speed\nin ",
                   month.names$names[[cnt]], " at tower location ",
                   PS$PS10$obs.name), line=1, cex=title.cex)
    }
  } else {
    ylim.set=c(1,15)
    legend.place = "bottom"
    if (as.character(PS$PS10$obs.name) == "Lindenberg") {ylim.set=c(2.5, 6.5)}
    if (as.character(PS$PS10$obs.name) == "Fino1") {ylim.set=c(5.5, 10.5)}
    if (as.character(PS$PS10$obs.name) == "Fino2") {ylim.set=c(1.5, 10.5)}
    if (as.character(PS$PS10$obs.name) == "Cabauw") {
      ylim.set=c(2.5, 8.5)
      legend.place = "top"
    }
    plot(Herz116$mean, col="blue", pch=16, type="b",
         ylim=ylim.set, xlab="", ylab="", las=1)
    lines(Herz69$mean, col="red", pch=16, type="b")
    lines(Herz35$mean, col="orange", pch=16, type="b")
    lines(Herz10$mean, col="green", pch=16, type="b")
    if (as.character(PS$PS10$obs.name) == "Fino1" | as.character(PS$PS10$obs.name) == "Fino2") {
      lines(F100$mean, col="magenta", pch=16, type="b")
      legend(legend.place, legend=c(paste0("COSMO-REA6 at ", as.character(PS$PS116$obs.height)),
                                    paste0("COSMO-REA6 at ", as.character(PS$PS69$obs.height)),
                                    paste0("COSMO-REA6 at ", as.character(PS$PS35$obs.height)),
                                    paste0("COSMO-REA6 at ", as.character(PS$PS10$obs.height)),
                                    paste0(as.character(PS$PSF100$obs.name), " at ",
                                           as.character(PS$PSF100$obs.height))),
             text.col=c("blue", "red", "orange", "green", "magenta"), cex=legend.cex)
    } else {
      legend(legend.place, legend=c(paste0("COSMO-REA6 at ", as.character(PS$PS116$obs.height)),
                                    paste0("COSMO-REA6 at ", as.character(PS$PS69$obs.height)),
                                    paste0("COSMO-REA6 at ", as.character(PS$PS35$obs.height)),
                                    paste0("COSMO-REA6 at ", as.character(PS$PS10$obs.height))),
             text.col=c("blue", "red", "orange", "green"), cex=legend.cex)
    }
    mtext(paste0("Daily cycle of COSMO-REA6 wind speed\nat tower location ",
                 PS$PS10$obs.name), line=0.5, cex=title.cex)
    title(xlab=x.lab, line=2)
    title(ylab=y.lab, line=2)
  }
  dev.off()

  # new plot for global ERAs only
  pdf(fname[2], width=PS$PS10$land.a4width, height=PS$PS10$land.a4height*1.25)
  par(mfrow=c(1,1), mar=c(3.5,3,3.5,0.5), mgp=c(3,1,0), cex=2,
      cex.lab=xylab.cex, cex.axis=xylab.cex)

  ylim.set=c(1,15)
  if (as.character(PS$PS10$obs.name) == "Lindenberg") {ylim.set=c(2.5, 6.5)}
  if (as.character(PS$PS10$obs.name) == "Fino1") {ylim.set=c(6.5, 10.5)}
  if (as.character(PS$PS10$obs.name) == "Fino2") {ylim.set=c(2.5, 10.5)}
  if (as.character(PS$PS10$obs.name) == "Cabauw") {
    ylim.set=c(2.5, 8.5)
    legend.place = "top"
  }
  legend.place = "bottom"

  plot(Era20c10$mean, col="yellowgreen", pch=17, type="b", ylim=ylim.set,
       xlab="", ylab="", las=1)
  lines(Era20c100$mean, col="cyan2", pch=17, type="b")
  lines(EraI10$mean, col="green", pch=19, type="b")
  lines(EraI100$mean, col="blue", pch=19, type="b")
  if (as.character(PS$PS10$obs.name) == "Fino1" | as.character(PS$PS10$obs.name) == "Fino2") {
    lines(F100$mean, col="magenta", pch=16, type="b")
    legend(legend.place, legend=c(paste0("ERA-I at ", as.character(PS$PSEI100$obs.height)),
                                  paste0("ERA20C at ", as.character(PS$PSEC100$obs.height)),
                                  paste0("ERA-I at ", as.character(PS$PSEI10$obs.height)),
                                  paste0("ERA20C at ", as.character(PS$PSEC10$obs.height)),
                                  paste0(as.character(PS$PSF100$obs.name), " at ",
                                         as.character(PS$PSF100$obs.height))),
           text.col=c("blue", "cyan2", "green", "yellowgreen", "magenta"), cex=legend.cex)
  } else {
    legend(legend.place, legend=c(paste0("ERA-I at ", as.character(PS$PSEI100$obs.height)),
                                  paste0("ERA20C at ", as.character(PS$PSEC10069$obs.height)),
                                  paste0("ERA-I at ", as.character(PS$PSEI10$obs.height)),
                                  paste0("ERA20C at ", as.character(PS$PSEC10$obs.height))),
           text.col=c("blue", "red", "orange", "green"), cex=legend.cex)
  }

  mtext(paste0("Daily cycle of ERA20C and ERA-I wind speed\nat tower location ",
  PS$PSEC10$obs.name), line=0.5, cex=title.cex)
  title(xlab=x.lab, line=2)
  title(ylab=y.lab, line=2)
  dev.off()

  # plot box plots
  pdf(fname[3], width=PS$PS10$land.a4width, height=PS$PS10$land.a4height)
  par(mfrow=c(2,1), oma=c(0.5,0.5,0.5,0.5), mar=c(4,4,2,0), cex.lab=2, cex.axis=2)

  swex = 0.4
  bwex = 0.3
  hori = F
  nch = T
  oline = F
  if (is.null(Herz116$vals)) {
    for (cnt in seq(month.names$names)) {
      boxplot.default(coredata(Herz116[[cnt]]$vals[[1]]), coredata(Herz116[[cnt]]$vals[[2]]),
                      coredata(Herz116[[cnt]]$vals[[3]]), coredata(Herz116[[cnt]]$vals[[4]]),
                      coredata(Herz116[[cnt]]$vals[[5]]), coredata(Herz116[[cnt]]$vals[[6]]),
                      coredata(Herz116[[cnt]]$vals[[7]]), coredata(Herz116[[cnt]]$vals[[8]]),
                      coredata(Herz116[[cnt]]$vals[[9]]), coredata(Herz116[[cnt]]$vals[[10]]),
                      coredata(Herz116[[cnt]]$vals[[11]]), coredata(Herz116[[cnt]]$vals[[12]]),
                      coredata(Herz116[[cnt]]$vals[[13]]), coredata(Herz116[[cnt]]$vals[[14]]),
                      coredata(Herz116[[cnt]]$vals[[15]]), coredata(Herz116[[cnt]]$vals[[16]]),
                      coredata(Herz116[[cnt]]$vals[[17]]), coredata(Herz116[[cnt]]$vals[[18]]),
                      coredata(Herz116[[cnt]]$vals[[19]]), coredata(Herz116[[cnt]]$vals[[20]]),
                      coredata(Herz116[[cnt]]$vals[[21]]), coredata(Herz116[[cnt]]$vals[[22]]),
                      coredata(Herz116[[cnt]]$vals[[23]]), coredata(Herz116[[cnt]]$vals[[24]]),
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.100), col="blue",
                      xlab="", ylab=y.lab, names=c(seq(24)))
      mtext(paste0("Daily cycle of ", PS$PS116$obs.height, " COSMO-REA6 wind speed in ",
                   month.names$names[[cnt]], " at station location ",
                   PS$PS116$obs.name), line=1, cex=title.cex)

      boxplot.default(coredata(Herz10[[cnt]]$vals[[1]]), coredata(Herz10[[cnt]]$vals[[2]]),
                      coredata(Herz10[[cnt]]$vals[[3]]), coredata(Herz10[[cnt]]$vals[[4]]),
                      coredata(Herz10[[cnt]]$vals[[5]]), coredata(Herz10[[cnt]]$vals[[6]]),
                      coredata(Herz10[[cnt]]$vals[[7]]), coredata(Herz10[[cnt]]$vals[[8]]),
                      coredata(Herz10[[cnt]]$vals[[9]]), coredata(Herz10[[cnt]]$vals[[10]]),
                      coredata(Herz10[[cnt]]$vals[[11]]), coredata(Herz10[[cnt]]$vals[[12]]),
                      coredata(Herz10[[cnt]]$vals[[13]]), coredata(Herz10[[cnt]]$vals[[14]]),
                      coredata(Herz10[[cnt]]$vals[[15]]), coredata(Herz10[[cnt]]$vals[[16]]),
                      coredata(Herz10[[cnt]]$vals[[17]]), coredata(Herz10[[cnt]]$vals[[18]]),
                      coredata(Herz10[[cnt]]$vals[[19]]), coredata(Herz10[[cnt]]$vals[[20]]),
                      coredata(Herz10[[cnt]]$vals[[21]]), coredata(Herz10[[cnt]]$vals[[22]]),
                      coredata(Herz10[[cnt]]$vals[[23]]), coredata(Herz10[[cnt]]$vals[[24]]),
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.10), col="green",
                      xlab=x.lab, ylab=y.lab, names=c(seq(24)))
      mtext(paste0("Daily cycle of ", PS$PS10$obs.height, " COSMO-REA6 wind speed in ",
                   month.names$names[[cnt]], " at station location ",
                   PS$PS10$obs.name), line=1, cex=title.cex)
    }
  } else {
    boxplot.default(coredata(Herz116$vals[[1]]), coredata(Herz116$vals[[2]]),
                    coredata(Herz116$vals[[3]]), coredata(Herz116$vals[[4]]),
                    coredata(Herz116$vals[[5]]), coredata(Herz116$vals[[6]]),
                    coredata(Herz116$vals[[7]]), coredata(Herz116$vals[[8]]),
                    coredata(Herz116$vals[[9]]), coredata(Herz116$vals[[10]]),
                    coredata(Herz116$vals[[11]]), coredata(Herz116$vals[[12]]),
                    coredata(Herz116$vals[[13]]), coredata(Herz116$vals[[14]]),
                    coredata(Herz116$vals[[15]]), coredata(Herz116$vals[[16]]),
                    coredata(Herz116$vals[[17]]), coredata(Herz116$vals[[18]]),
                    coredata(Herz116$vals[[19]]), coredata(Herz116$vals[[20]]),
                    coredata(Herz116$vals[[21]]), coredata(Herz116$vals[[22]]),
                    coredata(Herz116$vals[[23]]), coredata(Herz116$vals[[24]]),
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.100), col="blue",
                    xlab="", ylab=y.lab, names=c(seq(24)))
    mtext(paste0("Daily cycle of wind speed of COSMO-REA6 in ", PS$PS116$obs.height,
                 " at station location ", PS$PS116$obs.name), line=1, cex=title.cex)

    boxplot.default(coredata(Herz10$vals[[1]]), coredata(Herz10$vals[[2]]),
                    coredata(Herz10$vals[[3]]), coredata(Herz10$vals[[4]]),
                    coredata(Herz10$vals[[5]]), coredata(Herz10$vals[[6]]),
                    coredata(Herz10$vals[[7]]), coredata(Herz10$vals[[8]]),
                    coredata(Herz10$vals[[9]]), coredata(Herz10$vals[[10]]),
                    coredata(Herz10$vals[[11]]), coredata(Herz10$vals[[12]]),
                    coredata(Herz10$vals[[13]]), coredata(Herz10$vals[[14]]),
                    coredata(Herz10$vals[[15]]), coredata(Herz10$vals[[16]]),
                    coredata(Herz10$vals[[17]]), coredata(Herz10$vals[[18]]),
                    coredata(Herz10$vals[[19]]), coredata(Herz10$vals[[20]]),
                    coredata(Herz10$vals[[21]]), coredata(Herz10$vals[[22]]),
                    coredata(Herz10$vals[[23]]), coredata(Herz10$vals[[24]]),
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.10), col="green",
                    xlab=x.lab, ylab=y.lab, names=c(seq(24)))
    mtext(paste0("Daily cycle of wind speed of COSMO-REA6 in ", PS$PS10$obs.height,
                 " at station location ", PS$PS10$obs.name), line=1, cex=title.cex)
  }
  dev.off()

}

#-----------------------------------------------------------------------------------

#' @title Plotting the daily cycle of tower measurements.
#' @description This function actually performs the plotting of the daily cycle of
#'   the hourly tower measurements. Two types of plots are created: line plots
#'   showing the evolution of the daily cycle of the mean; and box plots showing the
#'   variability at each hourly time step. This is done each for the complete time
#'   period and for selected months only.
#' @param tower1,tower2,tower3,tower4,tower5,tower6 are lists holding the tower
#'   measurement data at diffferent height levels.
#' @param month.names is a named list holding the names of the month of year and
#'   their corresponding number within the year (list(names=, count=)).
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @importFrom zoo coredata
PlotDailyCycleTower <- function(tower1, tower2, tower3, tower4, tower5, tower6,
                                month.names, PS, fname) {

  x.lab = "time of day [hours]"
  y.lab = "wind speed [m/s]"

  legend.cex = 0.95
  xylab.cex = 1.25
  title.cex = 3

  # determine min and max value for plotting range
  if (is.null(tower1$mean)) { # there are more than one data sets saved
    min.dummy.mean = vector(mode="numeric", length=length(month.names$names))
    max.dummy.mean = vector(mode="numeric", length=length(month.names$names))
    max.dummy10 = vector(mode="numeric", length=length(month.names$names))
    max.dummy100 = vector(mode="numeric", length=length(month.names$names))
    for (cnt in seq(month.names$names)) {
      if (length(tower6)>0) {
        min.dummy.mean[cnt] = floor(min(tower6[[cnt]]$mean, na.rm=TRUE))
        max.dummy10[cnt] = ceiling(max(tower6[[cnt]]$vals[[1]], na.rm=TRUE)) # see comment below
      } else {
        min.dummy.mean[cnt] = floor(min(tower1[[cnt]]$mean, na.rm=TRUE))
      }
      max.dummy.mean[cnt] = ceiling(max(tower1[[cnt]]$mean, na.rm=TRUE))
      max.dummy100[cnt] = ceiling(max(tower1[[cnt]]$vals[[1]], na.rm=TRUE)) # see comment below
    }
    min.val.mean = min(min.dummy.mean)
    max.val.mean = max(max.dummy.mean)
    max.val.100 = max(max.dummy100)
    if (length(tower6)>0) {
      max.val.10 = max(max.dummy10)
    } else {
      max.val.10 = max.val.100
    }
  } else {  # there is only one data set saved
    if (length(tower6)>0) {
      min.val.mean = floor(min(tower6$mean, na.rm=TRUE))
      max.val.10 = ceiling(max(tower6$vals[[1]], na.rm=TRUE)) # see comment below
    } else {
      min.val.mean = floor(min(tower1$mean, na.rm=TRUE))
      max.val.10 = ceiling(max(tower1$vals[[1]], na.rm=TRUE)) # see comment below
    }
    max.val.mean = ceiling(max(tower1$mean, na.rm=TRUE))
    max.val.100 = ceiling(max(tower1$vals[[1]], na.rm=TRUE)) # see comment below
  }
  # comment: I assume that one time series (vals[[1]]) is long enough to cover
  # the variability of all hours

  # plot line plots
  pdf(fname[1], width=PS$PST1$land.a4width, height=PS$PST1$land.a4height*1.25)
  par(mfrow=c(1,1), mar=c(3.5,3,3.5,0.5), mgp=c(3,1,0), cex=2,
      cex.lab=xylab.cex, cex.axis=xylab.cex)

  if (is.null(tower1$mean)) {
    for (cnt in seq(month.names$names)) {
      plot(tower1[[cnt]]$mean, col="blue", pch=16, type="b",
           ylim=c(min.val.mean,max.val.mean), xlab=x.lab, ylab=y.lab, las=1)
      if (length(tower6)>0) {
        lines(tower2[[cnt]]$mean, col="magenta", pch=16, type="b")
        lines(tower3[[cnt]]$mean, col="red", pch=16, type="b")
        lines(tower4[[cnt]]$mean, col="orange", pch=16, type="b")
        lines(tower5[[cnt]]$mean, col="black", pch=16, type="b")
        lines(tower6[[cnt]]$mean, col="green", pch=16, type="b")
        legend("top", legend=c(paste0(as.character(PS$PST1$obs.name), " at ",
                                      as.character(PS$PST1$obs.height)),
                               paste0(as.character(PS$PST2$obs.name), " at ",
                                      as.character(PS$PST2$obs.height)),
                               paste0(as.character(PS$PST3$obs.name), " at ",
                                      as.character(PS$PST3$obs.height)),
                               paste0(as.character(PS$PST4$obs.name), " at ",
                                      as.character(PS$PST4$obs.height)),
                               paste0(as.character(PS$PST5$obs.name), " at ",
                                      as.character(PS$PST5$obs.height)),
                               paste0(as.character(PS$PST6$obs.name), " at ",
                                      as.character(PS$PST6$obs.height))),
               cex=legend.cex,
               text.col=c("blue", "magenta", "red", "orange", "black", "green"))
      } else {
        legend("top", legend=c(paste0(as.character(PS$PST1$obs.name), " at ",
                                      as.character(PS$PST1$obs.height))),
               cex=legend.cex, text.col=c("blue"))
      }
    }

  } else {

    ylim.set=c(0,10)
    if (as.character(PS$PST1$obs.name) == "Lindenberg") {ylim.set = c(2.5, 6.5)}
    if (as.character(PS$PST1$obs.name) == "Cabauw") {ylim.set = c(2.5, 8.5)}
    if (length(tower6)>0) {
      if (as.character(PS$PST1$obs.name) == "Cabauw") {
        plot(tower2$mean, col="blue", pch=16, type="b",
             xlab="", ylab="", ylim=ylim.set, las=1)
        lines(tower3$mean, col="red", pch=16, type="b")
        lines(tower4$mean, col="orange", pch=16, type="b")
        lines(tower5$mean, col="black", pch=16, type="b")
        lines(tower6$mean, col="green", pch=16, type="b")
        legend("bottom", legend=c(paste0(as.character(PS$PST2$obs.name), " at ",
                                         as.character(PS$PST2$obs.height)),
                                  paste0(as.character(PS$PST3$obs.name), " at ",
                                         as.character(PS$PST3$obs.height)),
                                  paste0(as.character(PS$PST4$obs.name), " at ",
                                         as.character(PS$PST4$obs.height)),
                                  paste0(as.character(PS$PST5$obs.name), " at ",
                                         as.character(PS$PST5$obs.height)),
                                  paste0(as.character(PS$PST6$obs.name), " at ",
                                         as.character(PS$PST6$obs.height))),
               cex=legend.cex,
               text.col=c("blue", "red", "orange", "black", "green"))
      } else if (as.character(PS$PST1$obs.name) == "Lindenberg") {
        plot(tower1$mean, col="blue", pch=16, type="b",
             xlab="", ylab="", ylim=ylim.set, las=1)
        lines(tower2$mean, col="red", pch=16, type="b")
        lines(tower3$mean, col="red", pch=16, type="b")
        lines(tower4$mean, col="orange", pch=16, type="b")
        lines(tower5$mean, col="black", pch=16, type="b")
        lines(tower6$mean, col="green", pch=16, type="b")
        legend("bottom", legend=c(paste0(as.character(PS$PST1$obs.name), " at ",
                                         as.character(PS$PST1$obs.height)),
                                  paste0(as.character(PS$PST2$obs.name), " at ",
                                         as.character(PS$PST2$obs.height)),
                                  paste0(as.character(PS$PST3$obs.name), " at ",
                                         as.character(PS$PST3$obs.height)),
                                  paste0(as.character(PS$PST4$obs.name), " at ",
                                         as.character(PS$PST4$obs.height)),
                                  paste0(as.character(PS$PST5$obs.name), " at ",
                                         as.character(PS$PST5$obs.height)),
                                  paste0(as.character(PS$PST6$obs.name), " at ",
                                         as.character(PS$PST6$obs.height))),
               cex=legend.cex,
               text.col=c("blue", "red", "red", "orange", "black", "green"))
      }
    } else {
      plot(tower1$mean, col="blue", pch=16, type="b",
           xlab=x.lab, ylab=y.lab, ylim=ylim.set, las=1)
      legend("top", legend=c(paste0(as.character(PS$PST1$obs.name), " at ",
                                    as.character(PS$PST1$obs.height))),
             text.col=c("blue"), cex=legend.cex)
    }
    mtext(paste0("Daily cycle of\n", PS$PST1$obs.name, " wind speed"),
          cex=title.cex, line=0.5)
    title(xlab=x.lab, line=2)
    title(ylab=y.lab, line=2)
  }
  dev.off()


  # plot box plots
  pdf(fname[2], width=PS$PST1$land.a4width, height=PS$PST1$land.a4height)
  par(mfrow=c(2,1), oma=c(0.5,0.5,0.5,0.5), mar=c(4,4,2,0), cex=1.0)

  swex = 0.4
  bwex = 0.3
  hori = F
  nch = T
  oline = F
  if (is.null(tower1$vals)) {
    for (cnt in seq(month.names$names)) {
      boxplot.default(coredata(tower1[[cnt]]$vals[[1]]), coredata(tower1[[cnt]]$vals[[2]]),
                      coredata(tower1[[cnt]]$vals[[3]]), coredata(tower1[[cnt]]$vals[[4]]),
                      coredata(tower1[[cnt]]$vals[[5]]), coredata(tower1[[cnt]]$vals[[6]]),
                      coredata(tower1[[cnt]]$vals[[7]]), coredata(tower1[[cnt]]$vals[[8]]),
                      coredata(tower1[[cnt]]$vals[[9]]), coredata(tower1[[cnt]]$vals[[10]]),
                      coredata(tower1[[cnt]]$vals[[11]]), coredata(tower1[[cnt]]$vals[[12]]),
                      coredata(tower1[[cnt]]$vals[[13]]), coredata(tower1[[cnt]]$vals[[14]]),
                      coredata(tower1[[cnt]]$vals[[15]]), coredata(tower1[[cnt]]$vals[[16]]),
                      coredata(tower1[[cnt]]$vals[[17]]), coredata(tower1[[cnt]]$vals[[18]]),
                      coredata(tower1[[cnt]]$vals[[19]]), coredata(tower1[[cnt]]$vals[[20]]),
                      coredata(tower1[[cnt]]$vals[[21]]), coredata(tower1[[cnt]]$vals[[22]]),
                      coredata(tower1[[cnt]]$vals[[23]]), coredata(tower1[[cnt]]$vals[[24]]),
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.100), col="blue",
                      xlab="", ylab=y.lab, names=c(seq(24)))
      title(main=paste0("Daily cycle of ", PS$PST1$obs.name, " wind speed in ",
                        month.names$names[[cnt]]), line=1, cex=1.5)
      if (length(tower6)>0) {
        boxplot.default(coredata(tower6[[cnt]]$vals[[1]]), coredata(tower6[[cnt]]$vals[[2]]),
                        coredata(tower6[[cnt]]$vals[[3]]), coredata(tower6[[cnt]]$vals[[4]]),
                        coredata(tower6[[cnt]]$vals[[5]]), coredata(tower6[[cnt]]$vals[[6]]),
                        coredata(tower6[[cnt]]$vals[[7]]), coredata(tower6[[cnt]]$vals[[8]]),
                        coredata(tower6[[cnt]]$vals[[9]]), coredata(tower6[[cnt]]$vals[[10]]),
                        coredata(tower6[[cnt]]$vals[[11]]), coredata(tower6[[cnt]]$vals[[12]]),
                        coredata(tower6[[cnt]]$vals[[13]]), coredata(tower6[[cnt]]$vals[[14]]),
                        coredata(tower6[[cnt]]$vals[[15]]), coredata(tower6[[cnt]]$vals[[16]]),
                        coredata(tower6[[cnt]]$vals[[17]]), coredata(tower6[[cnt]]$vals[[18]]),
                        coredata(tower6[[cnt]]$vals[[19]]), coredata(tower6[[cnt]]$vals[[20]]),
                        coredata(tower6[[cnt]]$vals[[21]]), coredata(tower6[[cnt]]$vals[[22]]),
                        coredata(tower6[[cnt]]$vals[[23]]), coredata(tower6[[cnt]]$vals[[24]]),
                        horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                        boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.10), col="green",
                        xlab=x.lab, ylab=y.lab, names=c(seq(24)))
        title(main=paste0("Daily cycle of ", PS$PST6$obs.name, " wind speed in ",
                          month.names$names[[cnt]]), line=1, cex=1.5)
      }
    }

  } else {

    boxplot.default(coredata(tower1$vals[[1]]), coredata(tower1$vals[[2]]),
                    coredata(tower1$vals[[3]]), coredata(tower1$vals[[4]]),
                    coredata(tower1$vals[[5]]), coredata(tower1$vals[[6]]),
                    coredata(tower1$vals[[7]]), coredata(tower1$vals[[8]]),
                    coredata(tower1$vals[[9]]), coredata(tower1$vals[[10]]),
                    coredata(tower1$vals[[11]]), coredata(tower1$vals[[12]]),
                    coredata(tower1$vals[[13]]), coredata(tower1$vals[[14]]),
                    coredata(tower1$vals[[15]]), coredata(tower1$vals[[16]]),
                    coredata(tower1$vals[[17]]), coredata(tower1$vals[[18]]),
                    coredata(tower1$vals[[19]]), coredata(tower1$vals[[20]]),
                    coredata(tower1$vals[[21]]), coredata(tower1$vals[[22]]),
                    coredata(tower1$vals[[23]]), coredata(tower1$vals[[24]]),
                    horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                    boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.100), col="blue",
                    xlab="", ylab=y.lab, names=c(seq(24)))
    title(main=paste0("Daily cycle of ", PS$PST1$obs.name, " wind speed at ",
                      PS$PST1$obs.height), line=1, cex=1.5)
    if (length(tower6)>0) {
      boxplot.default(coredata(tower6$vals[[1]]), coredata(tower6$vals[[2]]),
                      coredata(tower6$vals[[3]]), coredata(tower6$vals[[4]]),
                      coredata(tower6$vals[[5]]), coredata(tower6$vals[[6]]),
                      coredata(tower6$vals[[7]]), coredata(tower6$vals[[8]]),
                      coredata(tower6$vals[[9]]), coredata(tower6$vals[[10]]),
                      coredata(tower6$vals[[11]]), coredata(tower6$vals[[12]]),
                      coredata(tower6$vals[[13]]), coredata(tower6$vals[[14]]),
                      coredata(tower6$vals[[15]]), coredata(tower6$vals[[16]]),
                      coredata(tower6$vals[[17]]), coredata(tower6$vals[[18]]),
                      coredata(tower6$vals[[19]]), coredata(tower6$vals[[20]]),
                      coredata(tower6$vals[[21]]), coredata(tower6$vals[[22]]),
                      coredata(tower6$vals[[23]]), coredata(tower6$vals[[24]]),
                      horizontal=hori, notch=nch, outline=oline, na.action=na.pass,
                      boxwex=bwex, staplewex=swex, las=1, ylim=c(0,max.val.10), col="green",
                      xlab=x.lab, ylab=y.lab, names=c(seq(24)))
      title(main=paste0("Daily cycle of ", PS$PST6$obs.name, " wind speed at ",
                        PS$PST6$obs.height), line=1, cex=1.5)
    }
  }
  dev.off()

}

#-----------------------------------------------------------------------------------

#' @title Plotting the daily cycle differences between tower measurements and
#'   regional and global reanalyses.
#' @description This function actually performs the plotting of the anomalies of the
#'   daily cycle between the hourly tower measurements and the regional and global
#'   reanalyses.
#' @param Herz10,Herz116,Tower10,Tower100,Era20c10,Era20c100,EraI10,EraI100 are
#'   lists holding the tower measurement data and regional and global reanalysis
#'   data at diffferent height levels.
#'   their corresponding number within the year (list(names=, count=)).
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @importFrom zoo zoo
PlotDifferenceDailyCycle <- function(Herz10, Herz116, Tower10, Tower100,
                                     Era20c10, Era20c100, EraI10, EraI100, PS, fname) {

  x.lab = "time of day [hours]"
  y.lab = "wind speed in [m/s]"

  legend.cex = 0.65
  xylab.cex = 0.9
  title.cex = 2
  legend.place = "top"
  if (as.character(PS$PST10$obs.name) == "Fino1" |
      as.character(PS$PST10$obs.name) == "Fino2") {
    mfrow.set = c(1,1)
    height.mult = 0.67
  } else {
    mfrow.set = c(2,1)
    height.mult = 1.
  }

  # plot difference plot
  dummy = numeric(length=length(Herz10$vals)) * NA

  pdf(fname[1], width=PS$PS10$land.a4width, height=PS$PS10$land.a4height*1.25*height.mult)
  par(mfrow=mfrow.set, oma=c(4,4,3,0.5), mar=c(0,0,0,0), mgp=c(3,1,0), cex=2,
      cex.lab=xylab.cex, cex.axis=xylab.cex)

  if (as.character(PS$PST10$obs.name) == "Fino2") {
    ylim.set=c(-3,1)
  } else {
    ylim.set=c(-2,1)
  }
  if (as.character(PS$PST10$obs.name) == "Lindenberg") {legend.place="bottom"}

  if (as.character(PS$PST10$obs.name) != "Fino1" &
      as.character(PS$PST10$obs.name) != "Fino2") {
    # plot 10m differences
    plot(dummy, ylim=ylim.set, xlab="", xaxt="n", ylab="", las=1)
    lines((Herz10$mean - Tower10$mean), col="red", pch=17, type="b")
    lines((Era20c10$mean - Tower10$mean), col="blue", pch=17, type="b")
    lines((EraI10$mean - Tower10$mean), col="green", pch=17, type="b")
    title(ylab=y.lab, line=3, outer=T)
    title(main=paste0("Difference of daily cylce wind speed\nbetween reanalysis and ",
                      as.character(PS$PST100$obs.name)), line=0.5, cex=1.5, outer=T)

    legend(legend.place, legend=c(paste0(as.character(PS$PS10$rea.name), " - ",
                                         as.character(PS$PST10$obs.name), " at ",
                                         as.character(PS$PST10$obs.height)),
                                  paste0(as.character(PS$PSEC10$rea.name), " - ",
                                         as.character(PS$PST10$obs.name), " at ",
                                         as.character(PS$PST10$obs.height)),
                                  paste0(as.character(PS$PSEI10$rea.name), " - ",
                                         as.character(PS$PST10$obs.name), " at ",
                                         as.character(PS$PST10$obs.height))),
           cex=legend.cex, text.col=c("red", "blue", "green"))
  }

  if (as.character(PS$PST10$obs.name) == "Fino2") {
    ylim.set=c(-3,1)
  } else if (as.character(PS$PST10$obs.name) == "Fino1") {
    legend.place="bottom"
  } else
  {
    ylim.set=c(-2,1)
  }

  # plot 100m differences
  plot(dummy, ylim=ylim.set, xlab="", ylab="", las=1)
  lines((Herz116$mean - Tower100$mean), col="red", pch=17, type="b")
  lines((Era20c100$mean - Tower100$mean), col="blue", pch=17, type="b")
  lines((EraI100$mean - Tower100$mean), col="green", pch=17, type="b")
  if (as.character(PS$PST10$obs.name) == "Fino1" |
      as.character(PS$PST10$obs.name) == "Fino2") {
    title(main=paste0("Difference of daily cylce wind speed\nbetween reanalysis and ",
                    as.character(PS$PST100$obs.name)), line=0.5, cex=1.5, outer=T)
  }

  legend(legend.place, legend=c(paste0(as.character(PS$PS116$rea.name), " - ",
                                       as.character(PS$PST100$obs.name), " at ",
                                       as.character(PS$PST100$obs.height)),
                                paste0(as.character(PS$PSEC100$rea.name), " - ",
                                       as.character(PS$PST100$obs.name), " at ",
                                       as.character(PS$PST100$obs.height)),
                                paste0(as.character(PS$PSEI100$rea.name), " - ",
                                       as.character(PS$PST100$obs.name), " at ",
                                       as.character(PS$PST100$obs.height))),
         cex=legend.cex, text.col=c("red", "blue", "green"))
  title(xlab=x.lab, line=2, outer=T)
  title(ylab=y.lab, line=3, outer=T)

  dev.off()

}

#-----------------------------------------------------------------------------------

#' @title Plot extreme value analysis for station measurements
#' @description Plot different scores and skill scores of extreme value analysis.
#'   TODO: Not yet clear which plots of which scores and skill scores.
#'   The input to this function are a ClimObject holding measurements at one
#'   location (if tower also at different heights) and the corresponding reanalysis
#'   data (at different heights), the file name of the resulting plot, and a
#'   threshold value against which is tested.
#'   TODO: the threshold may be a vector holding a sequence of thresholds in order
#'   to plot the dependence on its value.
#'   This function does not have a return value.
#' @param stat.obj is a ClimObject holding the data of the station measurements and
#'   corresponding reanalysis data.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @param ana.time.res is a named list holding time relevant variables.
#' @export
PlotStationExtremesContr <- function(stat.obj, fname, threshold, ana.time.res) {

  PS = PlottingSettings(stat.obj$herz10$data)

  # against COSMO-REA6 at 10m
  obs = stat.obj$obs$data$wind_speed
  forec = stat.obj$herz10$data$wind_speed
  scores.df = GetScoresDF(threshold, obs, forec)
  ylims.df = as.data.frame(YLimsScores())
  fname.new = gsub("-extremes_", "_HErZ10m-extremes_", fname)
  PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
  fname.new = gsub("-extremes_", "_HErZ10m-extremes-HRvsFAR_", fname)
  PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                   scores.df$hit.rate, threshold, PS)

  if (ana.time.res$time.res != ana.time.res$hourly) {
    # against ERA20C at 10m
    PS = PlottingSettings(stat.obj$era20c10$data)
    obs = stat.obj$obs$data$wind_speed
    forec = stat.obj$era20c10$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_ERA20C10m-extremes_", fname)
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_ERA20C10m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against ERA-Interim at 10m
    PS = PlottingSettings(stat.obj$eraI10$data)
    obs = stat.obj$obs$data$wind_speed
    forec = stat.obj$eraI10$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_ERAI10m-extremes_", fname)
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_ERAI10m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)
  }

}

#-----------------------------------------------------------------------------------

#' @title Plot extreme value analysis for tower measurements
#' @description Plot different scores and skill scores of extreme value analysis.
#'   TODO: Not yet clear which plots of which scores and skill scores.
#'   The input to this function are a ClimObject holding measurements at one
#'   location (if tower also at different heights) and the corresponding reanalysis
#'   data (at different heights), the file name of the resulting plot, and a
#'   threshold value against which is tested.
#'   TODO: the threshold may be a vector holding a sequence of thresholds in order
#'   to plot the dependence on its value.
#'   This function does not have a return value.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @param ana.time.res is a named list holding time relevant variables.
#' @export
PlotTowerExtremesContr <- function(tower.obj, fname, threshold, ana.time.res) {

  t.obj = tower.obj$climate_data_objects
  PS = PlottingSettings(t.obj$herz116$data)

  if (t.obj$obs$data$StationName[1] == "Fino1" |
      t.obj$obs$data$StationName[1] == "Fino2") {

    PS["tower.height"] = t.obj$obs$data$height[1]

    # against COSMO-REA6 at 116m
    obs = t.obj$obs$data$wind_speed
    forec = t.obj$herz116$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ116m-extremes_", fname)
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ116m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    if (ana.time.res$time.res != ana.time.res$hourly) {
      # against ERA20C at 100m
      obs = t.obj$obs$data$wind_speed
      forec = t.obj$era20c100$data$wind_speed
      scores.df = GetScoresDF(threshold, obs, forec)
      ylims.df = as.data.frame(YLimsScores())
      fname.new = gsub("-extremes_", "_ERA20C100m-extremes_", fname)
      PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
      fname.new = gsub("-extremes_", "_ERA20C100m-extremes-HRvsFAR_", fname)
      PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                       scores.df$hit.rate, threshold, PS)
    }

  } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {

    # against COSMO-REA6 at 10m
    obs = t.obj$obs6$data$wind_speed
    forec = t.obj$herz10$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ10m-L10m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs6$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ10m-L10m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    if (ana.time.res$time.res != ana.time.res$hourly) {
      # against ERA20C at 10m
      obs = t.obj$obs6$data$wind_speed
      forec = t.obj$era20c10$data$wind_speed
      scores.df = GetScoresDF(threshold, obs, forec)
      ylims.df = as.data.frame(YLimsScores())
      fname.new = gsub("-extremes_", "_ERA20C10m-L10m-extremes_", fname)
      PS["tower.height"] = as.character(t.obj$obs6$data$height[1])
      PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
      fname.new = gsub("-extremes_", "_ERA20C10m-L10m-extremes-HRvsFAR_", fname)
      PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                       scores.df$hit.rate, threshold, PS)
    }

    # against COSMO-REA6 at 35m
    obs = t.obj$obs4$data$wind_speed
    forec = t.obj$herz35$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ35m-L40-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs4$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ35m-L40-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at 69m
    obs = t.obj$obs3$data$wind_speed
    forec = t.obj$herz69$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ69m-L60-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs3$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ69m-L60-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at 69m
    obs = t.obj$obs2$data$wind_speed
    forec = t.obj$herz69$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ69m-L80-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs2$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ69m-L80-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at 116m
    obs = t.obj$obs$data$wind_speed
    forec = t.obj$herz116$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ116m-L98m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ116m-L98m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    if (ana.time.res$time.res != ana.time.res$hourly) {
      # against ERA20C at 100m
      obs = t.obj$obs$data$wind_speed
      forec = t.obj$era20c100$data$wind_speed
      scores.df = GetScoresDF(threshold, obs, forec)
      ylims.df = as.data.frame(YLimsScores())
      fname.new = gsub("-extremes_", "_ERA20C100m-L98m-extremes_", fname)
      PS["tower.height"] = as.character(t.obj$obs$data$height[1])
      PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
      fname.new = gsub("-extremes_", "_ERA20C100m-L98m-extremes-HRvsFAR_", fname)
      PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                       scores.df$hit.rate, threshold, PS)
    }

    # against COSMO-REA6 at different heights
    obs = t.obj$obs$data$wind_speed
    forec = t.obj$herz116$data$wind_speed
    scores.116.df = GetScoresDF(threshold, obs, forec)
    scoresI.116.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs3$data$wind_speed
    forec = t.obj$herz69$data$wind_speed
    scores.69.df = GetScoresDF(threshold, obs, forec)
    scoresI.69.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs4$data$wind_speed
    forec = t.obj$herz35$data$wind_speed
    scores.35.df = GetScoresDF(threshold, obs, forec)
    scoresI.35.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs6$data$wind_speed
    forec = t.obj$herz10$data$wind_speed
    scores.10.df = GetScoresDF(threshold, obs, forec)
    scoresI.10.df = GetScoresDF(threshold, obs, forec, inverse=T)
    scores.lst = list(scores.116.df, scores.69.df, scores.35.df, scores.10.df)
    scoresI.lst = list(scoresI.116.df, scoresI.69.df, scoresI.35.df, scoresI.10.df)
    ylims.df = as.data.frame(YLimsScores())

    PST1 = PlottingSettings(t.obj$obs$data)
    PST3 = PlottingSettings(t.obj$obs3$data)
    PST4 = PlottingSettings(t.obj$obs4$data)
    PST6 = PlottingSettings(t.obj$obs6$data)
    # the var name needs to be increasing in name for further usage
    PS = list(PST1=PST1, PST2=PST3, PST3=PST4, PST4=PST6)
    fname.new = gsub("-extremes_", "_HErZ-Lindenberg_diffHeights_", fname)
    PlotTowerExtremesList(fname.new, scores.lst, ylims.df, threshold, PS,
                          use.ylims=F)
    fname.new = gsub("-extremes_", "_inverse_HErZ-Lindenberg_diffHeights_", fname)
    PlotTowerExtremesList(fname.new, scoresI.lst, ylims.df, threshold, PS,
                          use.ylims=T)
    fname.new = gsub("-extremes_", "_HErZ-Lindenberg_diffHeights-HRvsFAR_", fname)
    PlotTowerHRvsFARList(fname.new, scores.lst, threshold, PS)
    fname.new = gsub("-extremes_", "_inverse_HErZ-Lindenberg_diffHeights-HRvsFAR_", fname)
    PlotTowerHRvsFARList(fname.new, scoresI.lst, threshold, PS)

  } else if (t.obj$obs$data$StationName[1] == "Cabauw") {

    # against COSMO-REA6 at 10m
    obs = t.obj$obs6$data$wind_speed
    forec = t.obj$herz10$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ10m-C10m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs6$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ10m-C10m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at 35m
    obs = t.obj$obs4$data$wind_speed
    forec = t.obj$herz35$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ35m-C40m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs4$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ35m-C40m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at 69m
    obs = t.obj$obs3$data$wind_speed
    forec = t.obj$herz69$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ69m-C80m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs3$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ69m-C80m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at 116m
    obs = t.obj$obs2$data$wind_speed
    forec = t.obj$herz116$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ116m-C140m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs2$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ116m-C140m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    if (ana.time.res$time.res != ana.time.res$hourly) {
      # against ERA20C at 100m
      obs = t.obj$obs3$data$wind_speed
      forec = t.obj$era20c100$data$wind_speed
      scores.df = GetScoresDF(threshold, obs, forec)
      ylims.df = as.data.frame(YLimsScores())
      fname.new = gsub("-extremes_", "_ERA20C100m-C80m-extremes_", fname)
      PS["tower.height"] = as.character(t.obj$obs3$data$height[1])
      PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
      fname.new = gsub("-extremes_", "_ERA20C100m-C80m-extremes-HRvsFAR_", fname)
      PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                       scores.df$hit.rate, threshold, PS)

      # against ERA20C at 100m
      obs = t.obj$obs2$data$wind_speed
      forec = t.obj$era20c100$data$wind_speed
      scores.df = GetScoresDF(threshold, obs, forec)
      ylims.df = as.data.frame(YLimsScores())
      fname.new = gsub("-extremes_", "_ERA20C100m-C140m-extremes_", fname)
      PS["tower.height"] = as.character(t.obj$obs2$data$height[1])
      PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
      fname.new = gsub("-extremes_", "_ERA20C100m-C140m-extremes-HRvsFAR_", fname)
      PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                       scores.df$hit.rate, threshold, PS)
    }

    # against COSMO-REA6 at 178m
    obs = t.obj$obs$data$wind_speed
    forec = t.obj$herz178$data$wind_speed
    scores.df = GetScoresDF(threshold, obs, forec)
    ylims.df = as.data.frame(YLimsScores())
    fname.new = gsub("-extremes_", "_HErZ178m-C200m-extremes_", fname)
    PS["tower.height"] = as.character(t.obj$obs$data$height[1])
    PlotTowerExtremes(fname.new, scores.df, ylims.df, threshold, PS)
    fname.new = gsub("-extremes_", "_HErZ178m-C200m-extremes-HRvsFAR_", fname)
    PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                     scores.df$hit.rate, threshold, PS)

    # against COSMO-REA6 at different heights
    obs = t.obj$obs$data$wind_speed
    forec = t.obj$herz178$data$wind_speed
    scores.178.df = GetScoresDF(threshold, obs, forec)
    scoresI.178.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs2$data$wind_speed
    forec = t.obj$herz116$data$wind_speed
    scores.116.df = GetScoresDF(threshold, obs, forec)
    scoresI.116.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs3$data$wind_speed
    forec = t.obj$herz69$data$wind_speed
    scores.69.df = GetScoresDF(threshold, obs, forec)
    scoresI.69.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs4$data$wind_speed
    forec = t.obj$herz35$data$wind_speed
    scores.35.df = GetScoresDF(threshold, obs, forec)
    scoresI.35.df = GetScoresDF(threshold, obs, forec, inverse=T)
    obs = t.obj$obs6$data$wind_speed
    forec = t.obj$herz10$data$wind_speed
    scores.10.df = GetScoresDF(threshold, obs, forec)
    scoresI.10.df = GetScoresDF(threshold, obs, forec, inverse=T)
    scores.lst = list(scores.178.df, scores.116.df, scores.69.df, scores.35.df, scores.10.df)
    scoresI.lst = list(scoresI.178.df, scoresI.116.df, scoresI.69.df, scoresI.35.df, scoresI.10.df)
    ylims.df = as.data.frame(YLimsScores())

    PST1 = PlottingSettings(t.obj$obs$data)
    PST2 = PlottingSettings(t.obj$obs2$data)
    PST3 = PlottingSettings(t.obj$obs3$data)
    PST4 = PlottingSettings(t.obj$obs4$data)
    PST6 = PlottingSettings(t.obj$obs6$data)
    # the var name needs to be increasing in name for further usage
    PS = list(PST1=PST1, PST2=PST2, PST3=PST3, PST4=PST4, PST5=PST6)

    fname.new = gsub("-extremes_", "_HErZ-Cabauw_diffHeights_", fname)
    PlotTowerExtremesList(fname.new, scores.lst, ylims.df, threshold, PS,
                          use.ylims=T)
    fname.new = gsub("-extremes_", "_inverse_HErZ-Cabauw_diffHeights_", fname)
    PlotTowerExtremesList(fname.new, scoresI.lst, ylims.df, threshold, PS,
                          use.ylims=T)
    fname.new = gsub("-extremes_", "_HErZ-Cabauw_diffHeights-HRvsFAR_", fname)
    PlotTowerHRvsFARList(fname.new, scores.lst, threshold, PS)
    fname.new = gsub("-extremes_", "_inverse_HErZ-Cabauw_diffHeights-HRvsFAR_", fname)
    PlotTowerHRvsFARList(fname.new, scoresI.lst, threshold, PS)

  }
}

#-----------------------------------------------------------------------------------

#' @title Control function to prepare plotting random data.
#' @param random.obj ClimObject holding the random data.
#' @param fname string holding the file name of the plot.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @export
PlotRandomExtremesContr <- function(random.obj, fname, threshold) {
  r.obj = random.obj$climate_data_objects
  PS = PlottingSettings(r.obj$obs$data)

  obs = r.obj$obs$data$wind_speed
  forec = r.obj$forec$data$wind_speed
  scores.df = GetScoresDF(threshold, obs, forec)
  ylims.df = as.data.frame(YLimsScores())
  title.name = " of random data"
  PlotTowerExtremes(fname, scores.df, ylims.df, threshold, PS, title.name)
  fname.new = gsub("-extremes", "-extremes-HRvsFAR", fname)
  title.name = "Hit rate vs False alarm rate of random data"
  PlotTowerHRvsFAR(fname.new, scores.df$false.alarm.ratio,
                   scores.df$hit.rate, threshold, PS, title.name)
}

#-----------------------------------------------------------------------------------

#' @title Plot all skill scores for tower observations.
#' @description Plot twelve panels of skill scores for the tower measurements.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param scores.df is a data.frame holding the skill score values corresponding to the
#'   threshold as calculated by function \code{\link{GetScoresDF}}.
#' @param ylims.df data.frame holding the range values of the skill scores as provided
#'   by function \code{\link{YLimsScores}}.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param title.name default value is NULL, but a string can be passed for the title.
#' @param use.ylims is boolean which decides whether to use the ylim settings of
#'   function \code{\link{YLimsScores}},
PlotTowerExtremes <- function(fname, scores.df, ylims.df, threshold, PS,
                              title.name=NULL, use.ylims=F) {

  score.names = names(scores.df)
  pdf(fname, width=PS$land.a4width, height=PS$land.a4height)
  par(mfrow=c(2,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,0), cex=0.8)

  for (plot.step in seq(score.names)) {
    if (all(!is.finite(scores.df[[plot.step]]))) next
    if (is.null(title.name)) {
      titleName = paste0(score.names[plot.step]," of ", PS$time.agg, " ", PS$obs.height,
                         " WS at ", PS$obs.name, " vs ", PS$rea.name)
    } else {
      titleName = gsub(" of", paste0(score.names[plot.step], " of"), title.name)
    }
    if (use.ylims |
        score.names[plot.step] == "odds.ratio" & max(scores.df[[plot.step]]<100)) {
      plot(threshold, scores.df[[plot.step]], xlab="percentile",
           ylab=score.names[plot.step], col = "blue", pch=16, type="b",
           main = titleName)
    } else {
      plot(threshold, scores.df[[plot.step]], xlab="percentile",
           ylab=score.names[plot.step], col = "blue", pch=16, type="b",
           main = titleName, ylim=c(ylims.df[[plot.step]][1],
                                    ylims.df[[plot.step]][2]))
    }
  }

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot all skill scores for tower observations at different heitht levels.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param scores is a data.frame holding the skill score values corresponding to the
#'   threshold as calculated by function \code{\link{GetScoresDF}}.
#' @param ylims.df data.frame holding the range values of the skill scores as provided
#'   by function \code{\link{YLimsScores}}.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param title.name default value is NULL, but a string can be passed for the title.
#' @param use.ylims is boolean which decides whether to use the ylim settings of
#'   function \code{\link{YLimsScores}},
PlotTowerExtremesList <- function(fname, scores, ylims.df, threshold, PS,
                                  title.name=NULL, use.ylims=F) {

  score.names = names(scores[[1]])
  col.names = c("green", "blue", "red", "magenta", "black", "orange")
  pdf(fname, width=PS$PST1$land.a4width, height=PS$PST1$land.a4height)
  par(mfrow=c(2,2), oma=c(0.5,0.5,0.5,0.5), mar=c(2,2,2,0), cex=0.8)

  for (plot.step in seq(score.names)) {
    for (cnt in seq(scores)) {
      if (all(!is.finite(scores[[cnt]][[plot.step]]))) next
      if (cnt==1) {
        if (is.null(title.name)) {
          titleName = paste0(score.names[plot.step]," of ", PS$PST1$time.agg, " means at ",
                             PS$PST1$obs.name, " at different heights")
        } else {
          titleName = gsub(" of", paste0(score.names[plot.step], " of"), title.name)
        }
      }
      if (use.ylims |
          score.names[plot.step] == "odds.ratio" & max(scores[[cnt]][[plot.step]]<100)) {
        if (cnt==1) {
          min.val = 100
          max.val = -100
          for (i in seq(scores)) {
            min.val = min(min.val, min(scores[[i]][[plot.step]]), na.rm=T)
            max.val = max(max.val, max(scores[[i]][[plot.step]]), na.rm=T)
          }
          plot(threshold, scores[[cnt]][[plot.step]], col = col.names[[cnt]],
               pch=16, type="b", main = titleName, ylim=c(min.val, max.val))
        } else {
          lines(threshold, scores[[cnt]][[plot.step]], col=col.names[[cnt]],
                pch=16, type="b")
        }
      } else {
        if (cnt==1) {
          plot(threshold, scores[[cnt]][[plot.step]], col=col.names[[cnt]],
               pch=16, type="b", main = titleName, ylim=c(ylims.df[[plot.step]][1],
                                                          ylims.df[[plot.step]][2]))
        } else {
          lines(threshold, scores[[cnt]][[plot.step]], col=col.names[[cnt]],
                pch=16, type="b")
        }
      }
    }
    # placement of legend depending on plot of score
    if (plot.step <= 3 | plot.step >= 9 & plot.step <= 11) legend.place="top"
    if (plot.step >= 4 & plot.step <= 6 | plot.step == 8 | plot.step == 12) legend.place="bottom"
    if (plot.step == 7) legend.place == "topleft"
    if (length(scores) == 5) { # now this is Cabauw
      legend(legend.place, legend=c(as.character(PS$PST1$obs.height),
                                    as.character(PS$PST2$obs.height),
                                    as.character(PS$PST3$obs.height),
                                    as.character(PS$PST4$obs.height),
                                    as.character(PS$PST5$obs.height)),
             pch=16, col=col.names[1:length(scores)],
             text.col=col.names[1:length(scores)])
    } else if (length(scores) == 4) { # now this is Lindenberg
      legend(legend.place, legend=c(as.character(PS$PST1$obs.height),
                                    as.character(PS$PST2$obs.height),
                                    as.character(PS$PST3$obs.height),
                                    as.character(PS$PST4$obs.height)),
             pch=16, col=col.names[1:length(scores)],
             text.col=col.names[1:length(scores)])
    } else if (length(scores) == 3) { # comparison of all RRAs (except MF)
      legend(legend.place, legend=c(as.character(PS$PST1$rea.name),
                                    as.character(PS$PST2$rea.name),
                                    as.character(PS$PST3$rea.name)),
             pch=16, col=col.names[1:length(scores)],
             text.col=col.names[1:length(scores)])
    }
  }

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Hit rate and false alarm ratio plotted into the same plot.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param FAR false alarm ratio values corresponding to the threshold derived of
#'   function \code{\link{GetScoresDF}}.
#' @param HR as above for hit rate values.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @param PS is a plotting setting object (list) derived of function
#'   \code{\link{PlottingSettings}}.
#' @param title.name default value is NULL, but a string can be passed for the title.
PlotTowerHRvsFAR <- function(fname, FAR, HR, threshold, PS,
                             title.name=NULL) {

  pdf(fname, width=PS$land.a4width, height=PS$land.a4height)

  if(is.null(title.name)) {
    title.name = paste0("Hit rate and False alarm ratio of ", PS$time.agg, " ",
                        PS$obs.height, " WS at ", PS$obs.name, " vs ", PS$rea.name)
  }
  plot(threshold, HR, xlim = c(0,1), ylim=c(0,1), col="blue", pch=16, type="p",
       main = title.name, xlab="", ylab="")
  lines(threshold, FAR, col="red", pch=16, type="p", xlab="", ylab="")

  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot hit rate vs false alarm ratio at different heights.
#' @description Plot hit rate vs false alarm ratio of tower measurements at different
#'   height levels or of different RRAs.
#' @param fname is a string holding file name of the pdf plot to be created.
#' @param scores is a data.frame holding the skill score values corresponding to the
#'   threshold as calculated by function \code{\link{GetScoresDF}}.
#' @param threshold numeric sequence of percentiles between 0 and 1
#' @param PS is a plotting setting object (list) derived of function
#' @param title.name a string as title can be passed, otherwise it is NULL by default.
PlotTowerHRvsFARList <- function(fname, scores, threshold, PS, title.name=NULL) {

  score.names = names(scores[[1]])
  col.names = c("green", "blue", "red", "magenta", "black", "orange")
  pdf(fname, width=PS$PST1$land.a4width, height=PS$PST1$land.a4height)

  if(is.null(title.name)) {
    title.name = paste0("Hit rate vs False alarm ratio of ", PS$PST1$time.agg,
                        " means at ", PS$PST1$obs.name, " at different heights")
  }

  for (cnt in seq(scores)) {
    if (cnt==1) {
      plot(threshold, scores[[cnt]]$hit.rate, xlim = c(0,1), ylim=c(0,1),
           col=col.names[[cnt]], pch=16, type="p", main = title.name, xlab="", ylab="")
      lines(threshold, scores[[cnt]]$false.alarm.ratio, col=col.names[[cnt]], pch=16,
            type="p", xlab="", ylab="")
    } else {
      lines(threshold, scores[[cnt]]$hit.rate, col=col.names[[cnt]], pch=16,
            type="p")
      lines(threshold, scores[[cnt]]$false.alarm.ratio, col=col.names[[cnt]], pch=16,
            type="p")
    }
  }
  if (length(scores) == 5) { # now this is Cabauw
    legend("topright", legend=c(as.character(PS$PST1$obs.height),
                                as.character(PS$PST2$obs.height),
                                as.character(PS$PST3$obs.height),
                                as.character(PS$PST4$obs.height),
                                as.character(PS$PST5$obs.height)),
           pch=16, col=col.names[1:length(scores)],
           text.col=col.names[1:length(scores)])
  } else if (length(scores) == 4) { # now this is Lindenberg
    legend("topright", legend=c(as.character(PS$PST1$obs.height),
                                as.character(PS$PST2$obs.height),
                                as.character(PS$PST3$obs.height),
                                as.character(PS$PST4$obs.height)),
           pch=16, col=col.names[1:length(scores)],
           text.col=col.names[1:length(scores)])
  } else if (length(scores) == 3) { # comparison of all RRAs (except MF)
    legend("topright", legend=c(as.character(PS$PST1$rea.name),
                                as.character(PS$PST2$rea.name),
                                as.character(PS$PST3$rea.name)),
           pch=16, col=col.names[1:length(scores)],
           text.col=col.names[1:length(scores)])
  }

  dev.off()
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
#' @param plot.col is a string holding the plotting color which is set by default to
#'   "blue".
scatterPlot <- function(X, Y, yliml, ylimh, titname, xlabname, ylabname,
                        text.str=NULL, xaxis='s', yaxis='s', plot.col="blue") {

  if ((!xaxis=='n' & !xaxis=='s') & (!yaxis=='n' & !yaxis=='s')) {
    CallStop(paste0("Either xaxis or yaxis are not set as expected.\n",
                    "   The allowed values are 's' or 'n'\n",
                    "   xaxis = ", xaxis, " yaxis = ", yaxis))
  }

  plot(X, Y, pch=19,
       xlim=c(yliml,ylimh), ylim=c(yliml, ylimh),
       main=titname, xlab=xlabname, ylab=ylabname, col=plot.col,
       xaxt=xaxis, yaxt=yaxis)
  lines(c(yliml-1,ylimh), c(yliml-1,ylimh), col="gray60")
  lm.model = lm(Y ~ X)
  abline(lm.model, col="black")
  if(!is.null(text.str)) {
    #     text(yliml, ((ylimh-yliml)/2)+yliml,
    #          paste(text.str), adj=c(0, 0.5))
    text(1.1*yliml, ylimh-0.1*ylimh,
         paste0(text.str, "\nIntercept = ", round(lm.model$coefficients[1], 2),
                "\nSlope = ", round(lm.model$coefficient[2], 2)),
         adj=c(0, 0.5), col="black")
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
#' @export
histoPlot <- function(X, Y, breaks, xlims, titname='', xlabname='', ylabname='',
                      xaxis='s', yaxis='s', axis.cex=1.2, tit.cex=1.1,
                      addPlot=FALSE) {

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
    mtext(xlabname, side=1, line=2, cex=axis.cex)
  } else {
    mtext(xlabname, side=1, line=0, cex=axis.cex)
  }
  if(yaxis == 's') {
    mtext(ylabname, side=2, line=2, cex=axis.cex)
  } else {
    mtext(ylabname, side=2, line=0, cex=axis.cex)
  }
  mtext(titname, side=3, line=1, cex=tit.cex)
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
    text(yliml, ((ylimh-yliml)/2)+yliml,
         paste(text.str), adj=c(0, 0.5))
  }
}

#-----------------------------------------------------------------------------------

#' @title Plot legend with statistics for histogram plot.
#' @description Plot the legend for histogram plots with statistics (mean, median,
#'   etc) and weibull parameters which are calculated.
#' @param xlims the limits of the x-axis in the form c(low,high)
#' @param vals vector of the plotted values of which the statistics are calculated
#' @importFrom fitdistrplus fitdist
#' @export
plotLegendStats <- function(xlims, vals, weibull=TRUE) {

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
           legend = c(paste0("n = ", length(vals),
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
