#' @title Determine y-axis limits from data.
#' @description \code{GetYlims} determinies the low and high y-axis limits from four
#'   different time series.
#'   This needs to be enhanced so that not all of those time series need to be
#'   available. Something like present= in FORTRAN.
#' @param xts1,xts2,xts3,xts4 time series from which to determine the low and
#'   high range of the y-axis limits.
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

  legend("topleft", legend=c(paste0("Corr(ERA20C, Stat) = ", round(Corr.vals$c.20c.S, 2)),
                             paste0("Corr(ERAI, Stat) = ", round(Corr.vals$c.I.S, 2)),
                             paste0("Corr(HErZ, Stat) = ", round(Corr.vals$c.H.S, 2)),
                             paste0("Corr(ERA20C, ERAI)= ", round(Corr.vals$c.20c.I, 2)),
                             paste0("Corr(ERA20C, HErZ)= ", round(Corr.vals$c.20c.H, 2)),
                             paste0("Corr(ERAI, HErZ)= ", round(Corr.vals$c.I.H, 2))),
         text.col=c("blue", "red", "green", "black", "black", "black"))
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot seasonal time series of station data against locally corresponding
#'   global and reginal reanalyses
#' @description THIS FUNCTION IS NOT YET FINISHED.
#'   \code{PlotStationEraSeasons} plots seasonal means of station values against
#'   locally corresponding time series (pixels) of global and regional reanalyses.
#'   The rolling mean and optionally the anomalies are plotted. The plot is saved in
#'   pdf format and there is no return value.
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
PlotStationEraSeasons <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                  titname, outdir, fname, width, height,
                                  anomaly=FALSE, seasons=FALSE) {

  roll.mean = FALSE
  roll.time = 12

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
      onefile=TRUE, pointsize=13)

  eracsum = Era20cXts[.indexmon(Era20cXts) %in% c(5,6,7)] # JJA
  eracwin = Era20cXts[.indexmon(Era20cXts) %in% c(0,1,11)] # JFD

  eraisum = EraIXts[.indexmon(EraIXts) %in% c(5,6,7)]
  eraiwin = EraIXts[.indexmon(EraIXts) %in% c(0,1,11)]

  herzsum = HerzXts[.indexmon(HerzXts) %in% c(5,6,7)]
  herzwin = HerzXts[.indexmon(HerzXts) %in% c(0,1,11)]

  statsum = StatXts[.indexmon(StatXts) %in% c(5,6,7)]
  statwin = StatXts[.indexmon(StatXts) %in% c(0,1,11)]

  Ylims = GetYlims(eracwin, eraiwin, herzwin, statwin)
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  if (anomaly) {
    eracsum = eracsum - mean(eracsum)
    eracwin = eracwin - mean(eracwin)
    eraisum = eraisum - mean(eraisum)
    eraiwin = eraiwin - mean(eraiwin)
    herzsum = herzsum - mean(herzsum)
    herzwin = herzwin - mean(herzwin)
    statsum = statsum - mean(statsum)
    statwin = statwin - mean(statwin)
    yliml = -1.5
    ylimh = 1.5
  }

  # !!!!!!!!!!!!!!!!!!!!!!!!!
  # need to get a seasonal time series and plot all seasons
  # !!!!!!!!!!!!!!!!!!!!!!!!!

  # ERA20C
  dummy = numeric(length=length(eracwin)) * NA
  dummy = xts(dummy, order.by = index(eracwin))
  plot(dummy, main=titname, ylab="windspeed [m/s]", ylim=c(yliml, ylimh))

  if (roll.mean) {
    lines(rollmean(eracsum, roll.time), type="p", pch=21, col="blue", bg="blue", lw=2)
    lines(rollmean(eracwin, roll.time), type="p", pch=21, col="blue",
          bg="blue", lw=2)
  } else {
    lines(eracsum, type="p", pch=21, col="blue", bg="blue", lw=2)
    lines(eracwin, type="p", pch=21, col="blue",
          bg="blue", lw=2)
  }

  # ERA-I
  if (roll.mean) {
    lines(rollmean(eraisum, roll.time), type="p", pch=21, col="red", bg="red", lw=2)
    lines(rollmean(eraiwin, roll.time), type="p", pch=21, col="red", bg="red",
          lw=2)
  } else {
    lines(eraisum, type="p", pch=21, col="red", bg="red", lw=2)
    lines(eraiwin, type="p", pch=21, col="red", bg="red", lw=2)

  }
  # HErZ
  if (roll.mean) {
    lines(rollmean(herzsum, roll.time), type="p", pch=21, col="green3", bg="green3",
          lw=2)
    lines(rollmean(herzwin, roll.time), type="p", pch=21, col="green3",
          bg="green3", lw=2)
  } else {
    lines(herzsum, type="p", pch=21, col="green3", bg="green3", lw=2)
    lines(herzwin, type="p", pch=21, col="green3", bg="green3",
          lw=2)
  }

  # Station
  if (roll.mean) {
    lines(rollmean(statsum, roll.time), type="p", pch=21, col="black", bg="black",
          lw=2)
    lines(rollmean(statwin, roll.time), type="p", pch=21, col="black", bg="black",
          lw=2)
  } else {
    lines(statsum, type="p", pch=21, col="black", bg="black", lw=2)
    lines(statwin, type="p", pch=21, col="black", bg="black", lw=2)
  }

  legend("topleft", legend=c(paste0("ERA20C"),
                             paste0("ERAI"),
                             paste0("HErZ"),
                             paste0("Station")),
         text.col=c("blue", "red", "green3", "black"))
  dev.off()

}

#-----------------------------------------------------------------------------------

#' @title Plot only specific months of station data together with ERA20C, ERA-I,
#'   and HErZ.
#' @description
#'   \code{PlotStationEra} plots the station values together with the
#'   corresponding ERA20C, ERA-I, and HErZ pixel - for specific months only. These
#'   need to be set hard-coded within this function. Optionally, it is possible to
#'   plot the anomaly. The plot is saved in pdf format and there is no return value.
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
PlotStationEraMonths <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                 titname, outdir, fname, width, height,
                                 anomaly=FALSE) {

  # specify months to plot starting from 0 for January to 11 for December
  # within the list below
  months = list(0,7)
  all.months = c("January","February","March","April","May","June","July",
                 "August","September","October","November","December")
  mon.Era20c = list()
  mon.EraI = list()
  mon.Herz = list()
  mon.Stat = list()

  date.Era20c <- as.POSIXlt(index(Era20cXts))
  date.EraI <- as.POSIXlt(index(EraIXts))
  date.Herz <- as.POSIXlt(index(HerzXts))
  date.Stat <- as.POSIXlt(index(StatXts))

  for (cnt in seq(1,length(months))) {
    mon.Era20c[[cnt]] = Era20cXts[which( date.Era20c$mon==months[[cnt]] )]
    mon.EraI[[cnt]] = EraIXts[which( date.EraI$mon==months[[cnt]] )]
    mon.Herz[[cnt]] = HerzXts[which( date.Herz$mon==months[[cnt]] )]
    mon.Stat[[cnt]] = StatXts[which( date.Stat$mon==months[[cnt]] )]

    if (anomaly) {
      mon.Era20c[[cnt]] = mon.Era20c[[cnt]] - mean(mon.Era20c[[cnt]])
      mon.EraI[[cnt]] = mon.EraI[[cnt]] - mean(mon.EraI[[cnt]])
      mon.Herz[[cnt]] = mon.Herz[[cnt]] - mean(mon.Herz[[cnt]])
      mon.Stat[[cnt]] = mon.Stat[[cnt]] - mean(mon.Stat[[cnt]])
    }
  }

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
      onefile=TRUE, pointsize=13)

  yliml = vector(mode="numeric", length=length(months))
  ylimh = vector(mode="numeric", length=length(months))
  for (cnt in seq(1,length(months))) {
    Ylims = GetYlims(mon.Era20c[[cnt]], mon.EraI[[cnt]],
                     mon.Herz[[cnt]], mon.Stat[[cnt]])
    yliml[cnt] = Ylims$yll
    ylimh[cnt] = Ylims$ylh
  }
  yliml = min(yliml)
  ylimh = max(ylimh)

  dummy = numeric(length=length(mon.Era20c[[1]])) * NA
  dummy = xts(dummy, order.by = index(mon.Era20c[[1]]))

  par(mfrow=c(2,2))
  par(mar=c(0,0,0,0), oma=c(3,5,3,0.5))
  color = c("red", "black")

  plot(dummy, main=NULL, axes=FALSE, ylim=c(yliml, ylimh))
  axis(2)
  for (cnt in seq(1,length(months))) {
    lines(mon.Era20c[[cnt]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  legend("topleft", legend=c(paste0("ERA20C ", all.months[months[[1]]+1]),
                             paste0("ERA20C ", all.months[months[[2]]+1])),
         text.col=color)

  plot(dummy, main=NULL, axes=FALSE, ylim=c(yliml, ylimh))
  for (cnt in seq(1,length(months))) {
    lines(mon.EraI[[cnt]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  legend("topleft", legend=c(paste0("ERA-I ", all.months[months[[1]]+1]),
                             paste0("ERA-I ", all.months[months[[2]]+1])),
         text.col=color)

  plot(dummy, main=NULL, ylim=c(yliml, ylimh))
  for (cnt in seq(1,length(months))) {
    lines(mon.Herz[[cnt]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  legend("topleft", legend=c(paste0("HErZ ", all.months[months[[1]]+1]),
                             paste0("HErZ ", all.months[months[[2]]+1])),
         text.col=color)

  plot(dummy, main=NULL, yaxt="n", ylim=c(yliml, ylimh))
  for (cnt in seq(1,length(months))) {
    lines(mon.Stat[[cnt]], type="b", pch=21, col=color[cnt],
          bg=rgb(0,0,0,1./cnt), lw=2)
  }
  legend("topleft", legend=c(paste0("Station ", all.months[months[[1]]+1]),
                             paste0("Station ", all.months[months[[2]]+1])),
         text.col=color)

  mtext(titname, outer=TRUE)
  mtext("windspeed [m/s]", side=2, line=3, outer=TRUE)

  dev.off()

}

#-----------------------------------------------------------------------------------

#' @title Compare 100m wind speed of ERA20C and HErZ pixel by pixel.
#' @description \code{Plot100mEraHerz} compares the 100m wind speed of the ERA20C
#'   global reanalysis with the 116m wind speed of the HErZ regional reanalysis.
#'   This function performs a pixel wise comparison at the station locationto
#'   provided by the package. Scatter plots, QQplots, histogram plots, and the
#'   PDFscore are produced.
#' @param Era20cXts monthly mean extended time series of a ERA20C pixel
#' @param HerzXts same as above for HErZ
#' @param titname string of the plot title name
#' @param statname string of the station name whose pixel is plotted
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the file name of the plot
#' @param width,height of the plot in inches
Plot100mEraHerz <- function(Era20cXts, HerzXts,
                            titname, statname, outdir, fname, width, height) {

  same.length = F
  if (length(Era20cXts) == length(HerzXts)) {same.length = T}

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
    scatterPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname)

    titname = "Quantile-quantile plot"
    qqPlot(Era, Herz, yliml, ylimh, titname, xlabname, ylabname)

    titname = "Frequency distribution of ERA20C"

    min.val = floor(min(min(Era), min(Herz)))
    max.val = ceiling(max(max(Era), max(Herz)))
    breaks = seq(min.val, max.val, 0.25)
    dummy = numeric(length=length(Era)) * NA

    histoPlot(Era, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname)
    titname = "Frequency distribution of COSMO HErZ"
    xlabname = ylabname
    histoPlot(Herz, dummy, breaks, xlims=c(min.val, max.val), titname, xlabname)
    titname = paste0("Frequency distribution of ERA20C windspeed at 100m\n",
                     "in green and COSMO HErZ at 116m shaded")
    xlabname = "windspeed [m/s]"
    histoPlot(Era, Herz, breaks, xlims=c(min.val, max.val), titname, xlabname, addPlot=T)

    PlotMonthlyPDFScore(Era20cXts, HerzXts, outdir, paste0("PDFScore_100mEraHerz_",
                                                           statname,".pdf"),
                        "PDF Score between 100m Era20C and 116m HErZ windspeed [m/s]")
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
PlotMonthlyPDFScore <- function(era.xts, station.xts, outdir, fname, titname,
                                width, height) {

  pdf(paste(outdir, fname, sep=""), onefile=TRUE, pointsize=11, width=21./2.54)

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
    titname = "Histogram between \nreanalysis (green) and station data (shaded)"
    xlabname = "windspeed [m/s]"
    histoPlot(monthly.era, monthly.stat, breaks, xlims=c(min.val, max.val),
              titname, xlabname, addPlot=T)

    # par("usr") prvides the currently set axis limits
    #     text(min.val+0.1, par("usr")[4]-0.05, months[month+1], cex=2.)
    text(1+0.1, par("usr")[4]-0.05, months[month+1], cex=2.)

    PDF.score.anncycle[month+1] = PDFscore(monthly.era, monthly.stat)
  }
  PDF.score.ann[] = PDFscore(era.xts, station.xts)

  plot(PDF.score.anncycle, main=titname, ylab="pdf score", xlab="months of the year",
       type="b", pch=16, col="blue")
  lines(PDF.score.ann, type="b", lty=2, pch=20, col="red")

  vioplot(era.xts, station.xts, horizontal=TRUE,
          names=c("reanalysis", "station data"))

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
scatterPlot <- function(X, Y, yliml, ylimh, titname, xlabname, ylabname) {
  plot(X, Y, pch=19,
       xlim=c(yliml,ylimh), ylim=c(yliml, ylimh),
       main=titname, xlab=xlabname, ylab=ylabname, col="blue")
  lines(c(yliml-1,ylimh), c(yliml-1,ylimh))
  abline(lm(Y ~ X), col="blue")
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
#' @param addPlot optional boolean to determine whether to plot one (F) are two (T)
#'   data samples; default is to plot one data sample (addPlot=FALSE)
histoPlot <- function(X, Y, breaks, xlims, titname, xlabname, addPlot=FALSE) {
  if (addPlot) {
    # get high ylim for overplotting histograms
    hist1 = hist(X, breaks=breaks, plot=F)
    hist2 = hist(Y, breaks=breaks, plot=F)
    ylimh = ceiling(10.*max(hist1$density, hist2$density))/10.
    # plot both histograms
    hist(X, freq=F, breaks=breaks, xlim=xlims, ylim=c(0.0, ylimh),
         col="green", border="blue", main=titname, xlab=xlabname)
    hist(Y, freq=F, add=T, breaks=breaks, border="blue", density=10, angle=45)
  } else {
    hist(X, freq=F, breaks=breaks, xlim=xlims, col="green", border="blue",
         main=titname, xlab=xlabname)
    lines(density(X), col="red", lw=1.5)
  }
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
qqPlot <- function(X, Y, yliml, ylimh, titname, xlabname, ylabname) {
  qqplot(X, Y, pch=19,
         xlim=c(yliml-1,ylimh), ylim=c(yliml-1, ylimh),
         main=titname, xlab=xlabname, ylab=ylabname)
  abline(0,1)

}
#-----------------------------------------------------------------------------------
