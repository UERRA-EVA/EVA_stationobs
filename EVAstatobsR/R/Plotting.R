
#' @title Determine ylim ranges from data.
#' @description \code{GetYlims} determinies the ylim low and high range from four
#'   different time series.
#'   This needs to be enhanced so that not all of those time series need to be
#'   available. Something like present= in FORTRAN.
#' @param xts1, xts2, xts3, xts4 time series from which to determine the low and
#'   high range of the ylim.
#' @return yliml, ylimh a list of yliml and ylimh which are the lower and higher
#'   bond of ylim.
GetYlims <- function(xts1, xts2, xts3, xts4) {
  if (is.xts(xts1) & (is.xts(xts2)) & is.xts(xts3) & (is.xts(xts4))) {
    yliml = floor(min(min(xts1, na.rm=TRUE), min(xts2, na.rm=TRUE),
                      min(xts3, na.rm=TRUE), min(xts4, na.rm=TRUE)))
    ylimh = ceiling(max(max(xts1, na.rm=TRUE), max(xts2, na.rm=TRUE),
                        max(xts3, na.rm=TRUE), max(xts4, na.rm=TRUE)))
  } else {
    stop("\nXTS1 OR XTS2 OR XTS3 OR XTS4 IS NOT AN XTS !! ABORTING\n")
  }

  return(list(yliml, ylimh))
}

#-----------------------------------------------------------------------------------

#' @title Plot station measurements together with ERA20C, ERA-I, and HErZ.
#' @description \code{PlotStationEra} plot the station values together with the
#'   corresponding ERA20C, ERA-I, and HErZ pixel and provides the correlation between
#'   these time series. Optionally, it is possible to plot the anomaly. The plot is
#'   saved in pdf format and there is no return value.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param width, height of the plot in inches
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
  yliml = Ylims[[1]]
  ylimh = Ylims[[2]]

  # ERA20C
  dummy = numeric(length=length(Era20cXts)) * NA
  dummy = xts(dummy, order.by = index(Era20cXts))
  plot(dummy, main=titname, ylab="windspeed [m/s^2]", ylim=c(yliml, ylimh))

  if (monthly) { points(Era20cXts, type="b", pch=16, col="blue", lw=1.5) }
  if (!monthly & roll.mean) { points(rollmean(Era20cXts, roll.time),
                                     type="b", pch=16, col="blue", lw=1.5) }

  # ERA-I
  if (monthly) { points(EraIXts, type="b", pch=16, col="red", lw=1.5) }
  if (!monthly & roll.mean) { lines(rollmean(EraIXts, roll.time),
                                    type="b", pch=16, col="red", lw=1.5) }

  # HErZ
  if (monthly) { points(HerzXts, type="b", pch=16, col="green3", lw=1.5) }
  if (!monthly & roll.mean) { lines(rollmean(HerzXts, roll.time),
                                    type="b", pch=16, col="green3", lw=1.5) }

  # Station
  if (monthly) { points(StatXts, type="b", pch=16, col="black", lw=1.5) }
  if (!monthly & roll.mean) { lines(rollmean(StatXts, roll.time),
                                    type="b", pch=16, col="black", lw=1.5) }

  Corr.vals = GetCorrXts(Era20cXts, EraIXts, HerzXts, StatXts)

  legend("topleft", legend=c(paste0("Corr(ERA20C, Stat) = ", round(Corr.vals[[3]], 2)),
                             paste0("Corr(ERAI, Stat) = ", round(Corr.vals[[5]], 2)),
                             paste0("Corr(HErZ, Stat) = ", round(Corr.vals[[6]], 2)),
                             paste0("Corr(ERA20C, ERAI)= ", round(Corr.vals[[1]], 2)),
                             paste0("Corr(ERA20C, HErZ)= ", round(Corr.vals[[2]], 2)),
                             paste0("Corr(ERAI, HErZ)= ", round(Corr.vals[[4]], 2))),
         text.col=c("blue", "red", "green", "black", "black", "black"))
  dev.off()
}

#-----------------------------------------------------------------------------------

#' @title Plot seasonal time series of station data against locally corresponding
#'   global and reginal reanalyses
#' @description THIS FUNCTION IS NOT YET FINISHED.
#'   \code{PlotStationEraSeasons} plots seasonal means of station values against
#'   locally corresponding time series (pixels) of global and regional reanalyses. The
#'   rolling mean and optionally the anomalies are plotted. The plot is saved in pdf
#'   format and there is no return value.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param width, height of the plot in inches
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

  eracsum = Era20cXts[.indexmon(Era20cXts) %in% c(5,6,7)]
  eracwin = Era20cXts[.indexmon(Era20cXts) %in% c(0,1,11)]

  eraisum = EraIXts[.indexmon(EraIXts) %in% c(5,6,7)]
  eraiwin = EraIXts[.indexmon(EraIXts) %in% c(0,1,11)]

  herzsum = HerzXts[.indexmon(HerzXts) %in% c(5,6,7)]
  herzwin = HerzXts[.indexmon(HerzXts) %in% c(0,1,11)]

  statsum = StatXts[.indexmon(StatXts) %in% c(5,6,7)]
  statwin = StatXts[.indexmon(StatXts) %in% c(0,1,11)]

  Ylims = GetYlims(eracwin, eraiwin, herzwin, statwin)
  yliml = Ylims[[1]]
  ylimh = Ylims[[2]]

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
  plot(dummy, main=titname, ylab="windspeed [m/s^2]", ylim=c(yliml, ylimh))

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

#' @title Plot station measurements together with ERA20C, ERA-I, and HErZ for
#' specific months.
#' @description SCRIPT NEEDS TO BE FINIALIZED
#'   \code{PlotStationEra} plot the station values together with the
#'   corresponding ERA20C, ERA-I, and HErZ pixel and provides the correlation between
#'   these time series. Optionally, it is possible to plot the anomaly. The plot is
#'   saved in pdf format and there is no return value. The plot is
#'   saved in pdf format and there is no return value.
#' @param Era20cXts monthly mean extended time series of the ERA20C pixel
#'   corresponding to the station location
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts monthly mean extended time series of the station values
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param width, height of the plot in inches
#' @param monthly is an optional parameter which determines to plot the monthly
#'   values of the above time series
#' @param anomaly is an optional parameter which determines whether to plot anomalies
PlotStationEraMonths <- function(Era20cXts, EraIXts, HerzXts, StatXts,
                                 titname, outdir, fname, width, height,
                                 anomaly=FALSE) {

  roll.mean = FALSE
  roll.time = 12
  months = list(1,8)

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
      onefile=TRUE, pointsize=13)

  #
  # ==================================================================================
  #

#   date.eraC <-as.POSIXlt(index(Era20cXts))
#   date.eraI <-as.POSIXlt(index(EraIXts))
#   date.HErZ <-as.POSIXlt(index(HerzXts))
#   date.stat <-as.POSIXlt(index(StatXts))
#
#   monthly.eraC <- Era20cXts[which(date.eraC$mon==months)]
#   monthly.eraI <- EraIXts[which(date.eraI$mon==months)]
#   monthly.Herz <- HerzXts[which(date.Herz$mon==months)]
#   monthly.stat <- StatXts[which(date.stat$mon==months)]

  #
  # ==================================================================================
  #

  if (anomaly) {
    Era20cXts = Era20cXts - mean(Era20cXts)
    EraIXts = EraIXts - mean(EraIXts)
    HerzXts = HerzXts - mean(HerzXts)
    StatXts = StatXts - mean(StatXts)
  }
  Ylims = GetYlims(Era20cXts, EraIXts, HerzXts, StatXts)
  yliml = Ylims[[1]]
  ylimh = Ylims[[2]]

  # ERA20C
  dummy = numeric(length=length(Era20cXts)) * NA
  plot(dummy, main=titname, ylab="windspeed [m/s^2]", ylim=c(yliml, ylimh))

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

#' @title Compare 100m wind speed of ERA20C and HErZ pixel by pixel.
#' @description \code{Plot100mEraHerz} compares the 100m wind speed of the ERA20C
#'   global reanalysis with the 116m wind speed of the HErZ regional reanalysis. As of
#'   now THIS FUNCTION IS NOT YET FULLY FUNCTIONAL !!!
#'   This function will perform a pixel wise comparison to fit into the logic of this
#'   package. Later an areal comparison is envisaged.
#'   Scatter plots, PDFs, histograms, PDFscore, etc should all be considered as
#'   verification scores.
#' @param Era20cXts monthly mean extended time series of a ERA20C pixel
#' @param HerzXts same as above for HErZ
#' @param titname string of the plot title name
#' @param outdir string of the output directory into which the plot is saved
#' @param fname string of the file name of the plot
#' @param width, height of the plot in inches
Plot100mEraHerz <- function(Era20cXts, HerzXts,
                            titname, outdir, fname, width, height) {

  pdf(paste(outdir, fname, sep=""), width=width, height=height,
      onefile=TRUE, pointsize=13)

  dummy = numeric(length=length(Era20cXts)) * NA
  dummy = xts(dummy, order.by = index(Era20cXts))

  Ylims = GetYlims(Era20cXts, HerzXts, dummy, dummy)
  yliml = Ylims[[1]]
  ylimh = Ylims[[2]]

  plot(dummy, main=titname, ylab="windspeed [m/s^2]", ylim=c(yliml, ylimh))

  # ERA20C
  points(Era20cXts, type="b", pch=16, col="blue", lw=1.5)

  # HErZ
  points(HerzXts, type="b", pch=16, col="green3", lw=1.5)

  Corr.vals = GetCorrXts(Era20cXts, HerzXts, dummy, dummy)

  legend("topleft", legend=c(paste0("Corr(ERA20C, HErZ)= ", round(Corr.vals[[2]], 2)),
                             text.col=c("blue")))
  dev.off()

}
#-----------------------------------------------------------------------------------

#' @title to be filled in
#' @description to be filled in
#' @param to be filled in
#' @return to be filled in
PlotMonthlyPDFScore <- function(era.xts, station.xts, outdir, fname, titname,
                                width, height) {

  date.era <-as.POSIXlt(index(era.xts))
  date.stat <-as.POSIXlt(index(station.xts))

  PDF.score.anncycle = vector(mode="numeric", length=12)
  PDF.score.ann = vector(mode="numeric", length=12)

  for (month in seq(0,11)) {
    monthly.era <- era.xts[which(date.era$mon==month)]
    monthly.stat <- station.xts[which(date.stat$mon==month)]

    PDF.score.anncycle[month+1] = PDFscore(monthly.era, monthly.stat)
  }
  PDF.score.ann[] = PDFscore(era.xts, station.xts)

  pdf(paste(outdir, fname, sep=""), onefile=TRUE, pointsize=11, width=21./2.54)

  plot(PDF.score.anncycle, main=titname, ylab="pdf score", xlab="months of the year",
       type="b", pch=16, col="blue")
  lines(PDF.score.ann, type="b", lty=2, pch=20, col="red")

  dev.off()
}
#-----------------------------------------------------------------------------------
