#' @title Plot histogram plots for separately for publication purposes
#' @description The main difference is to plot only one plot per page
PlotHist_SeparatePlots <- function(fname, data.vals, data.date, titname) {

  dummy = xts(data.vals, order.by=data.date)
  min.val = floor(min(data.vals, na.rm=TRUE))
  max.val = ceiling(max(data.vals, na.rm=TRUE))

  dummy = numeric(length=length(data.vals)) * NA
  dummy = xts(dummy, order.by = data.date)
  breaks = seq(min.val, max.val, 0.2)

  PS = PlottingSettings(dummy)
  xlabname = "wind speed [m/s]"
  ylabname = "Density"

  xycex=1.5
  tit.cex=3.0
  lab.cex=2.0
  cex.text=1.5
  pdf(fname, width=PS$land.a4width/0.75, height=PS$land.a4height)
  par(mar=c(4,4,6,0.5), mgp=c(3,1,0), cex=xycex)

  histoPlot(data.vals, dummy, breaks, xlims=c(0,30),
            titname, xlabname, ylabname, axis.cex=xycex, lab.cex=lab.cex, tit.cex=tit.cex)
  plotLegendStats(xlims=c(min.val, max.val), as.numeric(data.vals),
                  cex.text=cex.text, weibull=F)

  dev.off()
}
