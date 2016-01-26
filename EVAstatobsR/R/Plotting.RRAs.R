#' @title Plot four RRA time series of monthly, daily, hourly data.
#' @description
#' @param
#' @return
LegendWithCorr <- function(obs.vals, rra.vals, conf.lev,
                           stat.name, rra.name, leg.pos,
                           col.rra, col.obs, leg.cex) {

  corr = cor.test(obs.vals, rra.vals, conf.level=conf.lev)
  legend(leg.pos, legend=c(paste0("corr = ", round(corr$estimate, 2),
                                  " (", round(corr$conf.int[[1]], 2), ", ",
                                  round(corr$conf.int[[2]], 2), ")"),
                           stat.name, rra.name),
         text.col=c("black", col.obs, col.rra), cex=leg.cex)
}

#-----------------------------------------------------------------------------------

#' @title Plot four RRA time series of monthly, daily, hourly data.
#' @description
#' @param
#' @return
GetRRAxts <- function(herz.obj, smhi.obj, mo.obj, mf.obj, ana.time.res, fname) {

  if (ana.time.res$time.res == ana.time.res$monthly |
      ana.time.res$time.res == ana.time.res$daily) {
    titname = paste0("Monthly 10m wind speed at ", station.name, " against ")
    herz.rra.xts = xts(herz.obj$rra10$data$wind_speed, order.by=herz.obj$rra10$data$date)
    herz.obs.xts = xts(herz.obj$obs$data$wind_speed, order.by=herz.obj$obs$data$date)
    smhi.rra.xts = xts(smhi.obj$rra10$data$wind_speed, order.by=smhi.obj$rra10$data$date)
    smhi.obs.xts = xts(smhi.obj$obs$data$wind_speed, order.by=smhi.obj$obs$data$date)
    mo.rra.xts = xts(mo.obj$rra10$data$wind_speed, order.by=mo.obj$rra10$data$date)
    mo.obs.xts = xts(mo.obj$obs$data$wind_speed, order.by=mo.obj$obs$data$date)
    mf.rra.xts = xts(mf.obj$rra10$data$wind_speed, order.by=mf.obj$rra10$data$date)
    mf.obs.xts = xts(mf.obj$obs$data$wind_speed, order.by=mf.obj$obs$data$date)
  } else  if (ana.time.res$time.res == ana.time.res$hourly) {
    if (!hourly.switch) { # rra and obs only at rra
      fname = gsub(".pdf", "_ObsAtRRA.pdf", fname)
      titname = paste0("10m Wind speed at ", station.name, "  against ")
      herz.rra.xts = xts(herz.obj$rra10$data$wind_speed, order.by=herz.obj$rra10$data$date)
      herz.obs.xts = xts(herz.obj$obs$data$wind_speed, order.by=herz.obj$obs$data$date)
      smhi.rra.xts = xts(smhi.obj$rra10$data$wind_speed, order.by=smhi.obj$rra10$data$date)
      smhi.obs.xts = xts(smhi.obj$stats.atrra10$data$wind_speed, order.by=smhi.obj$stats.atrra10$data$date)
      mo.rra.xts = xts(mo.obj$rra10$data$wind_speed, order.by=mo.obj$rra10$data$date)
      mo.obs.xts = xts(mo.obj$stats.atrra10$data$wind_speed, order.by=mo.obj$stats.atrra10$data$date)
      mf.rra.xts = xts(mf.obj$rra10$data$wind_speed, order.by=mf.obj$rra10$data$date)
      mf.obs.xts = xts(mf.obj$stats.atrra10$data$wind_speed, order.by=mf.obj$stats.atrra10$data$date)
    } else { # hourly rra (with na) and hourly obs
      fname = gsub(".pdf", "_HourlyObs.pdf", fname)
      titname = paste0("Hourly 10m wind speed at ", station.name, " against ")
      herz.rra.xts = xts(herz.obj$rra10$data$wind_speed, order.by=herz.obj$rra10$data$date)
      herz.obs.xts = xts(herz.obj$obs$data$wind_speed, order.by=herz.obj$obs$data$date)
      smhi.rra.xts = xts(smhi.obj$rra10.hourly$data$wind_speed, order.by=smhi.obj$rra10.hourly$data$date)
      smhi.obs.xts = xts(smhi.obj$obs$data$wind_speed, order.by=smhi.obj$obs$data$date)
      mo.rra.xts = xts(mo.obj$rra10.hourly$data$wind_speed, order.by=mo.obj$rra10.hourly$data$date)
      mo.obs.xts = xts(mo.obj$obs$data$wind_speed, order.by=mo.obj$obs$data$date)
      mf.rra.xts = xts(mf.obj$rra10.hourly$data$wind_speed, order.by=mf.obj$rra10.hourly$data$date)
      mf.obs.xts = xts(mf.obj$obs$data$wind_speed, order.by=mf.obj$obs$data$date)
    }
  }

  return(list(HE.rra.xts=herz.rra.xts, HE.obs.xts=herz.obs.xts,
              SM.rra.xts=smhi.rra.xts, SM.obs.xts=smhi.obs.xts,
              MO.rra.xts=mo.rra.xts, MO.obs.xts=mo.obs.xts,
              MF.rra.xts=mf.rra.xts, MF.obs.xts=mf.obs.xts,
              f.name=fname, tit.name=titname))
}

#-----------------------------------------------------------------------------------

#' @title Plot four RRA time series of monthly, daily, hourly data.
#' @description
#' @param
#' @return
PlotRRAtimeSeries <- function(herz.obj, smhi.obj, mo.obj, mf.obj, fname,
                              hourly.switch, station.name) {

  obs.date <- as.POSIXlt(herz.obj$obs$data$date)

  Ylims = GetYlims(xts(herz.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(smhi.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(mo.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(mf.obj$obs$data$wind_speed, order.by=obs.date))
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  rra.xts = GetRRAxts(herz.obj, smhi.obj, mo.obj, mf.obj, ana.time.res, fname)
  herz.rra.xts = rra.xts$HE.rra.xts
  herz.obs.xts = rra.xts$HE.obs.xts
  smhi.rra.xts = rra.xts$SM.rra.xts
  smhi.obs.xts = rra.xts$SM.obs.xts
  mo.rra.xts = rra.xts$MO.rra.xts
  mo.obs.xts = rra.xts$MO.obs.xts
  mf.rra.xts = rra.xts$MF.rra.xts
  mf.obs.xts = rra.xts$MF.obs.xts
  fname = rra.xts$f.name
  titname = rra.xts$tit.name

  if (ana.time.res$time.res == ana.time.res$monthly |
      ana.time.res$time.res == ana.time.res$daily) {
    pch.rra = 19
    pch.obs = 18
    cex.size = 1.5
    legend.cex = 1.5
    col.obs = "black"
  } else {
    pch.rra = 19
    pch.obs = 18
    cex.size = 2.0
    legend.cex = 1.5
    col.obs = "gray60"
  }
  col.light.obs = "gray70"
  col.herz = "green"
  col.smhi = "deepskyblue"
  col.mo = "red"
  conf.lev = 0.95
  leg.pos = "topright"

  PS = PlottingSettings(herz.obj$rra10$data)
  pdf(fname, width=PS$port.a4width, height=PS$port.a4height,
      onefile=TRUE, pointsize=13)
  par(mfrow=c(4,1), mar=c(3,2,2,0), oma=c(0,1,0,0.5), cex.main=1.5)



  if (ana.time.res$time.res == ana.time.res$monthly |
      ana.time.res$time.res == ana.time.res$daily) {


    dummy = xts(numeric(length=length(herz.obj$rra10$data$wind_speed)) * NA,
                order.by=herz.obj$rra10$data$date)
    plot(dummy, ylim=c(yliml, ylimh), xaxt="n",
         main=paste0(titname, "HErZ reanalysis"))
    lines(herz.obs.xts, col=col.obs, pch=pch.obs, type="p", cex=cex.size)
    lines(herz.rra.xts, col=col.herz, pch=pch.rra, type="p", cex=cex.size)
    corr = cor.test(as.numeric(herz.obs.xts), as.numeric(herz.rra.xts),
                    conf.level=0.99)
    legend("topright", legend=c(paste0("corr = ", round(corr$estimate, 2),
                                       " (", round(corr$conf.int[[1]], 2), ", ",
                                       round(corr$conf.int[[2]], 2), ")"),
                                station.name, "HErZ"),
           text.col=c("black", "black", "green"), cex=legend.cex)

    plot(dummy, ylim=c(yliml, ylimh), xaxt="n",
         main=paste0(titname, "SMHI reanalysis"))
    lines(smhi.obs.xts, col=col.obs, pch=pch.obs, type="p", cex=cex.size)
    lines(smhi.rra.xts, col=col.smhi, pch=pch.rra, type="p", cex=cex.size)
    corr = cor.test(as.numeric(smhi.obs.xts[1:21]), as.numeric(smhi.rra.xts[1:21]),
                    conf.level=0.99)
    legend("topright", legend=c(paste0("corr = ", round(corr$estimate, 2),
                                       " (", round(corr$conf.int[[1]], 2), ", ",
                                       round(corr$conf.int[[2]], 2), ")"),
                                station.name, "SMHI"),
           text.col=c("black", "black", col.smhi), cex=legend.cex)

    plot(dummy, ylim=c(yliml, ylimh), xaxt="n",
         main=paste0(titname, "MetOffice reanalysis"))
    lines(mo.obs.xts, col=col.obs, pch=pch.obs, type="p", cex=cex.size)
    lines(mo.rra.xts, col=col.mo, pch=pch.rra, type="p", cex=cex.size)
    corr = cor.test(as.numeric(mo.obs.xts), as.numeric(mo.rra.xts),
                    conf.level=0.99)
    legend("topright", legend=c(paste0("corr = ", round(corr$estimate, 2),
                                       " (", round(corr$conf.int[[1]], 2), ", ",
                                       round(corr$conf.int[[2]], 2), ")"),
                                station.name, "MetOffice"),
           text.col=c("black", "black", col.mo), cex=legend.cex)

    plot(dummy, ylim=c(yliml, ylimh),
         main="Wind speed of HErZ, SMHI, and MetOffice reanalysis")
    lines(herz.rra.xts, col=col.herz, pch=pch.rra, type="p", cex=cex.size)
    lines(smhi.rra.xts, col=col.smhi, pch=pch.rra, type="p", cex=cex.size)
    lines(mo.rra.xts, col=col.mo, pch=pch.rra, type="p", cex=cex.size)
    lines(herz.obs.xts, col=col.light.obs, pch=pch.obs, type="p", cex=cex.size)
    # lines(mf.rra.xts, col="magenta", pch=pch.rra, type="p")
    legend("topright", legend=c("HErZ", "SMHI", "MetOffice"),
           text.col=c("green", col.smhi, col.mo), cex=legend.cex)

    dev.off()


  } else {

    start.time = "2009-08-08 00:00:00"
    end.time = "2009-08-10 23:00:00"
    idx.herz = (which(index(herz.rra.xts) >=
                        as.POSIXct(as.POSIXct(strptime(start.time, format="%Y-%m-%d %H:%M:%S"),
                                              format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
                      & index(herz.rra.xts) <=
                        as.POSIXct(as.POSIXct(strptime(end.time, format="%Y-%m-%d %H:%M:%S"),
                                              format="%Y-%m-%d %H:%M:%S", tz = "UTC"))))

    idx.smhi = (which(index(smhi.rra.xts) >=
                        as.POSIXct(as.POSIXct(strptime(start.time,
                                                       format="%Y-%m-%d %H:%M:%S"),
                                              format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
                      & index(smhi.rra.xts) <=
                        as.POSIXct(as.POSIXct(strptime(end.time, format="%Y-%m-%d %H:%M:%S"),
                                              format="%Y-%m-%d %H:%M:%S", tz = "UTC"))))
    idx.mo = (which(index(mo.rra.xts) >=
                      as.POSIXct(as.POSIXct(strptime(start.time, format="%Y-%m-%d %H:%M:%S"),
                                            format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
                    & index(mo.rra.xts) <=
                      as.POSIXct(as.POSIXct(strptime(end.time, format="%Y-%m-%d %H:%M:%S"),
                                            format="%Y-%m-%d %H:%M:%S", tz = "UTC"))))

    dummy.herz = xts(numeric(length=length(herz.rra.xts)) * NA, order.by=index(herz.rra.xts))
    plot(dummy.herz[idx.herz], ylim=c(yliml, ylimh), xaxt="n",
         main=paste0(titname, "HErZ reanalysis"))
    lines(herz.obs.xts[idx.herz], col=col.obs, pch=pch.obs, type="p", cex=cex.size)
    lines(herz.rra.xts[idx.herz], col=col.herz, pch=pch.rra, type="p", cex=cex.size)
    LegendWithCorr(as.numeric(herz.obs.xts[idx.herz]), as.numeric(herz.rra.xts[idx.herz]),
                   conf.lev=conf.lev, stat.name=station.name, rra.name="HErZ",
                   leg.pos="topright", col.rra=col.herz, col.obs=col.obs, leg.cex=legend.cex)

    dummy = xts(numeric(length=length(smhi.rra.xts)) * NA, order.by=index(smhi.rra.xts))
    plot(dummy[idx.smhi], ylim=c(yliml, ylimh), xaxt="n",
         main=paste0(titname, "SMHI reanalysis"))
    lines(smhi.obs.xts[idx.smhi], col=col.obs, pch=pch.obs, type="p", cex=cex.size)
    lines(smhi.rra.xts[idx.smhi], col=col.smhi, pch=pch.rra, type="p", cex=cex.size)
    LegendWithCorr(as.numeric(smhi.obs.xts[idx.smhi]), as.numeric(smhi.rra.xts[idx.smhi]),
                   conf.lev=conf.lev, stat.name=station.name, rra.name="SMHI",
                   leg.pos="topright", col.rra=col.smhi, col.obs=col.obs, leg.cex=legend.cex)

    dummy = xts(numeric(length=length(mo.rra.xts)) * NA, order.by=index(mo.rra.xts))
    plot(dummy[idx.mo], ylim=c(yliml, ylimh), xaxt="n",
         main=paste0(titname, "MetOffice reanalysis"))
    lines(mo.obs.xts[idx.mo], col=col.obs, pch=pch.obs, type="p", cex=cex.size)
    lines(mo.rra.xts[idx.mo], col=col.mo, pch=pch.rra, type="p", cex=cex.size)
    LegendWithCorr(as.numeric(mo.obs.xts[idx.mo]), as.numeric(mo.rra.xts[idx.mo]),
                   conf.lev=conf.lev, stat.name=station.name, rra.name="MetOffice",
                   leg.pos="topright", col.rra=col.mo, col.obs=col.obs, leg.cex=legend.cex)

    plot(dummy.herz[idx.herz], ylim=c(yliml, ylimh),
         main="Wind speed of HErZ, SMHI, and MetOffice reanalysis")
    lines(herz.rra.xts[idx.herz], col=col.herz, pch=pch.rra, type="p", cex=cex.size)
    lines(smhi.rra.xts[idx.smhi], col=col.smhi, pch=pch.rra, type="p", cex=cex.size)
    lines(mo.rra.xts[idx.mo], col=col.mo, pch=pch.rra, type="p", cex=cex.size)
    # lines(mf.rra.xts, col="magenta", pch=pch.rra, type="p")
    lines(herz.obs.xts, col=col.light.obs, pch=pch.obs, type="p", cex=cex.size)
    legend("topleft", legend=c("HErZ", "SMHI", "MetOffice"),
           text.col=c(col.herz, col.smhi, col.mo), cex=legend.cex)

    dev.off()

  }
}

#-----------------------------------------------------------------------------------

#' @title Plot four RRA time series of monthly, daily, hourly data.
#' @description
#' @param
#' @return
PlotRRAscatterQQ <- function(herz.obj, smhi.obj, mo.obj, mf.obj, fname,
                             hourly.switch, station.name) {

  obs.date <- as.POSIXlt(herz.obj$obs$data$date)

  Ylims = GetYlims(xts(herz.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(smhi.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(mo.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(mf.obj$obs$data$wind_speed, order.by=obs.date))
  yliml = Ylims$yll
  ylimh = Ylims$ylh

}
