#' @title Plot four RRA time series of monthly, daily, hourly data.
#' @description
#' @param
#' @return
PlotRRAtimeSeries <- function(herz.obj, smhi.obj, mo.obj, mf.obj, fname, hourly.switch) {

  obs.date <- as.POSIXlt(herz.obj$obs$data$date)

  Ylims = GetYlims(xts(herz.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(smhi.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(mo.obj$obs$data$wind_speed, order.by=obs.date),
                   xts(mf.obj$obs$data$wind_speed, order.by=obs.date))
  yliml = Ylims$yll
  ylimh = Ylims$ylh

  if (ana.time.res$time.res == ana.time.res$monthly |
      ana.time.res$time.res == ana.time.res$daily) {
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
      herz.rra.xts = xts(herz.obj$rra10$data$wind_speed, order.by=herz.obj$rra10$data$date)
      herz.obs.xts = xts(herz.obj$stats.atrra10$data$wind_speed, order.by=herz.obj$stats.atrra10$data$date)
      smhi.rra.xts = xts(smhi.obj$rra10$data$wind_speed, order.by=smhi.obj$rra10$data$date)
      smhi.obs.xts = xts(smhi.obj$stats.atrra10$data$wind_speed, order.by=smhi.obj$stats.atrra10$data$date)
      mo.rra.xts = xts(mo.obj$rra10$data$wind_speed, order.by=mo.obj$rra10$data$date)
      mo.obs.xts = xts(mo.obj$stats.atrra10$data$wind_speed, order.by=mo.obj$stats.atrra10$data$date)
      mf.rra.xts = xts(mf.obj$rra10$data$wind_speed, order.by=mf.obj$rra10$data$date)
      mf.obs.xts = xts(mf.obj$stats.atrra10$data$wind_speed, order.by=mf.obj$stats.atrra10$data$date)
    } else { # hourly rra (with na) and hourly obs
      fname = gsub(".pdf", "_HourlyObs.pdf", fname)
      herz.rra.xts = xts(herz.obj$rra10.hourly$data$wind_speed, order.by=herz.obj$rra10.hourly$data$date)
      herz.obs.xts = xts(herz.obj$obs$data$wind_speed, order.by=herz.obj$obs$data$date)
      smhi.rra.xts = xts(smhi.obj$rra10.hourly$data$wind_speed, order.by=smhi.obj$rra10.hourly$data$date)
      smhi.obs.xts = xts(smhi.obj$obs$data$wind_speed, order.by=smhi.obj$obs$data$date)
      mo.rra.xts = xts(mo.obj$rra10.hourly$data$wind_speed, order.by=mo.obj$rra10.hourly$data$date)
      mo.obs.xts = xts(mo.obj$obs$data$wind_speed, order.by=mo.obj$obs$data$date)
      mf.rra.xts = xts(mf.obj$rra10.hourly$data$wind_speed, order.by=mf.obj$rra10.hourly$data$date)
      mf.obs.xts = xts(mf.obj$obs$data$wind_speed, order.by=mf.obj$obs$data$date)
    }
  }


}
