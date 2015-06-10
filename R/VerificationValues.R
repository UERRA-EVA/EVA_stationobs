

#' paper: Identifying added value in high-resolution climate simulations over
#' Scandinavia, Mayer et al., 2015, tellus
#' @title
#' @description
#' @param
#' @return
PDFscore <- function(xts1, xts2) {

  min.step = 0
  max.step = 100
  size.step=0.5

  hist.xts1 = hist(xts1, breaks=seq(min.step, max.step, by=size.step), plot=FALSE)
  hist.xts2 = hist(xts2, breaks=seq(min.step, max.step, by=size.step), plot=FALSE)

  pdfscore <- size.step*sum(pmin(hist.xts1$density,hist.xts2$density))

  return(pdfscore)
}

#-----------------------------------------------------------------------------------

# KSTest <- functino(xts1, xts2) {
#   alpha = 0.05 # 0.01
#
#   D.s = max()
# }

#-----------------------------------------------------------------------------------


#' Determine correlations between time series if it is possible, i.e., if they spand
#' the same time period. Otherwise return NA.
#' @title
#' @description
#' @param
#' @return
GetCorrXts <- function(Era20cXts, EraIXts, HerzXts, StatXts) {
  if ((index(Era20cXts[1]) == index(EraIXts[1])) &
        (tail(index(Era20cXts), 1) == tail(index(EraIXts), 1))) {
    Corr.Era20cEraI = cor(Era20cXts, EraIXts)
  } else{
    Corr.Era20cEraI = NA
  }
  if ((index(Era20cXts[1]) == index(HerzXts[1])) &
        (tail(index(Era20cXts), 1) == tail(index(HerzXts), 1))) {
    Corr.Era20cHerz = cor(Era20cXts, HerzXts)
  } else{
    Corr.Era20cHerz = NA
  }

  if ((index(EraIXts[1]) == index(HerzXts[1])) &
        (tail(index(EraIXts), 1) == tail(index(HerzXts), 1))) {
    Corr.EraIHerz = cor(EraIXts, HerzXts)
  } else{
    Corr.EraIHerz = NA
  }

  if ((index(Era20cXts[1]) == index(StatXts[1])) &
        (tail(index(Era20cXts), 1) == tail(index(StatXts), 1))) {
    Corr.Era20cStat = cor(Era20cXts, StatXts)
  } else{
    Corr.Era20cStat = NA
  }
  if ((index(EraIXts[1]) == index(StatXts[1])) &
        (tail(index(EraIXts), 1) == tail(index(StatXts), 1))){
    Corr.EraIStat = cor(EraIXts, StatXts)
  } else{
    Corr.EraIStat = NA
  }
  if ((index(HerzXts[1]) == index(StatXts[1])) &
        (tail(index(HerzXts), 1) == tail(index(StatXts), 1))) {
    Corr.HerzStat = cor(HerzXts, StatXts)
  } else{
    Corr.HerzStat = NA
  }

  return(list(Corr.Era20cEraI, Corr.Era20cHerz, Corr.Era20cStat, Corr.EraIHerz,
              Corr.EraIStat, Corr.HerzStat))

}

#-----------------------------------------------------------------------------------
