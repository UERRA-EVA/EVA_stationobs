#' @title A PDF based skill score.
#' @description This skill score is descripted in the paper by Mayer et al., 2015,
#'   Identifying added value in high-resolution climate simulations over Scandinavia,
#'   Tellus. It compares the PDFs of two populations, in this case here that of two
#'   reanalysis data products, or one reanalyis and one measurement data product.
#' @param xts1,xts2 are the two time series from which a PDF is determined and
#'   the PDF skill score calcualted
#' @return pdfscore is the PDF skill score
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

#' @title Correlation between two (extended) time series.
#' @description \code{GetCorrXts} calculates the correlation between two extended
#'   time series if it is possible, i.e., if they span the same time period. Otherwise
#'   return NA. This function is hardcoded to handle all combinations between four
#'   provided extended time series.
#' @param Era20cXts extended time series of an ERA20C pixel corresponding locally to
#'   the station time series
#' @param EraIXts same as above for ERA-Interim
#' @param HerzXts same as above for HErZ
#' @param StatXts same as above for the station values
#' @return list a list of all possible perturbations of correlations between the
#'   provided four extended time series.
GetCorrXts <- function(era20c=Era20cXts, eraI=EraIXts, herz=HerzXts, stat=StatXts) {
  if ((index(era20c[1]) == index(eraI[1])) &
        (tail(index(era20c), 1) == tail(index(eraI), 1))) {
    Corr.Era20cEraI = cor(era20c, eraI)
  } else{
    Corr.Era20cEraI = NA
  }
  if ((index(era20c[1]) == index(herz[1])) &
        (tail(index(era20c), 1) == tail(index(herz), 1))) {
    Corr.Era20cHerz = cor(era20c, herz)
  } else{
    Corr.Era20cHerz = NA
  }

  if ((index(eraI[1]) == index(herz[1])) &
        (tail(index(eraI), 1) == tail(index(herz), 1))) {
    Corr.EraIHerz = cor(eraI, herz)
  } else{
    Corr.EraIHerz = NA
  }

  if ((index(era20c[1]) == index(stat[1])) &
        (tail(index(era20c), 1) == tail(index(stat), 1))) {
    Corr.Era20cStat = cor(era20c, stat)
  } else{
    Corr.Era20cStat = NA
  }
  if ((index(eraI[1]) == index(stat[1])) &
        (tail(index(eraI), 1) == tail(index(stat), 1))){
    Corr.EraIStat = cor(eraI, stat)
  } else{
    Corr.EraIStat = NA
  }
  if ((index(herz[1]) == index(stat[1])) &
        (tail(index(herz), 1) == tail(index(stat), 1))) {
    Corr.HerzStat = cor(herz, stat)
  } else{
    Corr.HerzStat = NA
  }

  # naming means, e.g., for c.H.S: correlation between Herz and Station data
  return(list(c.20c.I=Corr.Era20cEraI, c.20c.H=Corr.Era20cHerz,
              c.20c.S=Corr.Era20cStat, c.I.H=Corr.EraIHerz,
              c.I.S=Corr.EraIStat, c.H.S=Corr.HerzStat))

}

#-----------------------------------------------------------------------------------
