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
#'   time series if it is possible, i.e., if they span the same time period.
#'   Otherwise return NA. This function is hardcoded to handle all combinations
#'   between four provided extended time series.
#' @param era20c extended time series of an ERA20C pixel corresponding locally to
#'   the station time series
#' @param eraI same as above for ERA-Interim
#' @param herz same as above for HErZ
#' @param stat same as above for the station values
#' @return Return a named list (c.20c.I=,c.20c.H=,c.20c.S=,c.I.H=,c.I.S=,c.H.S=)
#'   holding all possible perturbations of correlations between the provided four
#'   extended time series.
#' @importFrom zoo index
GetCorrXts <- function(era20c, eraI, herz, stat) {

  if (is.null(era20c)) {
    Corr.Era20cEraI = NA
    Corr.Era20cHerz = NA
    Corr.Era20cStat = NA
  }
  if (is.null(eraI)) {
    Corr.EraIHerz = NA
    Corr.EraIStat = NA
  }

  if (!is.null(era20c) & !is.null(eraI)) {
    if ((index(era20c[1]) == index(eraI[1])) &
        (tail(index(era20c), 1) == tail(index(eraI), 1))) {
      Corr.Era20cEraI = cor(era20c, eraI, use="pairwise.complete.obs")
    } else{
      Corr.Era20cEraI = NA
    }
  }

  if (!is.null(era20c)) {
    if (!is.null(era20c) & (index(era20c[1]) == index(herz[1])) &
        (tail(index(era20c), 1) == tail(index(herz), 1))) {
      Corr.Era20cHerz = cor(era20c, herz, use="pairwise.complete.obs")
    } else{
      Corr.Era20cHerz = NA
    }
  }

  if (!is.null(eraI)) {
    if (!is.null(eraI) & (index(eraI[1]) == index(herz[1])) &
        (tail(index(eraI), 1) == tail(index(herz), 1))) {
      Corr.EraIHerz = cor(eraI, herz, use="pairwise.complete.obs")
    } else{
      Corr.EraIHerz = NA
    }
  }

  if (!is.null(era20c)) {
    if (!is.null(era20c) & (index(era20c[1]) == index(stat[1])) &
        (tail(index(era20c), 1) == tail(index(stat), 1))) {
      Corr.Era20cStat = cor(era20c, stat, use="pairwise.complete.obs")
    } else{
      Corr.Era20cStat = NA
    }
  }

  if (!is.null(eraI)) {
    if (!is.null(eraI) & (index(eraI[1]) == index(stat[1])) &
        (tail(index(eraI), 1) == tail(index(stat), 1))){
      Corr.EraIStat = cor(eraI, stat, use="pairwise.complete.obs")
    } else{
      Corr.EraIStat = NA
    }
  }

  if ((index(herz[1]) == index(stat[1])) &
      (tail(index(herz), 1) == tail(index(stat), 1))) {
    Corr.HerzStat = cor(herz, stat, use="pairwise.complete.obs")
  } else{
    Corr.HerzStat = NA
  }

  # naming means, e.g., for c.H.S: correlation between Herz and Station data
  return(list(c.20c.I=Corr.Era20cEraI, c.20c.H=Corr.Era20cHerz,
              c.20c.S=Corr.Era20cStat, c.I.H=Corr.EraIHerz,
              c.I.S=Corr.EraIStat, c.H.S=Corr.HerzStat))

}

#-----------------------------------------------------------------------------------

#' @title Calculate the relative difference between two values.
#' @description Provided is a value and the mean value to which the relative
#'   difference is to be computed.
#' @param value is the value of which the relvative difference is to be calcualted
#'   compated to mean.value
#' @param mean.value is the mean value from above
#' @return is the relative difference
RelDiff <- function(value, mean.value) {
  return((value - mean.value) / mean.value)
}

#-----------------------------------------------------------------------------------

#' @title Calculate the norm of a value given the the upper and lower bound.
#' @description Given the upper and lower bound calculate the norm value.
#' @param value is the value of which the norm is to be calcualted. Needs to satisfay:
#'   min.value =< value =< max.val.
#' @param min.val,max.val are both the lower and upper bound
#' @return is the norm value.
NormVals <- function(value, min.val, max.val) {
  return((value-min.val)/(max.val-min.val))
}

#-----------------------------------------------------------------------------------

#' @title Calculate the 2x2 contingency table of the distribution between
#'   observations and forecasts (here: reanalysis)
#' @description This function calculates the 2x2 contigency table  of the
#'   distribution between observations and forecasts (here: reanalysis). The input
#'   are two numeric vectors of observations and forecasts (reanalysis) and a
#'   benchmark for which the table shall be calculated. The output is a names list
#'   holding the distribution in the variables a, b, c, d.
#' @param obs a numeric vector holding the observations. It is sorted in chronological
#'   order (it consists of the data values of an extended time series) and needs to
#'   have the same length as frcst.
#' @param frcst same as above but the the forecast (here: reanalysis) data.
#' @param benchmark value as percentile for which the contingency table values shall
#'   be calculated
#' @param inverse is a boolean to indicate whether to use the increasing order of
#'   benchmarks (percentiles) (F) or the inverse (decreasing) order (F) to calculate
#'   the contigency table
#' @return is a names list holding the distribution in the variables a, b, c, d.
#' @export
CalcContTable <- function(obs, frcst, benchmark, inverse) {

  # check for NA values and set to NA at same time steps to both time series
  idx = which(!is.finite(obs))
  frcst[idx] = NA
  idx = which(!is.finite(frcst))
  obs[idx] = NA
  abs.bench.obs = quantile(obs, benchmark, na.rm=T)
  abs.bench.frcst = quantile(frcst, benchmark, na.rm=T)

  cnt_a = 0
  cnt_b = 0
  cnt_c = 0
  cnt_d = 0

  if (length(obs) != length(frcst))
    CallStop(paste0("Length of observation and forecast vector should match; ",
                    "length(obs) = ", length(obs), "   length(frcst) = ", length(frcst)))

  if (!inverse) {
    for (steps in seq(obs)) {
      if (is.finite(obs[steps]) & is.finite(frcst[steps])) {
        if (obs[steps] >= abs.bench.obs) {
          if (frcst[steps] >= abs.bench.frcst) {
            cnt_a = cnt_a + 1
          } else {
            cnt_c = cnt_c + 1
          }
        } else {
          if (frcst[steps] >= abs.bench.frcst) {
            cnt_b = cnt_b + 1
          } else {
            cnt_d = cnt_d + 1
          }
        }
      }
    }
  } else {
    for (steps in seq(obs)) {
      if (is.finite(obs[steps]) & is.finite(frcst[steps])) {
        if (obs[steps] <= abs.bench.obs) {
          if (frcst[steps] <= abs.bench.frcst) {
            cnt_a = cnt_a + 1
          } else {
            cnt_c = cnt_c + 1
          }
        } else {
          if (frcst[steps] <= abs.bench.frcst) {
            cnt_b = cnt_b + 1
          } else {
            cnt_d = cnt_d + 1
          }
        }
      }
    }
  }
  return(list(a=cnt_a, b=cnt_b, c=cnt_c, d=cnt_d))
}

#-----------------------------------------------------------------------------------

#' @title Calcualte scores and skill scores based on the contigency table.
#' @description Input are only the for distribution values of the contingency table
#'   a, b, c, d. In this function twelve scores and skill scores are calculated,
#'   including: hit rate (probability of detection (POD)), false alarm rate
#'   (probability of false detection), false alarm ratio, Hanssen-Kuipers score
#'   (True Statistic, Pierce Kill Score), threat score (critical success index),
#'   equitable threat score (Gilbert skill score), frequency bias index, Heidke
#'   skill score, accuracy (percent correct), odds ratio, extremal dependence index,
#'   symmetric extremal dependence index (both from Ferro and Stephenson, 2011, doi:
#'   \url{10.1175/WAF-D-10-05030.1}).
#' @param a,b,c,d the four parameters of the contigency table (calculated by
#'   \code{\link{CalcContTable}}) which are also called hits, flase alarms, misses,
#'   and correct rejects, respectively.
#' @export
ContTableScores <- function(a, b, c, d) {

  n = a + b + c + d
  r = (a + b) * (a + c) / n

  # checks
  if (a == 0) cat(paste0("\n *** WARNING: Contigency table: a = 0"))
  if (b == 0) cat(paste0("\n *** WARNING: Contigency table: b = 0"))
  if (c == 0) cat(paste0("\n *** WARNING: Contigency table: c = 0"))
  if (d == 0) cat(paste0("\n *** WARNING: Contigency table: d = 0"))
  if (n == 0) cat(paste0("\n *** WARNING: Contigency table: n = 0"))
  if (r == 0) cat(paste0("\n *** WARNING: Contigency table: r = 0"))

  # calculate (skill) scores based on the contingency table
  POD = a / (a+c) # aka hit rate (probability of detection)
  POFD = b / (b+d) # aka false alarm rate (probability of false detection)
  FAR = b / (a+b) # false alarm ratio
  # Hanssen-Kuipers/True Skill Statistic/Pierce Skill Score (POD - POFD)
  HK = (a*d - b*c) / ( (a+c)*(b+d))
  TS = a / (a+b+c) # threat score or critical success index
  ETS = (a-r) / (a+b+c-r) # aka Gilbert Skill Score
  BIAS = (a+b) / (a+c) # aka frequency bias index
  HSS = 2*(a*d - b*c)/( (a+c)*(c+d) + (a+b)*(b+d)) # Heidke Skill Score
  PC = (a+d) / n # aka proportion/percent correct or accuracy
  OR = a*d / (b*c) # odds ratio
  EDI = (log(POFD) - log(POD)) / (log(POFD) + log(POD)) # aka extremal dependence index
  SEDI = (log(POFD) - log(POD) - log(1-POFD) + log(1-POD)) / # aka symmetric extremal
    (log(POFD) + log(POD) + log(1-POFD) + log(1-POD))        #      dependence index

  return(list(hit.rate=POD, false.alarm.rate=POFD, false.alarm.ratio=FAR,
              true.skill.stats=HK, threat.score=TS, equi.threat.score=ETS,
              frequency.bias.index=BIAS, heidke.sksc=HSS, accuracy=PC, odds.ratio=OR,
              edi=EDI, sedi=SEDI))
}

#-----------------------------------------------------------------------------------

#' @title Manual settings of the value of the skill score.
#' @description This function sets fixed values of the range of each skill score.
#'   The values are two concantenated numbers, i.e., c(x,y).
#' @return a list holding the range values for hit rate, false alarm rate, false
#'   alarm ratio, threat score, equitable threat score, bias, Heidke skill score,
#'   accuracy, odds ratio, (symmetric) dependence index.
#' @export
YLimsScores <- function() {

  POD.ylim = c(0,1)
  POFD.ylim = c(0,1)
  FAR.ylim = c(0,1)
  HK.ylim = c(-1,1)
  TS.ylim = c(0,1)
  ETS.ylim = c(-0.33,1)
  BIAS.ylim = c(0,2)
  HSS.ylim = c(-1,1)
  PC.ylim = c(0,1)
  OR.ylim = c(0,1000)
  EDI.ylim = c(-1,1)
  SEDI.ylim = c(-1,1)

  return(list(hit.rate.ylim=POD.ylim, false.alarm.rate.ylim=POFD.ylim,
              false.alarm.ratio.ylim=FAR.ylim,
              true.skill.stats.ylim=HK.ylim, threat.score.ylim=TS.ylim,
              equi.threat.score.ylim=ETS.ylim,
              bias.index.ylim=BIAS.ylim, heidke.sksc.ylim=HSS.ylim,
              accuracy.ylim=PC.ylim, odds.ratio.ylilm=OR.ylim,
              edi.yim=EDI.ylim, sedi.ylim=SEDI.ylim))
}

#-----------------------------------------------------------------------------------

#' @title Calculate contigency table based skill scores into a data frame.
#' @param thresh value as percentile for which the contingency table values shall
#'   be calculated. This is to be passed to \code{\link{CalcContTable}}.
#' @param obs a numeric vector holding the observations. This is to be passed to
#'   \code{\link{CalcContTable}}.
#' @param frcst a numeric vector holding the forecast (here: reanalysis) values.
#'   This is to be passed to \code{\link{CalcContTable}}.
#' @param inverse is a boolean to be passed to function \code{\link{CalcContTable}}
#'   to decide whether read the thresholds in increasing (F, default) or descreasing
#'   (T) order.
#' @return data.frame which holds the skill scores calculated by
#'   \code{\link{ContTableScores}}.
#' @export
GetScoresDF <- function(thresh, obs, frcst, inverse=F) {

  Cont.Table.cnt = CalcContTable(obs, frcst, thresh[1], inverse)
  scores = ContTableScores(Cont.Table.cnt$a, Cont.Table.cnt$b,
                           Cont.Table.cnt$c, Cont.Table.cnt$d)
  scores.df = as.data.frame(scores)

  if (length(thresh) > 1) {
    for (thresh.step in seq(length(thresh)-1)+1) {
      Cont.Table.cnt = CalcContTable(obs, frcst, thresh[thresh.step], inverse)
      scores = ContTableScores(Cont.Table.cnt$a, Cont.Table.cnt$b,
                               Cont.Table.cnt$c, Cont.Table.cnt$d)
      scores.df = rbind(scores.df, scores)
    }
  }

  return(scores.df)

}

#-----------------------------------------------------------------------------------

#' @title Prepare two exetended time series with random data.
#' @description This function prepares random observation and forecast (reanalysis)
#'   data in order to plot them and test the skill scores.
#' @param num is an integer number greater than zero amounting to the random number
#'   generated.
#' @param use.distr is a string which chosses the distribution for which the random
#'   data are to be generated; Gaussian and Weibull are supported right now.
#' @return a list holding the observation and forecast random data.
#' @importFrom xts xts
#' @export
PrepareRandomData <- function(num, use.distr) {
  if (use.distr != "Gaussian" & use.distr != "Weibull") {
    CallStop(paste0("I expected either Gaussian or Weibull, but parameter is: ",
                    use.distr))
  }
  if (use.distr == "Gaussian") {
    obs = rnorm(num)
    forec = rnorm(num)
  } else if (use.distr == "Weibull") {
    obs = rweibull(num, shape=2, scale = 6)
    forec = rweibull(num, shape=2, scale = 6)
  }
  date.seq = seq.POSIXt(strptime("20000101", format="%Y%m%d", tz="UTC"),
                        by="hour", length.out=num)

  obs.xts = xts(obs, order.by=date.seq)
  forec.xts = xts(forec, order.by=date.seq)

  return(list(obs=obs.xts, forec=forec.xts))
}

#-----------------------------------------------------------------------------------

IsFinite <- function(val) {
  if (!is.atomic(val)) {
    return(c(round(val$conf.int[1],4),
             round(val$estimate[[1]],4),
             round(val$conf.int[2],4)))
  } else {
    return(c(NA,NA,NA))
  }
}

#-----------------------------------------------------------------------------------

#' @title Fill the correlation list with the correlation values
#' @param t.obj
#' @param corr.lst previous list holding all values so far which is to be appended
#' @param corr10.crea,corr10.eraI,corr10.era20c,corr100.crea,corr100.eraI,corr100.era20c
#'   correlation values of the regional and global reanalyses
#' @return corr.lst appended (updated) list holding all correlation values
GetCorList <- function(t.obj, corr.lst, corr10.crea, corr10.eraI, corr10.era20c,
                       corr100.crea, corr100.eraI, corr100.era20c) {

  # fill out the list corr.lst
  if (t.obj$obs$data$StationName[1] == "Fino1") {
    l.name100 = "Fino1"
    t.height100 = t.obj$obs$data$height[1]
    t.name100 = paste0(t.obj$obs$data$StationName[1], "\nat ", as.character(t.height100))
  }
  if (t.obj$obs$data$StationName[1] == "Fino2") {
    l.name100 = "Fino2"
    t.height100 = t.obj$obs$data$height[1]
    t.name100 = paste0(t.obj$obs$data$StationName[1], "\nat ", as.character(t.height100))
  } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {
    t.height100 = t.obj$obs$data$height[1]
    t.height10 = t.obj$obs6$data$height[1]
    l.name100 = "Lind100"
    l.name10 = "Lind10"
    t.name100 = paste0(t.obj$obs$data$StationName[1], "\nat ", as.character(t.height100))
    t.name10 = paste0(t.obj$obs$data$StationName[1], "\nat ", as.character(t.height10))
  } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
    t.height100 = t.obj$obs2$data$height[1]
    t.height10 = t.obj$obs6$data$height[1]
    t.name100 = paste0(t.obj$obs$data$StationName[1], "\nat ", as.character(t.height100))
    t.name10 = paste0(t.obj$obs$data$StationName[1], "\nat ", as.character(t.height10))
    l.name100 = "Cab100"
    l.name10 = "Cab10"
  }
  corr.lst = append(corr.lst, list(dummy.name = list(name = paste0(as.character(t.name100,
                                                                                "\nat", t.height100)),
                                                     CREA6 = IsFinite(corr100.crea),
                                                     ERAI = IsFinite(corr100.eraI),
                                                     ERA20C = IsFinite(corr100.era20c))))
  names(corr.lst)[which(names(corr.lst)=="dummy.name")] = l.name100
  if (t.obj$obs$data$StationName[1] != "Fino1" &
      t.obj$obs$data$StationName[1] != "Fino2") {
    corr.lst = append(corr.lst, list(dummy.name = list(name = paste0(as.character(t.name10,
                                                                                  "\nat ", t.height10)),
                                                       CREA6 = IsFinite(corr10.crea),
                                                       ERAI = IsFinite(corr10.eraI),
                                                       ERA20C = IsFinite(corr10.era20c))))
    names(corr.lst)[which(names(corr.lst)=="dummy.name")] = l.name10
  }

  return(corr.lst)
}

#-----------------------------------------------------------------------------------

#' @title Calcualte correlation values between reanalyses and tower measurements.
#' @description The function cor.test ist used to calculate correlation values
#'   between reanalyses and tower measurements. Since the reanalyses have a different
#'   temporal resolution, the correlation against the hourly measurements is
#'   calculated at six hourly (ERA-I), three hourly (ERA20C), and hourly (COSMO-REA6)
#'   time steps.
#' @param tower.obj is a ClimObject holding the data of tower measurements and
#'   corresponding reanalysis data.
#' @param ana.time.res named list holding parameters monthly="monthly",
#'   daily="daily", hourly="hourly", and time.res= to determine the time resolution
#'   of the data to be read.
#' @export
GetCorVals <- function(tower.obj, ana.time.res, cor.list, print.cor=TRUE) {

  if (ana.time.res$time.res == ana.time.res$hourly) {
    t.obj = tower.obj$climate_data_objects
    tower.date <- as.POSIXlt(t.obj$herz10$data$date)

    # hourly correlation between C-REA6 and tower at 10m and 100m
    if (length(tower.date) == length(t.obj$obs$data$date)) {
      if (t.obj$obs$data$StationName[1] == "Fino1" |
          t.obj$obs$data$StationName[1] == "Fino2") {
        corr100 = cor.test(t.obj$herz116$data$wind_speed, t.obj$obs$data$wind_speed)
        dummy.lst = GetCorList(t.obj, list(), corr10.crea=NA, corr10.eraI=NA, corr10.era20c=NA,
                               corr100, corr100.eraI=NA, corr100.era20c=NA)
      } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {
        corr10 = cor.test(t.obj$herz10$data$wind_speed, t.obj$obs6$data$wind_speed)
        corr100 = cor.test(t.obj$herz116$data$wind_speed, t.obj$obs$data$wind_speed)
        dummy.lst = GetCorList(t.obj, list(), corr10, corr10.eraI=NA, corr10.era20c=NA,
                               corr100, corr100.eraI=NA, corr100.era20c=NA)
      } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
        corr10 = cor.test(t.obj$herz10$data$wind_speed, t.obj$obs6$data$wind_speed)
        corr100 = cor.test(t.obj$herz116$data$wind_speed, t.obj$obs2$data$wind_speed)
        dummy.lst = GetCorList(t.obj, list(), corr10, corr10.eraI=NA, corr10.era20c=NA,
                               corr100, corr100.eraI=NA, corr100.era20c=NA)
      }
    } else {
      dummy.lst = GetCorList(t.obj, list(), NA, NA, NA, NA, NA, NA)
    }
    lst.hourly = list(Hourly = dummy.lst)

    if (print.cor) {
      cat(paste0("\n   ***   HOURLY   ***\n116m C-REA6 at station: ", t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100$estimate, 4),
                 "\n    conf interv = ", round(corr100$conf.int[[1]], 4), "  ",
                 round(corr100$conf.int[[2]], 4), "\n"))

      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m C-REA6 at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10$estimate, 4),
                   "\n    conf interv = ", round(corr10$conf.int[[1]], 4), "  ",
                   round(corr10$conf.int[[2]], 4), "\n"))
      }
    }

    # three-hourly correlation between COSMO-REA6, ERA20C and tower at 10m and 100m
    if (length(tower.date) == length(t.obj$obs$data$date)) {
      if (t.obj$obs$data$StationName[1] == "Fino1" |
          t.obj$obs$data$StationName[1] == "Fino2") {
        t100.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
      } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {
        t10.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
        t100.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
      } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
        t10.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
        t100.xts = xts(t.obj$obs2$data$wind_speed, order.by=tower.date)
      }
      era20c100.xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
      crea116.xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
      idx = which(is.finite(era20c100.xts))
      corr100.era20c = cor.test(era20c100.xts[idx], t100.xts[idx])
      corr100.crea = cor.test(crea116.xts[idx], t100.xts[idx])

      corr10.era20c = NA
      corr10.crea = NA
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        era20c10.xts = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
        crea10.xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
        idx = which(is.finite(era20c10.xts))
        corr10.era20c = cor.test(era20c10.xts[idx], t10.xts[idx])
        corr10.crea = cor.test(crea10.xts[idx], t10.xts[idx])
      }
      dummy.lst = GetCorList(t.obj, list(), corr10.crea, corr10.eraI=NA,
                             corr10.era20c, corr100.crea, corr100.eraI=NA,
                             corr100.era20c)
    } else {
      dummy.lst = GetCorList(t.obj, list(), corr10.crea=NA, corr10.eraI=NA,
                             corr10.era20c=NA, corr100.crea=NA, corr100.eraI=NA,
                             corr100.era20c=NA)
    }
    lst.threehourly = list(ThreeHourly = dummy.lst)

    if (print.cor) {
      cat(paste0("\n   ***   THREE (3) HOURLY   ***\n100m ERA20C at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.era20c$estimate, 4),
                 "\n    conf interv = ", round(corr100.era20c$conf.int[[1]], 4), "  ",
                 round(corr100.era20c$conf.int[[2]], 4), "\n"))

      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m ERA20C at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.era20c$estimate, 4),
                   "\n    conf interv = ", round(corr10.era20c$conf.int[[1]], 4), "  ",
                   round(corr10.era20c$conf.int[[2]], 4), "\n"))
      }

      cat(paste0("\n   ***   THREE (3) HOURLY   ***\n116m COSMO-REA6 at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.crea$estimate, 4),
                 "\n    conf interv = ", round(corr100.crea$conf.int[[1]], 4), "  ",
                 round(corr100.crea$conf.int[[2]], 4), "\n"))

      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m COSMO-REA6 at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.crea$estimate, 4),
                   "\n    conf interv = ", round(corr10.crea$conf.int[[1]], 4), "  ",
                   round(corr10.crea$conf.int[[2]], 4), "\n"))
      }
    }

    # six-hourly correlation between COSMO-REA6, ERA20C, ERA-I and tower at 10m and 100m
    if (length(tower.date) == length(t.obj$obs$data$date)) {
      if (t.obj$obs$data$StationName[1] == "Fino1" |
          t.obj$obs$data$StationName[1] == "Fino2") {
        t100.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
      } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {
        t10.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
        t100.xts = xts(t.obj$obs$data$wind_speed, order.by=tower.date)
      } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
        t10.xts = xts(t.obj$obs6$data$wind_speed, order.by=tower.date)
        t100.xts = xts(t.obj$obs2$data$wind_speed, order.by=tower.date)
      }
      era20c100.xts = xts(t.obj$era20c100$data$wind_speed, order.by=tower.date)
      eraI100.xts = xts(t.obj$eraI100$data$wind_speed, order.by=tower.date)
      crea116.xts = xts(t.obj$herz116$data$wind_speed, order.by=tower.date)
      idx = which(is.finite(eraI100.xts))
      corr100.era20c = cor.test(era20c100.xts[idx], t100.xts[idx])
      corr100.crea = cor.test(crea116.xts[idx], t100.xts[idx])
      corr100.eraI = cor.test(eraI100.xts[idx], t100.xts[idx])

      corr10.era20c = NA
      corr10.crea = NA
      corr10.eraI = NA
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        era20c10.xts = xts(t.obj$era20c10$data$wind_speed, order.by=tower.date)
        eraI10.xts = xts(t.obj$eraI10$data$wind_speed, order.by=tower.date)
        crea10.xts = xts(t.obj$herz10$data$wind_speed, order.by=tower.date)
        idx = which(is.finite(eraI10.xts))
        corr10.era20c = cor.test(era20c10.xts[idx], t10.xts[idx])
        corr10.crea = cor.test(crea10.xts[idx], t10.xts[idx])
        corr10.eraI = cor.test(eraI10.xts[idx], t10.xts[idx])
      }
      dummy.lst = GetCorList(t.obj, list(), corr10.crea, corr10.eraI, corr10.era20c,
                             corr100.crea, corr100.eraI, corr100.era20c)
    } else {
      dummy.lst = GetCorList(t.obj, list(), corr10.crea=NA, corr10.eraI=NA, corr10.era20c=NA,
                             corr100.crea=NA, corr100.eraI=NA, corr100.era20c=NA)
    }
    lst.sixhourly = list(SixHourly = dummy.lst)
    cor.list = c(list(SixHourly = c(cor.list$SixHourly, lst.sixhourly$SixHourly)),
                 list(ThreeHourly = c(cor.list$ThreeHourly, lst.threehourly$ThreeHourly)),
                 list(Hourly = c(cor.list$Hourly, lst.hourly$Hourly)))

    if (print.cor) {
      cat(paste0("\n   ***   SIX (6) HOURLY   ***\n100m ERA20C at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.era20c$estimate, 4),
                 "\n    conf interv = ", round(corr100.era20c$conf.int[[1]], 4), "  ",
                 round(corr100.era20c$conf.int[[2]], 4), "\n"))
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m ERA20C at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.era20c$estimate, 4),
                   "\n    conf interv = ", round(corr10.era20c$conf.int[[1]], 4), "  ",
                   round(corr10.era20c$conf.int[[2]], 4), "\n"))
      }

      cat(paste0("\n   ***   SIX (6) HOURLY   ***\n100m ERA-Interim at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.eraI$estimate, 4),
                 "\n    conf interv = ", round(corr100.eraI$conf.int[[1]], 4), "  ",
                 round(corr100.eraI$conf.int[[2]], 4), "\n"))
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m ERA-Interim at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.eraI$estimate, 4),
                   "\n    conf interv = ", round(corr10.eraI$conf.int[[1]], 4), "  ",
                   round(corr10.eraI$conf.int[[2]], 4), "\n"))
      }

      cat(paste0("\n   ***   SIX (6) HOURLY   ***\n116m COSMO-REA6 at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.crea$estimate, 4),
                 "\n    conf interv = ", round(corr100.crea$conf.int[[1]], 4), "  ",
                 round(corr100.crea$conf.int[[2]], 4), "\n"))
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m COSMO-REA6 at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.crea$estimate, 4),
                   "\n    conf interv = ", round(corr10.crea$conf.int[[1]], 4), "  ",
                   round(corr10.crea$conf.int[[2]], 4), "\n"))
      }
    }

  } else if (ana.time.res$time.res == ana.time.res$daily |
             ana.time.res$time.res == ana.time.res$monthly) {
    t.obj = tower.obj$climate_data_objects
    tower.date <- as.POSIXlt(t.obj$obs$data$date)

    # daily or monthly correlation between C-REA6, ERA-I, ERA20C and tower at 10m and 100m
    if (t.obj$obs$data$StationName[1] == "Fino1" |
        t.obj$obs$data$StationName[1] == "Fino2") {
      corr100.crea = cor.test(t.obj$herz116$data$wind_speed, t.obj$obs$data$wind_speed)
      corr100.eraI = cor.test(t.obj$eraI100$data$wind_speed, t.obj$obs$data$wind_speed)
      corr100.era20c = cor.test(t.obj$era20c100$data$wind_speed, t.obj$obs$data$wind_speed)

      cor.list = GetCorList(t.obj, cor.list, corr10.crea=NULL, corr10.eraI=NULL, corr10.era20c=NULL,
                            corr100.crea, corr100.eraI, corr100.era20c)
    } else if (t.obj$obs$data$StationName[1] == "Lindenberg") {
      corr10.crea = cor.test(t.obj$herz10$data$wind_speed, t.obj$obs6$data$wind_speed)
      corr10.eraI = cor.test(t.obj$eraI10$data$wind_speed, t.obj$obs6$data$wind_speed)
      corr10.era20c = cor.test(t.obj$era20c10$data$wind_speed, t.obj$obs6$data$wind_speed)
      corr100.crea = cor.test(t.obj$herz116$data$wind_speed, t.obj$obs$data$wind_speed)
      corr100.eraI = cor.test(t.obj$eraI100$data$wind_speed, t.obj$obs$data$wind_speed)
      corr100.era20c = cor.test(t.obj$era20c100$data$wind_speed, t.obj$obs$data$wind_speed)

      cor.list = GetCorList(t.obj, cor.list, corr10.crea, corr10.eraI, corr10.era20c,
                            corr100.crea, corr100.eraI, corr100.era20c)
    } else if (t.obj$obs$data$StationName[1] == "Cabauw") {
      corr10.crea = cor.test(t.obj$herz10$data$wind_speed, t.obj$obs6$data$wind_speed)
      corr10.eraI = cor.test(t.obj$eraI10$data$wind_speed, t.obj$obs6$data$wind_speed)
      corr10.era20c = cor.test(t.obj$era20c10$data$wind_speed, t.obj$obs6$data$wind_speed)
      corr100.crea = cor.test(t.obj$herz116$data$wind_speed, t.obj$obs2$data$wind_speed)
      corr100.eraI = cor.test(t.obj$eraI100$data$wind_speed, t.obj$obs2$data$wind_speed)
      corr100.era20c = cor.test(t.obj$era20c100$data$wind_speed, t.obj$obs2$data$wind_speed)

      cor.list = GetCorList(t.obj, cor.list, corr10.crea, corr10.eraI, corr10.era20c,
                            corr100.crea, corr100.eraI, corr100.era20c)
    }

    # print out the values
    if (ana.time.res$time.res == ana.time.res$daily) { time.res = "DAILY" }
    if (ana.time.res$time.res == ana.time.res$monthly) { time.res = "MONTHLY" }
    if (print.cor) {
      cat(paste0("\n   ***   ", time.res,"   ***\n116m C-REA6 at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.crea$estimate, 4),
                 "\n    conf interv = ", round(corr100.crea$conf.int[[1]], 4), "  ",
                 round(corr100.crea$conf.int[[2]], 4), "\n"))
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m C-REA6 at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.crea$estimate, 4),
                   "\n    conf interv = ", round(corr10.crea$conf.int[[1]], 4), "  ",
                   round(corr10.crea$conf.int[[2]], 4), "\n"))
      }

      cat(paste0("\n   ***   ", time.res,"   ***\n100m ERA-Interim at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.eraI$estimate, 4),
                 "\n    conf interv = ", round(corr100.eraI$conf.int[[1]], 4), "  ",
                 round(corr100.eraI$conf.int[[2]], 4), "\n"))
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m ERA-Interim at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.eraI$estimate, 4),
                   "\n    conf interv = ", round(corr10.eraI$conf.int[[1]], 4), "  ",
                   round(corr10.eraI$conf.int[[2]], 4), "\n"))
      }

      cat(paste0("\n   ***   ", time.res,"   ***\n100m ERA20C at station: ",
                 t.obj$obs$data$StationName[1],
                 "\n    correlation = ", round(corr100.era20c$estimate, 4),
                 "\n    conf interv = ", round(corr100.era20c$conf.int[[1]], 4), "  ",
                 round(corr100.era20c$conf.int[[2]], 4), "\n"))
      if (t.obj$obs$data$StationName[1] != "Fino1" &
          t.obj$obs$data$StationName[1] != "Fino2") {
        cat(paste0("\n10m ERA20C at station: ", t.obj$obs$data$StationName[1],
                   "\n    correlation = ", round(corr10.era20c$estimate, 4),
                   "\n    conf interv = ", round(corr10.era20c$conf.int[[1]], 4), "  ",
                   round(corr10.era20c$conf.int[[2]], 4), "\n"))
      }
    }

  } else {
    CallStop(paste0("This time resolution is not yet supported: ", ana.time.res$time.res))
  }
  return(cor.list)
}

#-----------------------------------------------------------------------------------
