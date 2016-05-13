Calc.thresholds <- function(data.vals.obj) {

  data.rea6 = data.vals.obj$herz116$data$wind_speed
  if (as.character(data.vals.obj$obs$data$StationName[1]) == "Fino1" |
      as.character(data.vals.obj$obs$data$StationName[1]) == "Fino2" |
      as.character(data.vals.obj$obs$data$StationName[1]) == "Lindenberg") {
    data.meas = data.vals.obj$obs$data$wind_speed
  } else if (as.character(data.vals.obj$obs$data$StationName[1]) == "Cabauw") {
    data.meas = data.vals.obj$obs3$data$wind_speed
  }


  idx.rea6 = which(!is.finite(data.rea6))
  idx.meas = which(!is.finite(data.meas))

  if (length(idx.rea6) > 0) {
    data.meas[idx.rea6] = NA
  }
  if (length(idx.meas) > 0) {
    data.rea6[idx.meas] = NA
  }

  print(data.vals.obj$herz116$data$StationName[1])
  print(ecdf(data.rea6)(1.8))
  print(ecdf(data.meas)(1.8))
  print(ecdf(data.rea6)(11))
  print(ecdf(data.meas)(11))

}
