#' @title Check for existence of file.
#' @description \code{CheckFile} checks for the existence of the file(s) passed to
#' it. If one file doese not exist it stops with an error message.
#' @param infile Character string or concatenated character strings holding the file
#' name(s).
CheckFile <- function(infile) {
  if (any(!file.exists(infile))) {
    idx = which(!file.exists(infile))
    missing.file = infile[idx]
    err = simpleError("Missing File")
    tryCatch(stop(err), finally=print(missing.file))
  }
}
