% Generated by roxygen2 (4.0.2): do not edit by hand
\name{PDFscore}
\alias{PDFscore}
\title{A PDF based skill score.}
\usage{
PDFscore(xts1, xts2)
}
\arguments{
\item{xts1,xts2}{are the two time series from which a PDF is determined and
the PDF skill score calcualted}
}
\value{
pdfscore is the PDF skill score
}
\description{
This skill score is descripted in the paper by Mayer et al., 2015,
  Identifying added value in high-resolution climate simulations over Scandinavia,
  Tellus. It compares the PDFs of two populations, in this case here that of two
  reanalysis data products, or one reanalyis and one measurement data product.
}

