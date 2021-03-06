% Generated by roxygen2 (4.0.2): do not edit by hand
\name{GetMetadata}
\alias{GetMetadata}
\title{Provide metadata for downloading data from DWD-ftp server.}
\usage{
GetMetadata(time.resol, station.id, date.begin, date.end, path.element)
}
\arguments{
\item{time.resol}{String of station data time resolution, e.g., daily, hourly}

\item{station.id}{string of the station id}

\item{date.begin}{string of the begin date of the station data in the format
"YYYY-MM-DD"}

\item{date.end}{same as above for the end date}

\item{path.element}{string as an additional element of the path to the data}
}
\value{
metadata The metadata saved in a string vector containing the above
  parameters
}
\description{
Load metadata which specifies which data to download from the
  DWD-ftp server.
}

