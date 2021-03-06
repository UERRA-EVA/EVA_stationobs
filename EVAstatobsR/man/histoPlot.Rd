% Generated by roxygen2 (4.0.2): do not edit by hand
\name{histoPlot}
\alias{histoPlot}
\title{Produce one or two overlapping histogram plot(s)}
\usage{
histoPlot(X, Y, breaks, xlims, titname = "", xlabname = "", ylabname = "",
  xaxis = "s", yaxis = "s", axis.cex = 1.2, tit.cex = 1.1,
  addPlot = FALSE)
}
\arguments{
\item{X}{first data sample for the histogram}

\item{Y}{second sample; may be empty, NULL, NA, or anything else if
addPlot is set to FALSE (see below)}

\item{breaks}{as described in the hist documentation}

\item{xlims}{limits of the x-axis in the format c(x,y)}

\item{titname}{string of the title name of the plot}

\item{xlabname}{string for the name of the x-axis label}

\item{ylabname}{string for the name of the y-axis label}

\item{xaxis,yaxis}{are two named variables which set whether to plot the x- and
y-axis (xaxis='s', yaxis='s') or omit either one (either set to 'n'). It not
set the default value will be used which is set to 's' meaning that the axis
will be plotted.}

\item{addPlot}{optional boolean to determine whether to plot one (F) are two (T)
data samples; default is to plot one data sample (addPlot=FALSE)}
}
\description{
Procudes a standard histogram plot of one or two data samples. If
  two data samples are to be plotted overlapping into one plot, then the same
  breaks are used and the y-limits of the plot are determined befor plotting.
  Density, rather than frequency, distribution is plotted.
}

