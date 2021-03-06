% Generated by roxygen2 (4.0.2): do not edit by hand
\name{qqPlot}
\alias{qqPlot}
\title{Create QQ plot of two data samples}
\usage{
qqPlot(X, Y, yliml, ylimh, titname, xlabname, ylabname, text.str = NULL,
  xaxis = "s", yaxis = "s")
}
\arguments{
\item{X}{first data samples for the QQ plot}

\item{Y}{second data sample}

\item{yliml}{numeric low value of the y-limits of the plot}

\item{ylimh}{numeric high value of the y-limits}

\item{titname}{string of the title name}

\item{xlabname}{string for the name of the x-axis label}

\item{ylabname}{string for the name of the y-axis label}

\item{text.str}{named variable of type string which will be written into the
plot with \code{text}.}

\item{xaxis,yaxis}{are two named variables which set whether to plot the x- and
y-axis (xaxis='s', yaxis='s') or omit either one (either set to 'n'). It not
set the default value will be used which is set to 's' meaning that the axis
will be plotted.}
}
\description{
Create a simple Quantile-Quantile plot of two data samples.
}

