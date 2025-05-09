% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{analyze_hfoPow}
\alias{analyze_hfoPow}
\title{compute HFO analysis}
\usage{
analyze_hfoPow(hfoPow, thresHfo = 0.2, ltHfoThreshold = 1.5)
}
\arguments{
\item{hfoPow}{Matrix of mean HFO power. the row names are the electrode names and the column names are the time points}

\item{thresHfo}{Numeric. Threshold to detect significant Hfo power intensity}

\item{ltHfoThreshold}{Numeric. Minumum sustained HFo to mark electrode as SOZ}
}
\value{
A HFO power object analysis
}
\description{
compute HFO analysis
}
\examples{
data("pt01EcoG")
}
