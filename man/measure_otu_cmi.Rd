% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/measure_otu_cmi.R
\name{measure_otu_cmi}
\alias{measure_otu_cmi}
\title{Measure the conditional mutual information in the OTU table}
\usage{
measure_otu_cmi(data, labels, discrete = FALSE, disc = "equalwidth",
  nbins = 25, method = "emp")
}
\arguments{
\item{data}{matrix with the OTU table}

\item{labels}{class labels for each of the samples in the OTU table}

\item{discrete}{discretize the data. default is FALSE}

\item{disc}{method of performing discretization. available: equalfreq,
equalwidth, globalequalwidth. the default is equalwidth}

\item{nbins}{number of bins to discretize into}

\item{method}{method of performing mutual information calculation.}
}
\value{
returns a vector with the mutual information
}
\description{
This function measures the conditional mutual information shared
between the OTU variables and the class labels that have been
extracted from the map file describing the metadata.
}
\author{
Gregory Ditzler
}

