% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{trans_test}
\alias{trans_test}
\title{trans_tester}
\usage{
trans_test(f, p = NULL)
}
\arguments{
\item{f}{the function to test}

\item{p}{an optional \code{list} of parameters and values}
}
\value{
a character vector of messages
}
\description{
Transformation Tester
}
\details{
Tests a mathematical transformation function for errors.
The function must accept an input vector \code{v} as well as, optionally, additional parameters.
}
\examples{
trans_test(log)
}
