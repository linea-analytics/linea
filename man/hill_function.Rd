% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformations.R
\name{hill_function}
\alias{hill_function}
\title{hill_function}
\usage{
hill_function(v, k = 1, m = 5, abs = TRUE)
}
\arguments{
\item{v}{Numeric vector}

\item{k}{Numeric integer or decimal}

\item{m}{Numeric integer or decimal}

\item{abs}{Boolean to determine if diminishing scale \code{m} is a percentage or absolute value}
}
\value{
The transformed vector \code{v}
}
\description{
Hill Function
}
\details{
Applies the Hill Function 1 - (k^m)(k^m + v^m) on the input vector, v
}
\examples{
hill_function(c(1,0,0,0,10,0,0,0,20), k=10)
hill_function(c(1,0,0,0,10,0,0,0,20), k=0.1, abs = FALSE)
hill_function(c(1,0,0,0,10,0,0,0,20), k=10, m = 3)
}
