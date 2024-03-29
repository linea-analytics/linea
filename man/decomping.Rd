% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/charting.R
\name{decomping}
\alias{decomping}
\title{decomping}
\usage{
decomping(
  model = NULL,
  de_normalise = FALSE,
  categories = NULL,
  tail_window = NULL,
  start = NULL,
  end = NULL,
  verbose = FALSE
)
}
\arguments{
\item{model}{Model object}

\item{de_normalise}{A boolean to specify whether to apply the normalisation}

\item{categories}{\code{data.frame} mapping variables to groups}

\item{tail_window}{for time series, length of tail}

\item{start}{start date of decomping period (inclusive)}

\item{end}{end date of decomping period (inclusive)}

\item{verbose}{A boolean to specify whether to print warnings}
}
\value{
a \code{list} of 3 \code{data.frame}'s representing the variable and category decomposition, and the fitted values.
}
\description{
Variable decomposition of linear regression
}
\details{
Calculates the decomposition of the independent variables based on an input model object.
This can be expanded by leveraging id variables (e.g. date) and categories (i.e. groups of variables).
}
\examples{
run_model(data = mtcars,dv = 'mpg',ivs = c('wt','cyl','disp'),decompose=FALSE) \%>\% decomping()
}
