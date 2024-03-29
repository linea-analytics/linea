% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasonality.R
\name{first_last_dates}
\alias{first_last_dates}
\title{first_last_dates}
\usage{
first_last_dates(date_values, date_type)
}
\arguments{
\item{date_values}{a date-type or numeric vector}

\item{date_type}{The date column type as either of the following strings:'weekly starting','weekly ending','daily'}
}
\value{
list of first and last daily dates
}
\description{
Check if a time-series is uniform
}
\details{
Check if a time-series is uniform, where the step (e.g. days(1),weeks(7)) is consistent
}
