% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/charting.R
\name{total_decomp_chart}
\alias{total_decomp_chart}
\title{total_decomp_chart}
\usage{
total_decomp_chart(
  model = NULL,
  decomp_list = NULL,
  pool = NULL,
  colors = color_palette() \%>\% unlist() \%>\% as.character(),
  variable_decomp = FALSE,
  verbose = FALSE,
  plot_bgcolor = "rgba(0, 0, 0, 0)",
  paper_bgcolor = "rgba(0, 0, 0, 0)",
  font_color = "#1c0022",
  zero_line_color = "#1c0022"
)
}
\arguments{
\item{model}{Model object}

\item{decomp_list}{list object generated by the \code{decomping} function.}

\item{pool}{string specifying a group within the pool column to be filtered}

\item{colors}{character vector of colors in hexadecimal notation for bars}

\item{variable_decomp}{boolean specifying whether the chart should be based on the variable_decomp or the category_decomp from the \code{decomping} function.}

\item{verbose}{A boolean to specify whether to print warnings}

\item{plot_bgcolor}{string representing chart color of plot_bgcolor}

\item{paper_bgcolor}{string representing chart color of paper_bgcolor}

\item{font_color}{string representing chart color of font}

\item{zero_line_color}{string representing chart color zerolinecolor}
}
\value{
a \code{plotly} stacked bar chart of the model's decomposition
}
\description{
Total Decomposition Stacked Chart
}
\details{
Plot the total variable, or category, decomposition as a stacked bar chart.
}
