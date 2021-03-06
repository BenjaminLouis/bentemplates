% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_functions.R
\name{parse_amount}
\alias{parse_amount}
\title{Transform a number into a string representing the number with two decimals,
a comma delimiter and the euro sign}
\usage{
parse_amount(x)
}
\arguments{
\item{x}{a numeric value}
}
\value{
the numeric value with two decimals, a comma delimiter, and the euro sign
}
\description{
Transform a number into a string representing the number with two decimals,
a comma delimiter and the euro sign
}
\examples{
parse_amount(pi)
}
