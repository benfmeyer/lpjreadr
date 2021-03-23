% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seq_date_lpj.R
\name{seq_date_lpj}
\alias{seq_date_lpj}
\title{seq_date_lpj
primarily for internal use in read_lpj but can also be used on its own}
\usage{
seq_date_lpj(time, res)
}
\arguments{
\item{time}{time variable from lpj output file}

\item{res}{temporal resolution of lpj output file}
}
\value{
returns a date vector without leap years
}
\description{
seq_date_lpj
primarily for internal use in read_lpj but can also be used on its own
}