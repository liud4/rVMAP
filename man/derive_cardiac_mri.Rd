% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derive_cardiac_mri.R
\name{derive_cardiac_mri}
\alias{derive_cardiac_mri}
\title{Derive, label, and add cardiac MRI variables to the merged data set.}
\usage{
derive_cardiac_mri(data)
}
\arguments{
\item{data}{A data frame containing VMAC variables.}
}
\value{
\code{data} with added cardiac MRI variables.
}
\description{
This function requires the variable `bsa` which is created in \code{derive_med_hx()}.
}
