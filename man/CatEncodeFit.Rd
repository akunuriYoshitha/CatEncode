% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CatEncodeFit.R
\name{CatEncodeFit}
\alias{CatEncodeFit}
\title{A fit function to detect categorical data and encode the data using the pre-defined statistics for different encoding techniques}
\usage{
CatEncodeFit(data, dv, fit)
}
\arguments{
\item{data}{Any dataset in which the categorical data needs to be detected and encoded}

\item{dv}{Dependent variable in the given dataset}

\item{fit}{A list returned from "BestCatEncode" that is used to fit the test data.}
}
\value{
Returns the encoded dataset
}
\description{
Detects and treats categorical data by taking in a data set and fit values of the train data.
}
