% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CatEncodePrerequisites.R
\name{encode_helmert}
\alias{encode_helmert}
\title{Contrast Encoding using Helmert Coding}
\usage{
encode_helmert(x, dv)
}
\arguments{
\item{x}{Any categorical vector which needs to be encoded}

\item{dv}{Dependent variable of the dataset}
}
\value{
Returns a list with encoded data and fit file to fit the test data
}
\description{
Takes in a vector and encodes using Helmert Coding
}
