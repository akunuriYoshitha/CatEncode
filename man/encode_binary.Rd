% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CatEncodePrerequisites.R
\name{encode_binary}
\alias{encode_binary}
\title{Binary Encoding - Supporting Function}
\usage{
encode_binary(x, colname, returnFit)
}
\arguments{
\item{x}{A categorical field that needs to be Binary encoded}

\item{colname}{A string with field name of categorical field}

\item{returnFit}{A boolean indicating if the result should be a fit or the encoded values}
}
\value{
Returns encoded data when returnFit parameter is FALSE and fit file to fit the test data otherwise
}
\description{
Takes in a vector and encodes using Integer Encoding and then passes the result to Binery Encoding
}
