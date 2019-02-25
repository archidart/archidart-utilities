\name{correctFirstOrderRoots}
\alias{correctFirstOrderRoots}
\title{Correcting first order roots}
\description{This function updates a data frame returned by \code{\link{architect}} by providing additional information for the primary root and the basal roots. Particularly useful for fibrous root systems.}
\usage{
correctFirstOrderRoots(x, y)}
\arguments{
  \item{x}{A rsmlToTable object.}
  \item{y}{A data frame returned by \code{\link{architect}}.}}
\value{
Returns an updated data frame.}
\author{Benjamin M. Delory}
\seealso{\code{\link{rsmlToTable}}, \code{\link{architect}}.}