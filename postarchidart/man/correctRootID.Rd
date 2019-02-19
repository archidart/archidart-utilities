\name{correctRootID}
\alias{correctRootID}
\title{Correcting root identification numbers}
\description{This function corrects the root identification numbers in a data frame returned by \code{\link{root}}. It needs to be used when the following criteria are met: (1) several root systems are present in one image, and (2) the RSML file associated with this image does not distinguish the plants (i.e., 1 plant tag in the RSML file). This situation can happen if several root systems present in one image are traced with SmartRoot for instance.}
\usage{
correctRootID(x)}
\arguments{
  \item{x}{A data frame returned by \code{\link{root}}.}}
\value{
Returns a data frame with updated root identification numbers.}
\author{Benjamin M. Delory}
\seealso{\code{\link{root}}.}