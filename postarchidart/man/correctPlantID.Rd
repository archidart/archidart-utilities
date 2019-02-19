\name{correctPlantID}
\alias{correctPlantID}
\title{Correcting plant identification numbers}
\description{This function corrects the plant identification numbers in a rsmlToTable object. It needs to be used when the following criteria are met: (1) several root systems are present in one image, and (2) the RSML file associated with this image does not distinguish the plants (i.e., 1 plant tag in the RSML file). This situation can happen if several root systems present in one image are traced with SmartRoot for instance.}
\usage{
correctPlantID(x)}
\arguments{
  \item{x}{A rsmlToTable object. Mandatory.}}
\value{
Returns a data frame (rsmlToTable object) with updated plant identification numbers.}
\author{Benjamin M. Delory}
\seealso{\code{\link{rsmlToTable}}.}