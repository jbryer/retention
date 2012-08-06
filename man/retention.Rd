\name{retention}
\alias{retention}
\title{Estimates retention.}
\usage{
  retention(students, grads,
    warehouseDateColumn = "CREATED_DATE",
    gradColumn = "START_DATE", grouping = NULL, ...)
}
\arguments{
  \item{students}{data frame containing all the student
  information.}

  \item{graduates}{data frame containing all graduate
  information.}

  \item{warehouseDateColumn}{the name of the column
  representing the date the record was recorded. This is
  the basis for defining cohorts.}

  \item{the}{name of the grouping column if rates by a
  group are desired.}

  \item{...}{other parameters passed to
  \link{cohortRetention} function.}
}
\description{
  Estimates retention.
}
\seealso{
  cohortRetention
}

