\name{cohortRetention}
\alias{cohortRetention}
\title{Estimates cohort retention.}
\usage{
  cohortRetention(students, graduates,
    studentIdColumn = "CONTACT_ID_SEQ",
    degreeColumn = "DEGREE_CODE",
    persistColumn = "PERSIST_FLAG",
    warehouseDateColumn = "CREATED_DATE",
    gradColumn = "START_DATE", grouping = NULL, ...)
}
\arguments{
  \item{students}{data frame containing all the student
  information.}

  \item{graduates}{data frame containing all graduate
  information.}

  \item{studentIdColumn}{the name of the student ID column
  in the students and graduates data frames.}

  \item{degreeColumn}{the name of the degree column in the
  students and graduates data frame.}

  \item{persistColumn}{the name of the column representing
  whether a student is persisting in the students data
  frame. Must of logical type.}

  \item{warehouseDateColumn}{the name of the column
  representing the date the record was recorded. This is
  the basis for defining cohorts.}

  \item{gradColumn}{the name of the graduation date column
  in the data frame.}

  \item{the}{name of the grouping column if rates by a
  group are desired.}

  \item{...}{other unspecified parameters.}
}
\description{
  Estimates cohort retention.
}

