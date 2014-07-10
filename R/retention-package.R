#' Functions to estimate and visualize retention and complete rates.
#'
#' @name retention-package
#' @aliases retention
#' @docType package
#' @title Retention and completion rates
#' @author \email{jason@@bryer.org}
#' @keywords package institutional research retention completion
#' @import reshape ggplot2 tools
NULL

#' Sample student enrollment data. These data do not represent actual data and
#' and should not be used for any inferential analysis.
#' 
#' @name students
#' @docType data
#' @format a data frame 1,555,866 records of 5 variables
#' @source example student enrollment
#' @keywords datasets
NULL

#' Sample graduates data. These data do not represent actual data and
#' and should not be used for any inferential analysis.
#' 
#' @name graduates
#' @docType data
#' @format a data frame 155,984 records of 3 variables
#' @source example graduates data
#' @keywords datasets
NULL

.onAttach <- function(libname, pkgname) {
	pkgEnv = pos.to.env(match('package:retention', search()))
}

#' Calculates the difference in months between the two dates.
#' 
#' @param d1 date one. 
#' @param d2 date two
#' @export diff.month
diff.month <- function(d1, d2) { 
	monnb <- function(d) { 
		lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
		lt$year*12 + lt$mon
	} 
	return(abs(monnb(d2) - monnb(d1)))
}

#' Depcreated function
#' @inheritParams diff.month
mondf <- function(d1, d2) {
	warning("This function is deprecated. Use diff.month instead!!!")
	return(diff.month(d1, d2))
}
