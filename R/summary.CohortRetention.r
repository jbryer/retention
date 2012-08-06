#' 
#' @export
summary.CohortRetention <- function(object, 
									months=c(15, 36, 48, 72, 96), 
									...) {
	dr = object$Summary
	s = data.frame()
	if(is.null(object$grouping)) {
		s = data.frame(Month=numeric, Cohort=character(), RetentionRate=numeric(), 
					   GraduationRate=numeric(), Enrollments=integer())
		for(m in months) {
			s = rbind(s, dr[which(dr$Month == m),names(s)])
		}
	} else {
		s = data.frame(Month=numeric, Cohort=character(), 
					   Group=character(), RetentionRate=numeric(), 
					   GraduationRate=numeric(), Enrollments=integer())
		for(m in months) {
			s = rbind(s, dr[which(dr$Month == m),names(s)])
		}
	}
	row.names(s) = 1:nrow(s)
	return(s)
}
