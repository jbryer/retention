#' Prints summary information aggregated across cohorts.
#' 
#' @export
summary.Retention <- function(object, 
							  retentionMonths = c(15),
							  completionMonths = c(36, 48, 72, 96),
							  ...) {
	dr = object$Summary
	s = data.frame()
	if(is.null(object$grouping)) {
		s = data.frame(Month=numeric, RetentionRate=numeric(), 
					   GraduationRate=numeric(), Enrollments=integer())
		
		for(i in c(retentionMonths, completionMonths)) {
			tmp = dr[which(dr$Month == i),]
			s = rbind(s, data.frame(
				Month=i,
				RetentionRate = sum(tmp$RetentionRate * tmp$Enrollments) / 
					sum(tmp$Enrollments, na.rm=TRUE),
				GraduationRate =  sum(tmp$GraduationRate * tmp$Enrollments) / 
					sum(tmp$Enrollments, na.rm=TRUE),
				Enrollments = sum(tmp$Enrollments, na.rm=TRUE)
			))
		}
	} else {
		s = data.frame(Month=numeric,
					   Group=character(), RetentionRate=numeric(), 
					   GraduationRate=numeric(), Enrollments=integer())
		for(g in unique(object$Summary$Group)) {
			for(i in c(retentionMonths, completionMonths)) {
				tmp = dr[which(dr$Month == i & dr$Group == g),]
				s = rbind(s, data.frame(
					Month = i,
					Group = g,
					RetentionRate = sum(tmp$RetentionRate * tmp$Enrollments, na.rm=TRUE) / 
						sum(tmp$Enrollments, na.rm=TRUE),
					GraduationRate =  sum(tmp$GraduationRate * tmp$Enrollments, na.rm=TRUE) / 
						sum(tmp$Enrollments, na.rm=TRUE),
					Enrollments = sum(tmp$Enrollments, na.rm=TRUE)
				))
			}
		}
	}
	row.names(s) = 1:nrow(s)
	return(s)
}
