#' Internal method.
#' Estimates cohort retention.
#' 
#' @param students a data frame of enrolled students. This data frame should be
#'        structured like a warehouse data where a student is represented for
#'        each cohort (i.e. warehouseDateColumn) in which they are enrolled.
#' @param graduates a data frame of graduates, or degrees earned.
#' @param studentIdColumn the name of the column corresponding the student's id.
#' @param degreeColumn (optional) the name of the column corresponding to the degree the 
#'        student is enrolled in (within students) or graduated (within graduates).
#'        If omitted, then \code{Transferred} and \code{Graduated Other} levels
#'        in the returned \code{Status} variable will have no values set.
#' @param persistColumn (optional) the name of the column correspondinng to
#'        whether the student is persisting within that month or not. If omitted,
#'        the \code{Persisting} column will not be present in the returned
#'        data frame. The column must be \code{logical}.
#' @param warehouseDateColumn the name of the column corresponding to the warehouse
#'        date, or cohort. Each unique date will thereby define a cohort.
#' @param gradColumn the name of the column in the \code{graduates} data frame
#'        corresponding to the graduation date.
#' @param min.cell.size the minimum cell size to return a rate.
cohortDetails <- function(students, graduates, 
						  studentIdColumn='CONTACT_ID_SEQ',
						  degreeColumn='DEGREE_CODE',
						  persistColumn='PERSIST_FLAG',
						  warehouseDateColumn='CREATED_DATE', 
						  gradColumn='START_DATE',
						  min.cell.size=5) {
	#TODO: If gradColumn and warehouseDateColumn are the same there will be a problem
	# with the merge below (i.e. merge will append .x and .y to the names). A simple
	# fix could be to rename the columns here before having to do the merge.
	students = students[order(students[,warehouseDateColumn], na.last=FALSE),]
	graduates = graduates[order(graduates[,gradColumn], na.last=FALSE),]
	#keep only the first degree
	graduates = graduates[!duplicated(graduates[,studentIdColumn]),] 
	
	firstWHDate = min(students[,warehouseDateColumn], na.rm=TRUE)
	lastWHDate = max(students[,warehouseDateColumn], na.rm=TRUE)
	last = students[which(students[,warehouseDateColumn] == lastWHDate),]
	
	if( !(is.null(persistColumn)) ) {
		students[,persistColumn] = NULL
	}
	
	graduates = graduates[which(as.Date(graduates[,gradColumn]) < lastWHDate),]
	
	#This will leave only the first occurance of a student in a particular degree code
	if(is.null(degreeColumn)) {
		students = students[!duplicated(students[,studentIdColumn]),]
	} else {
		students = students[!duplicated(students[,c(studentIdColumn, degreeColumn)]),]
		#By finding duplicates starting at the end, the first occurance will have the
		#Transferred value set to true
		students$Transferred = FALSE
		rows = duplicated(students[,studentIdColumn], fromLast=TRUE)
		if(nrow(students[rows,]) > 0 ) { 
			students[rows,]$Transferred = TRUE
			#We can now remove duplciated students, the remaining data frame contains 
			#the students' first enrollment
			students = students[!duplicated(students[,studentIdColumn], fromLast=FALSE),]
		}
	}
	#Merge in graduates
	students = merge(students, graduates[,c(studentIdColumn, gradColumn)], 
					 by=c(studentIdColumn), all.x=TRUE)
	#If the START_DATE is not NA, then the student graduated. Note that this does
	#not mean they graduated from their initial DEGREE_CODE (see merge above
	#matches only on CONTACT_ID_SEQ)
	students$Graduated = FALSE
	gr = !is.na(students[,gradColumn]) & !is.na(students[,warehouseDateColumn]) & 
					students[,gradColumn] >= students[,warehouseDateColumn]
	gr[is.na(gr)] = FALSE #TODO: How are the NAs?s
	if(nrow(students[gr,]) > 0) {
		students[gr,]$Graduated = TRUE
	}
	#See if the student is still enrolled as of the last cohort
	last$StillEnrolled = TRUE
	lastCols <- c(studentIdColumn, 'StillEnrolled')
	if( !(is.null(persistColumn)) ) {
		lastCols <- c(lastCols, persistColumn)
	}
	students = merge(students, last[,lastCols], by=c(studentIdColumn), all.x=TRUE)
	se = is.na(students$StillEnrolled)
	if(nrow(students[se,]) > 0) {
		students[se,]$StillEnrolled = FALSE
	}
	#Remove the baseline data
	students = students[!is.na(students[,warehouseDateColumn]),]
	#Remove the first and last cohort month
	if(length(which(students[,warehouseDateColumn] == firstWHDate)) > 0) {
		students = students[-which(students[,warehouseDateColumn] == firstWHDate),]    	
	}
	if(length(which(students[,warehouseDateColumn] == lastWHDate)) > 0) {
		students = students[-which(students[,warehouseDateColumn] == lastWHDate),]
	}
	if(nrow(students) < min.cell.size) {
		return(NULL)
	}
	#Create the status variable that we will plot with
	students$Status = factor('Withdrawn', levels=c('Graduated', 'Graduated Other', 
									'Still Enrolled', 'Transferred', 'Withdrawn' ))
	if(length(which(students$StillEnrolled) == TRUE) > 0) {
		students[students$StillEnrolled,]$Status = 'Still Enrolled'
	}
	if(length(which(students$StillEnrolled == TRUE & 
			students$Transferred == TRUE)) > 0) {
		students[students$StillEnrolled & students$Transferred,]$Status = 'Transferred'
	}
	if(length(which(students$Graduated == TRUE)) > 0) {
		students[students$Graduated,]$Status = 'Graduated'
	}
	if(length(which(students$Transferred == TRUE & students$Graduated == TRUE)) > 0) {
		students[students$Graduated & students$Transferred,]$Status = 'Graduated Other'
	}
	
	#Are those who are still enrolled persisting?
	if( !(is.null(persistColumn)) ) {
		persistData = students[,persistColumn]
		students[,persistColumn] = NULL
		students$Persisting = as.logical(NA)
		persistingYes = which(students$Status %in% c('Still Enrolled', 'Transferred') & 
			persistData)
		if(length(persistingYes) > 0) {
			students[persistingYes,]$Persisting = TRUE
		}
		persistingNo = which(students$Status %in% c('Still Enrolled', 'Transferred') & 
			!persistData)
		if(length(persistingNo) > 0) {
			students[persistingNo,]$Persisting = FALSE
		}
	}
	
	students$Month = as.factor(format(students[,warehouseDateColumn], format='%Y-%m'))
	
	students$Months = diff.month(students[,warehouseDateColumn], lastWHDate)
	
	return(students)
}

#' Estimates cohort retention.
#' 
#' @inheritParams cohortDetails
#' @param grouping the name of the grouping column if rates by a group are desired.
#' @param ... other unspecified parameters.
#' @export
cohortRetention <- function(students, graduates, 
							studentIdColumn='CONTACT_ID_SEQ',
							degreeColumn='DEGREE_CODE',
							persistColumn='PERSIST_FLAG',
							warehouseDateColumn='CREATED_DATE', 
							gradColumn='START_DATE',
							grouping=NULL, ...) {
	cr = list()
	cr$studentIdColumn = studentIdColumn
	cr$degreeColumn = degreeColumn
	cr$persistColumn = persistColumn
	cr$warehouseDateColumn = warehouseDateColumn
	cr$gradColumn = gradColumn
	cr$grouping = grouping
	cr$ComparisonCohort = max(students[,warehouseDateColumn], na.rm=TRUE)
	
	students <- cohortDetails(students, graduates, 
							  studentIdColumn=studentIdColumn,
							  degreeColumn=degreeColumn,
							  persistColumn=persistColumn,
							  warehouseDateColumn=warehouseDateColumn,
							  gradColumn=gradColumn,
							  ...)
	cr$Students = students
	
	buildSummary <- function(s) {
		t = as.data.frame(table(s$Month, s$Status))
		if(!('Var1' %in% names(t) & 'Var2' %in% names(t))) {
			return(NULL)
		}
		
		t = cast(t, Var1 ~ Var2, value='Freq')
		totals = apply(t[,2:ncol(t)], 1, sum)
		t[,2:ncol(t)] = 100 * t[,2:ncol(t)] / totals
		GraduationRate = apply(t[,2:3], 1, sum)
		RetentionRate = apply(t[,2:5], 1, sum)
		t = melt(t, id='Var1')
		rownames(t) = 1:nrow(t)
		
		if(!is.null(persistColumn)) {
			if(all(is.na(s$Persisting))) {
				PersistenceRate <- NA
			} else {
				t2 = as.data.frame(table(s$Month, s$Persisting))
				if(!('Var1' %in% names(t2) & 'Var2' %in% names(t2))) {
					return(NULL)
				}
				t2 = cast(t2, Var1 ~ Var2, value='Freq')
				if(!'TRUE' %in% names(t2)) {
					t2[,'TRUE'] = 0
				}
				if(!'FALSE' %in% names(t2)) {
					t2[,'FALSE'] = 0
				}
				totals2 = apply(t2[,2:3], 1, sum)
				t2[,2:3] = 100 * t2[,2:3] / totals2
				PersistenceRate = t2[,'TRUE']
			}
		}
		
		if('Var1' %in% names(t) & 'Var2' %in% names(t)) {
			Summary = cast(t, Var1 ~ Var2, value='value')
			if(!is.null(persistColumn)) {
				Summary = cbind(Summary, GraduationRate, RetentionRate, 
								PersistenceRate, totals)
			} else {
				Summary = cbind(Summary, GraduationRate, RetentionRate, totals)
			}
			names(Summary)[1] = 'Cohort'
			names(Summary)[ncol(Summary)] = 'Enrollments'
			
			return(Summary)
		} else {
			return(NULL)
		}
	}
	
	results = data.frame()
	
	if(is.null(grouping)) {
		results = buildSummary(students)
	} else {
		groups = unique(students[,grouping])
		for(g in groups) {
			s = students[which(students[,grouping] == g),]
			if(nrow(s) > 20) {
				r = buildSummary(s)
				if(!is.null(r)) {
					r$Group = g
					results = rbind(results, r)
				}
			}
		}
	}
	
	results$Month = diff.month(as.Date(paste(results$Cohort, '-01', sep='')), 
						  as.Date(cr$ComparisonCohort))
	cr$Summary = results
	class(cr) = "CohortRetention"
	
	return(cr)
}

#' Prints the summary portion of the results
#'
#' @param x object with class type CohortRetention.
#' @param ... unused parameters.
#' @export
print.CohortRetention <- function(x, ...) {
	return(x$Summary)
}
