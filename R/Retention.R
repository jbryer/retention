#' Returns student status at the specified months
#' 
#' @inheritParams cohortDetails
#' @export
studentDetails <- function(students, grads, 
						   months=c(15,24),
						   warehouseDateColumn='CREATED_DATE',
						   gradColumn='START_DATE',
						   ...) {
	students = students[order(students[,warehouseDateColumn], na.last=FALSE),]
	grads = grads[order(grads[,gradColumn], na.last=FALSE),]
	
	cohorts = unique(students[!is.na(students[,warehouseDateColumn]),warehouseDateColumn])
	
	results <- data.frame()
	for(i in length(cohorts):3) {
		result = cohortDetails(students, grads, 
							   warehouseDateColumn = warehouseDateColumn, 
							   gradColumn = gradColumn, ...)
		if(!is.null(result)) {
			result = result[result$Months %in% months,]
			if(nrow(result) > 0) {
				results = rbind(results, result)
			}
		}
		remove = -which(students[,warehouseDateColumn] == cohorts[i])
		if(length(remove) > 0) {
			students = students[remove,]
		}
	}
	
	return(results)
}

#' Estimates retention.
#' 
#' @inheritParams cohortDetails
#' @param ... other parameters passed to \link{cohortRetention} function.
#' @seealso cohortRetention
#' @export
retention <- function(students, grads, 
					  studentIdColumn='CONTACT_ID_SEQ',
					  degreeColumn='DEGREE_CODE',
					  persistColumn='PERSIST_FLAG',
					  warehouseDateColumn='CREATED_DATE', 
					  gradColumn='START_DATE',
					  grouping=NULL, ...) {
	students = students[order(students[,warehouseDateColumn], na.last=FALSE),]
	grads = grads[order(grads[,gradColumn], na.last=FALSE),]
	
	cohorts = unique(students[!is.na(students[,warehouseDateColumn]),warehouseDateColumn])
    cohorts = cohorts[order(cohorts, decreasing=FALSE)]
	
    results <- list()
    for(i in length(cohorts):3) {
    	tryCatch( {
	        result <- cohortRetention(students, grads, 
	        						  studentIdColumn=studentIdColumn,
	        						  degreeColumn=degreeColumn,
	        						  persistColumn=persistColumn,
	        						  gradColumn=gradColumn,
	        						  warehouseDateColumn=warehouseDateColumn, 
	        						  grouping=grouping, ...)
	        if(!is.null(result$Summary)) {
	            results[[as.character(cohorts[i])]] = result$Summary
	        }
	        },
    		error = function(e) { 
    			warning(paste('Error calculating cohort retention with ',
    						   cohorts[i], ' reference cohort. Results will not ',
    						  'be included. Try the following to debug further:\n',
    						  "students <- students[students$", warehouseDateColumn, 
    						      " <= as.Date('", cohorts[i], "'),]\n",
    						  "cohortRetention(students, grads, warehouseDateColumn='", 
    						      warehouseDateColumn, "', gradColumn='", gradColumn, 
    						      "', grouping='", grouping, "')\n", 
    						  'Calling error: ', as.character(e),
    						  sep=''))
    		},
    		finally = {}
    	)
	        
        remove = -which(students[,warehouseDateColumn] == cohorts[i])
        if(length(remove) > 0) {
            students = students[remove,]
        }
    }
    
    if(length(results) == 0) {
        return(NULL)
    }
    
	cols = c('Cohort', 'GraduationRate', 'RetentionRate', 'Enrollments', 
			 'Graduated','Graduated Other','Still Enrolled','Transferred')
	if(!is.null(persistColumn)) {
		cols = c(cols, 'PersistenceRate')
	}
	if('Group' %in% names(results[[1]])) {
		cols = c(cols, 'Group')
	}
	
	long2 = results[[1]][,cols]
	long2$Month = diff.month(as.Date(paste(long2$Cohort, '01', sep='-')), 
						 as.Date(paste(names(results)[1], '01', sep='-')) )
	long2$Comparison = names(results)[1]
    for(i in 2:length(results)) {
    	if(nrow(results[[i]]) > 0 & ncol(results[[i]]) > 0) {
	        l = results[[i]][,cols]
			l$Month = diff.month(as.Date(paste(l$Cohort, '01', sep='-')), 
							 as.Date(paste(names(results)[i], '01', sep='-')) )
			l$Comparison = names(results)[i]
	    	long2 = rbind(long2, l)
    	}
    }
    
    ret = list()
    ret[['Summary']] = long2
    ret[['Students']] = students
    ret[['Graduates']] = grads
    ret[['WarehouseDateColumn']] = warehouseDateColumn
    ret[['Cohorts']] = results
	ret[['grouping']] = grouping
	
    class(ret) = 'Retention'
    return(ret)
}
