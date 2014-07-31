#' Alternative (and possibly faster) retention calculation.
#' 
#' @inheritParams retention
#' @inheritParams cohortRetention
#' @export
retention2 <- function(students, graduates, 
					   months=c(15,24,36,48,72,96),
					   warehouseDateColumn='CREATED_DATE',
					   gradColumn='START_DATE', 
					   studentIdColumn='CONTACT_ID_SEQ',
					   degreeColumn='DEGREE_CODE',
					   persistColumn='Persist') {
	# TODO: This might be faster using data.table
	std <- students
	grads <- graduates
	
	std$Cohort <- format(std[,warehouseDateColumn], '%Y-%m')
	grads$MonthGraduated <- format(grads[,gradColumn], '%Y-%m')
	
	std <- std[order(std$Cohort, na.last=FALSE),]
	grads <- grads[order(grads$MonthGraduated, na.last=FALSE),]
	
	firstGrad <- grads[!duplicated(grads[,studentIdColumn]),]
	firstTime <- std[!duplicated(std[,studentIdColumn]),]
	otherTime <- std[duplicated(std[,studentIdColumn]), 
					 c(studentIdColumn,'Cohort', degreeColumn)]
	names(otherTime)[3] <- 'CurrentDegree'
	
	#Drop students before 2002
	preIds <- unique(firstTime[is.na(firstTime$Cohort),studentIdColumn])
	firstTime <- firstTime[-which(firstTime[,studentIdColumn] %in% preIds),]
	otherTime <- otherTime[-which(otherTime[,studentIdColumn] %in% preIds),]
	grads <- grads[-which(grads[,studentIdColumn] %in% preIds),]
	
	firstTime$Cohort <- as.Date(paste0(firstTime$Cohort, '-01'))
	otherTime$Cohort <- as.Date(paste0(otherTime$Cohort, '-01'))
	
	grads <- grads[!duplicated(grads[,studentIdColumn]),]
	grads <- merge(grads, firstTime[,c(studentIdColumn, 'Cohort')],
				   by=studentIdColumn, all.x=TRUE)
	grads <- grads[!is.na(grads$Cohort),]
	grads$MonthGraduated <- as.Date(paste0(grads$MonthGraduated, '-01'))
	grads$MonthsToGrad <- diff.month(grads$MonthGraduated, grads$Cohort)
	
	#TODO: look at student 24601, their 1992 enrollment does not appear in students data frame
	#HACK!!!
	badgrads <- grads[grads$MonthGraduated < grads$Cohort, studentIdColumn]
	if(length(badgrads) > 0) {
		warning(paste0('Removing ', length(badgrads), ' students who have a graduation ',
					   'date before their first available enrollment record.'))
		grads <- grads[-which(grads[,studentIdColumn] %in% badgrads),]
		firstTime <- firstTime[-which(firstTime[,studentIdColumn] %in% badgrads),]
		otherTime <- otherTime[-which(otherTime[,studentIdColumn] %in% badgrads),]
	}
	
	grads <- grads[,c(studentIdColumn, degreeColumn, 'MonthGraduated', 'MonthsToGrad')]
	names(grads)[2] <- 'DegreeAtGraduation'
	
	results <- firstTime
	results$MonthsEnrolled <- diff.month(results$Cohort, max(firstTime$Cohort))
	results <- merge(results, grads, by=studentIdColumn, all.x=TRUE)
	
	results[,persistColumn] <- NULL
	#results[,warehouseDateColumn] <- NULL
	
	ot.merged <- merge(otherTime, 
				firstTime[,c(studentIdColumn,'Cohort',degreeColumn,persistColumn)], 
				by=studentIdColumn, all.x=TRUE)
	ot.merged$MonthDiff <- diff.month(ot.merged$Cohort.x, ot.merged$Cohort.y)
	
	for(m in months) {
		ot <- ot.merged[ot.merged$MonthDiff == m,]
		ot$Transfer <- ot$CurrentDegree != ot[,degreeColumn]
		
		status <- rep('Withdrawn', nrow(results))
		status[which(results[,studentIdColumn] %in% 
					 	ot[!ot$Transfer,studentIdColumn])] <- 'Enrolled'
		status[which(results[,studentIdColumn] %in% 
					 	ot[ot$Transfer,studentIdColumn])] <- 'Transferred'
		status[which(results$MonthsToGrad <= m & 
					 results$DegreeAtGraduation == results[,degreeColumn])] <- 'Graduated'
		status[which(results$MonthsToGrad <= m & 
					 results$DegreeAtGraduation != results[,degreeColumn])] <- 'Graduated Other'
		status[which(results$MonthsEnrolled < m)] <- NA
		
		status <- factor(status, levels=c('Withdrawn','Transferred','Enrolled',
										  'Graduated Other','Graduated'), ordered=TRUE)
		results[,paste0('Status', m)] <- status
	}
	
	class(results) <- c('retention2', 'data.frame')
	return(results)
}

if(FALSE) {
	ret <- retention2(students, graduates)
	head(ret)
	table(ret$Cohort, ret$Status15)
	#15-month retention rate
	prop.table(table(ret$Cohort, as.integer(ret$Status15) > 1), 1) * 100
	#24-month completion rate
	prop.table(table(ret$Cohort, as.integer(ret$Status24) > 3), 1) * 100
	
	ret$FY <- getFY(ret$Cohort)
	table(ret$FY, ret$Status15)
	prop.table(table(ret$FY, as.integer(ret$Status15) > 1), 1) * 100
	
	# By ADN
# 	ret$ADN <- ifelse(ret$DEGREE_CODE %in% c('AAN','ASN'), 'ADN', 'non-ADN')
# 	table(ret$FY, ret$Status15, ret$ADN)
# 	ret$ADN.FY <- paste0(ret$FY, '-', ret$ADN)
# 	tmp <- as.data.frame(prop.table(table(ret$ADN.FY, as.integer(ret$Status15) > 1), 1) * 100, 
# 						 stringsAsFactors=FALSE)
#  	tmp <- tmp[!is.nan(tmp$Freq),]
# 	tmp$FY <- (unlist(strsplit(tmp$Var1, split='-', fixed=TRUE))[seq(1, 2*nrow(tmp), by=2)])
# 	tmp$ADN <- (unlist(strsplit(tmp$Var1, split='-', fixed=TRUE))[seq(2, 2*nrow(tmp), by=2)])
# 	tmp$Var2 <- as.logical(tmp$Var2)
# 	tmp<-tmp[which(tmp$Var2==TRUE),]
#   ggplot(tmp, aes(x=FY, y=Freq, color=ADN, group=ADN)) + geom_line() + ylim(c(0,100))
	
  #age at enrollment
	ret$AgeGroup <- getAgeGroupsEC(ret$DATE_OF_BIRTH, ret$Cohort)
	tmp <- as.data.frame(prop.table(table(
			paste0(ret$FY, '.', ret$AgeGroup, '.', ret$ADN), 
			as.integer(ret$Status15) > 1), 1) * 100,
			stringsAsFactors=FALSE)
	tmp <- tmp[!is.nan(tmp$Freq),]
	tmp <- tmp[tmp$Var2 == 'TRUE',]
	tmp$FY <- unlist(strsplit(tmp$Var1, split='.', fixed=TRUE))[seq(1, 3*nrow(tmp), by=3)]
	tmp$Age <- unlist(strsplit(tmp$Var1, split='.', fixed=TRUE))[seq(2, 3*nrow(tmp), by=3)]
	tmp$ADN <- unlist(strsplit(tmp$Var1, split='.', fixed=TRUE))[seq(3, 3*nrow(tmp), by=3)]
	tmp <- tmp[tmp$Age != 'NA',]
  names(tmp)
  #tmp$FY = as.numeric(tmp$FY)
  #names(tmp) = c("Var1", "Var2", "Retention Rate", "Fiscal Year", "Age", "ADN")
  
	ggplot(tmp, aes(x=FY, y= Freq, color=ADN, group=ADN,)) + 
    geom_path() +	ylim(c(0,100)) + facet_wrap(~ Age, ncol=1) +
    xlab('Fiscal Year') +ylab('Retention Rate') +ggtitle('XYZ')
		View(tmp)
	
  ret$Age <- getAge(ret$DATE_OF_BIRTH, ret$Cohort)
  ret.b <- ret[substr(ret$DEGREE_CODE, 1, 1) == 'B',]
  cor(ret.b$Age, as.integer(ret.b$Status96) > 3, use='pairwise.complete.obs')
	cor(ret.b$Age, as.integer(ret.b$Status96), use='pairwise.complete.obs')
	
	ret.a <- ret[substr(ret$DEGREE_CODE, 1, 1) == 'A',]
	cor(ret.a$Age, as.integer(ret.a$Status48) > 3, use='pairwise.complete.obs')
	cor(ret.a$Age, as.integer(ret.a$Status48), use='pairwise.complete.obs')
}
