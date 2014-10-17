#' Plots the results of \code{\link{retention}} use \code{ggplot2}.
#' 
#' @param ret the results of \code{\link{retention}}.
#' @param retentionMonths the months to label for the retention rates.
#' @param completionMonths the months to label for completion rates. 
#' @param ... currently unused.
#' @export
plot.Retention <- function(ret, 
						   retentionMonths=c(15), 
						   completionMonths=c(36, 48, 72, 96),
						   ...) {
	long2 = ret$Summary
	
	grt = data.frame()
	rrt = data.frame()
	
	p = ggplot(long2, aes(x=Month, y=GraduationRate), stat='identity')
	
	if(is.null(long2$Group)) {
		wt = cast(long2, Month ~ Cohort, value='Enrollments')
		gr = cast(long2, Month ~ Cohort, value='GraduationRate')
		rr = cast(long2, Month ~ Cohort, value='RetentionRate')
		pr = cast(long2, Month ~ Cohort, value='PersistenceRate')
		grt = data.frame(x=gr$Month, y=apply(wt[,2:ncol(wt)] * gr[,2:ncol(gr)], 
						1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
		rrt = data.frame(x=rr$Month, y=apply(wt[,2:ncol(wt)] * rr[,2:ncol(rr)], 
						1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE))
		p = p + 
			geom_histogram(data=rrt, aes(x=x, y=y), colour='grey', alpha=.2, stat='identity') +
			geom_histogram(data=grt, aes(x=x, y=y), colour='grey', alpha=.2, stat='identity') +
			geom_path(aes(y=RetentionRate, colour=Cohort), alpha=.3) + 
			geom_path(aes(y=GraduationRate, colour=Cohort), linetype=2, alpha=.3) +
			theme(legend.position='none')
	} else {
		for(g in unique(long2$Group)) {
			l = long2[which(long2$Group == g),]
			wt = cast(l, Month ~ Cohort, value='Enrollments')
			gr = cast(l, Month ~ Cohort, value='GraduationRate')
			rr = cast(l, Month ~ Cohort, value='RetentionRate')
			pr = cast(l, Month ~ Cohort, value='PersistenceRate')
			grt = rbind(grt,
						data.frame(x=gr$Month, 
								   y=apply(wt[,2:ncol(wt)] * gr[,2:ncol(gr)], 
						1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE),
								   Group=g) )
			rrt = rbind(rrt,
						data.frame(x=rr$Month, 
								   y=apply(wt[,2:ncol(wt)] * rr[,2:ncol(rr)], 
						1, sum, na.rm=TRUE) / apply(wt[,2:ncol(wt)], 1, sum, na.rm=TRUE),
								   Group=g) )
		}
		grt$Type = 'Completion'
		rrt$Type = 'Retention'
		
		rt = rbind(grt, rrt)
		
		p = p + geom_path(data=rt, aes(x=x, y=y, colour=Group, linetype=Type))
	}
	
	p = p + ylim(c(0,100)) +
		xlab('Months Since Enrollment') + ylab('Percentage')
	
	if(is.null(long2$Group) & !is.null(retentionMonths)) {
		rlabel <- data.frame(
			x=retentionMonths,
			y=rep(100, length(retentionMonths)),
			label=paste(retentionMonths, '-month retention rate: ', 
						format(rrt[retentionMonths,2], digits=3), '%', sep='') )
		p = p + geom_vline(data=rlabel, aes(xintercept=x), colour='black', size=1, alpha=.3) +
			geom_text(data=rlabel, aes(x=x, y=y, label=label), 
					  group=1, size=3, vjust=-.3, hjust=0, angle=-90)
	}
	if(is.null(long2$Group) & !is.null(completionMonths)) {
		clabel <- data.frame(
			x=completionMonths,
			y=rep(100, length(completionMonths)),
			label=paste(completionMonths, '-month completion rate: ', 
						format(grt[completionMonths,2], digits=3), '%', sep='') )
		p = p + geom_vline(data=clabel, aes(xintercept=x), colour='black', size=1, alpha=.3) +
			geom_text(data=clabel, aes(x=x, y=y, label=label), 
					  group=1, size=3, vjust=-.3, hjust=0, angle=-90)
	}
	
	return(p)
}
