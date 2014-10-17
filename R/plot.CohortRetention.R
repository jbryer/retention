#' Plots the results of \code{\link{cohortRetention}} using \code{ggplot2}.
#' 
#' @param cohortRetention the results from \code{\link{cohortRetention}}.
#' @param labelPoints if TRUE, points will be labeled.
#' @param textsize the size of the text.
#' @param reverse if TRUE, the x-axis is reversed so most recent cohort is on the left.
#' @param title the plot title.
#' @param xlab the x-axis title.
#' @param ylab2 the y-axis title for the histogram of cohort sizes.
#' @param ylab1 the y-axis title for the rates.
#' @param legend.position the placement for the legend.
#' @param legend.justification the legend justification.
#' @param retentionMonths the months to label for the retention rates.
#' @param completionMonths the months to label for completion rates. 
#' @param plot.histogram if TRUE, a histogram will be included representing cohort sizes.
#' @param useGridExtra if TRUE, layout options from the \code{GridExtra} package will be used.
#' @param ... currently unused.
#' @export
plot.CohortRetention <- function(cohortRetention, 
								labelPoints=FALSE, 
								textsize=3, 
								reverse=TRUE, 
								title="Cohort Based Retention", 
								xlab='Cohort', 
								ylab1='Percentage',
								ylab2='Cohort Size',
								legend.position=c(.1,.7), 
								legend.justification='left',
								retentionMonths=c(15),
								completionMonths=c(36, 48, 72, 96), 
								plot.histogram=TRUE, 
								useGridExtra = TRUE,
								...) {
	if(!is.null(cohortRetention$grouping)) {
		tmp1 = cohortRetention$Summary[,c('Month','RetentionRate','Group')]
		tmp2 = cohortRetention$Summary[,c('Month','GraduationRate','Group')]
		names(tmp1)[2] = 'Rate'
		names(tmp2)[2] = 'Rate'
		tmp1$Type = 'Retention'
		tmp2$Type = 'Completion'
		tmp = rbind(tmp1, tmp2)
		p = ggplot(tmp, aes(x=Month, y=Rate, colour=Group, linetype=Type)) + geom_path() 
		return(p)
	}
	
	results = cohortRetention$Summary
	students = cohortRetention$Students
	
	t = melt(results[,1:6], id='Cohort')
	
	lastWHDate = cohortRetention$ComparisonCohort
	students$Month = as.factor(format(students[,cohortRetention$warehouseDateColumn], 
									  format='%Y-%m'))
	
	df = results[,c('Cohort', 'GraduationRate', 'RetentionRate')]
	
	#Bottom part of the plot
	plot1 = ggplot(t, aes(x=Cohort, y=value), stat='identity') + 
		geom_bar(aes(fill=variable), alpha=.5) + 
		theme(axis.text.x=theme_text(angle=-90, size=unit(8,'points'), hjust=0)) + 
		xlab(xlab) + ylab(ylab1) + 
		scale_fill_manual(paste('Status as of', lastWHDate), 
						  values=c('Withdrawn'='pink', 
						  		 'Transferred'='lightgreen', 
						  		 'Still Enrolled'='green', 
						  		 'Graduated Other'='lightblue', 
						  		 'Graduated'='blue')) + 
		theme(legend.key.width=unit(.2,"cm"), 
			 legend.text=theme_text(size=8), 
			 legend.title=theme_text(size=9,hjust=0), 
			 legend.position=legend.position, 
			 legend.justification=legend.justification, 
			 legend.background=theme_rect(colour='white', fill='white')) +
		geom_path(data=df, aes(x=Cohort, y=GraduationRate, group=1), 
				  fill=NULL, label=NULL, colour='blue', stat='identity') + 
		geom_path(data=df, aes(x=Cohort, y=RetentionRate, group=1), 
				  fill=NULL, label=NULL, colour='black', stat='identity')
	if(labelPoints) {
		plot1 = plot1 + geom_text(data=df, aes(x=Cohort, y=GraduationRate, 
							label=round(GraduationRate, digits=0)), 
							hjust=.5, vjust=-1, colour='blue', size=textsize) +
			geom_text(data=df, aes(x=Cohort, y=RetentionRate, 
								   label=round(RetentionRate, digits=0)), 
					  hjust=.5, vjust=2, colour='black', size=textsize)
	}
	
	xaxis = levels(t$Cohort)
	
	if(reverse) { 
		xaxis = rev(levels(t$Cohort))
		plot1 = plot1 + xlim(rev(levels(t$Cohort)))
	}
	
	if(!is.null(retentionMonths)) {
		rlabel <- data.frame(
			x=df[(nrow(df)-retentionMonths),'Cohort'],
			y=rep(100, length(retentionMonths)),
			label=paste(retentionMonths, '-month retention rate: ', 
					format(df[(nrow(df)-retentionMonths),'RetentionRate'], digits=3), 
						'%', sep='')
		)
		rlabel$position=which(xaxis %in% rlabel$x)
		rlabel$x=as.character(rlabel$x)
		plot1 = plot1 + geom_vline(data=rlabel, aes(xintercept=position), 
								   colour='black', size=1, alpha=.3) 
		plot1 = plot1 + geom_text(data=rlabel, aes(x=position, y=y, label=label), 
								  group=1, size=3, vjust=-.3, hjust=0, angle=-90)
	}
	if(!is.null(completionMonths)) {
		clabel <- data.frame(
			x=df[(nrow(df)-completionMonths),'Cohort'],
			y=rep(100, length(completionMonths)),
			label=paste(completionMonths, '-month completion rate: ', 
						format(df[(nrow(df)-completionMonths),'GraduationRate'], digits=3), 
						'%', sep='')
			)
		clabel$position=which(xaxis %in% clabel$x)
		clabel$x = as.character(clabel$x)
		plot1 = plot1 + geom_vline(data=clabel, aes(xintercept=position), 
								   colour='black', size=1, alpha=.3)
		plot1 = plot1 + geom_text(data=clabel, aes(x=position, y=y, label=label), 
								  group=1, size=3, vjust=-.3, hjust=0, angle=-90)
	}
	
	#Top part of the graph (histogram of new enrollments)
	if(plot.histogram) {
		df2 = results[,c('Cohort', 'Enrollments')]
		plot2 = ggplot(df2, aes(x=Cohort, y=Enrollments), stat='identity') + 
			geom_bar(colour='grey', fill='grey', alpha=.7) + 
			#theme(axis.text.x=theme_text(angle=-90, size=unit(8,'points'), hjust=0)) + 
			theme(axis.ticks=theme_blank(), axis.text.x=theme_blank()) + 
			geom_text(aes(label=Enrollments), angle=-90, vjust=.5, hjust=-.1, size=textsize) + 
			ylab(ylab2) + xlab(NULL)
		
		if(reverse) { plot2 = plot2 + xlim(rev(levels(df2$Cohort))) }
	}
	
	plot1 = plot1 + theme(panel.background=theme_blank(), 
				 panel.grid.major=theme_blank(), 
				 panel.border=theme_blank())
	
	if(plot.histogram) {
		if(useGridExtra) {
			grid_layout = grid.layout(nrow=2, ncol=1, 
									  widths=c(5), #TODO: may want these to be parameters
									  heights=c(1,3), respect=TRUE)
			grid.newpage()
			pushViewport( viewport( layout=grid_layout ) )
			retention:::align.plots(grid_layout, 
						list(plot2, 1, 1), 
						list(plot1 + theme(legend.position='none'), 2, 1))
		} else {
			empty <- plyr::empty
			Layout <- grid.layout(nrow = 2, ncol = 1)
			vplayout <- function(...) {
				grid.newpage()
				pushViewport(viewport(layout = Layout))
			}
			subplot <- function(x, y) { viewport(layout.pos.row = x, layout.pos.col = y) }
			mplot <- function(p1, p2) {
				vplayout()
				print(p2 + theme(main=title), vp = subplot(1, 1))
				print(p1, vp = subplot(2, 1))
			}
			
			mplot(plot1 + theme(plot.margin=unit(c(-10,.5,0,.3), "lines")), plot2 + 
				theme(plot.margin=unit(c(0,0,9,0), "lines")))
		}
	} else {
		return(plot1)
	}
}
