#' Prints LaTeX of cohort summary.
#' 
#' @seealso xtable
#' @seealso print.xtable
#' @export
xtable.CohortRetention <- function(x, caption=NULL, label=NULL, align=NULL, digits=NULL,
								   display=NULL,	
								   sparkline.retention.rect=NULL, 
								   sparkline.completion.rect=NULL, 
								   sparkline.width=10, 
								   sparkline.height=NULL, 
								   retentionMonths=c(15), 
								   completionMonths=c(36, 48, 72, 96),
								   min.cell.size=20,
								   ...) {
	sparklineTex <- function(df, width=sparkline.width, height=sparkline.height, 
							 rect=NULL, sparkdot.color='red', n=24) {
		if(nrow(df) > n) {
			df = df[(nrow(df)-n):(nrow(df)),]
			df$x = 0:(nrow(df)-1)
		}
		df$x = df$x / max(df$x)
		str = paste(
			ifelse(is.null(height), '', paste('\\renewcommand{\\sparklineheight}{', height, '}', sep='')),
			paste('\\begin{sparkline}{', width, '}', sep=''),
			ifelse(is.null(rect), '', paste('\\sparkrectangle ', rect[1], ' ', rect[2], sep='')),
			paste('\\sparkdot', df[nrow(df),'x'], round(df[nrow(df),'y'], digits=2), sparkdot.color, sep=' '),
			'\\spark ', paste(round(df$x, digits=2), round(df$y, digits=2), collapse=' '), ' /',
			'\\end{sparkline}',
			sep=' ')
		return(str)
	}
	
	retsum = summary(x, months=c(retentionMonths, completionMonths))
	retsum[retsum$Enrollments < min.cell.size, c('RetentionRate', 'GraduationRate')] = NA
	
	n = as.data.frame(cast(retsum, Group ~ Month, value='Enrollments'))
	rr = as.data.frame(cast(retsum, Group ~ Month, value='RetentionRate'))
	cr = as.data.frame(cast(retsum, Group ~ Month, value='GraduationRate'))
	
	tmp = cbind(rr[,as.character(retentionMonths)], cr[,as.character(completionMonths)])
	names(tmp) = paste(c(retentionMonths, completionMonths), '-Months', sep='')
	for(i in 2:ncol(n)) {
		
	}
	
	retsum$RetentionSparkline = as.character(NA)
	retsum$CompletionSparkline = as.character(NA)
	if(!is.null(sparkline.retention.rect)) {
		#sparkline.retention.rect = (sparkline.retention.rect - .50) / 1/(.50)
	}
	if(!is.null(sparkline.completion.rect)) {
		#sparkline.completion.rect = (sparkline.completion.rect - .50) / 1/(.5)
	}
	for(i in retsum[,category]) {
		tmp = ret[which(ret[,'Group'] == i & ret[,'Month'] == 15),]
		coords = data.frame(x=as.integer(tmp$Cohort), y=(tmp$RetentionRate/100))
		coords$x = abs(coords$x - max(coords$x) - 1)
		#coords$y = (coords$y - .5) * 1/.5
		coords = coords[!is.nan(coords$y),]
		coords = coords[!is.na(coords$y),]
		if(nrow(coords) >= min.cell.size & mean(tmp$Enrollments) >= min.cell.size) {
			retsum[which(retsum[,category] == i), 'RetentionSparkline'] = sparklineTex(coords, rect=sparkline.retention.rect)
		}
		
		tmp = ret[which(ret[,'Group'] == i & ret[,'Month'] == completion.month),]
		coords = data.frame(x=as.integer(tmp$Cohort), y=(tmp$GraduationRate/100))
		coords$x = abs(coords$x - max(coords$x) - 1)
		#coords$y = (coords$y - .5) * 1/.5
		if(nrow(coords) >= min.cell.size & mean(tmp$Enrollments) >= min.cell.size) {
			retsum[which(retsum[,category] == i), 'CompletionSparkline'] = sparklineTex(coords, rect=sparkline.completion.rect)
		}
	}
	
	names(retsum) = c(category, 'Rates', 'n', '36-Months', 'n', '48-Months', 'n', '72-Months', 'n', '96-Months', 'n', 'RetentionSparkline', 'CompletionSparkline')
	retsum = retsum[,c(1:3, (ncol(retsum)-1), 4:(ncol(retsum)-2), ncol(retsum))]
	addtorow <- list()
	addtorow$pos <- list()
	addtorow$pos[[1]] <- c(-1)
	addtorow$command <- c(paste('\\hline & & & & \\multicolumn{9}{c}{Completion Rate} \\\\ \\cline{5-13} ',
								category, ' & \\multicolumn{3}{c}{Retention Rate} & \\multicolumn{2}{c}{36-Months} & \\multicolumn{2}{c}{48-Months} & \\multicolumn{2}{c}{72-Months} & \\multicolumn{2}{c}{96-Months} & Past Two Years\\\\ \\hline ', 
								sep=''))
	x = xtable(retsum, caption=caption, label=label, digits=2, align=c('l', category.align, 'r@{}', '>{ \\tiny}l', 'c', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'r@{}', '>{ \\tiny}l', 'c'))
	p = capture.output(print(x, table.placement='h!', include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller'))
	p = p[3:length(p)]
	p = gsub('$\\backslash$', '\\', p, fixed=TRUE)
	p = gsub('\\{', '{', p, fixed=TRUE)
	p = gsub('\\}', '}', p, fixed=TRUE)
	return(p)
}
