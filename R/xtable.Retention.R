#' Prints LaTeX of CohortRetention summary.
#' 
#' @param x an R object of type CohortRetention.
#' @param caption the table caption.
#' @param label the table label.
#' @param align the alignment used for cell placement. Unused for grouped results.
#' @param digits the number of digits to display.
#' @param display currently ignored.
#' @param ... other parameters passed to xtable and print.xtable.
#' @seealso xtable
#' @seealso print.xtable
#' @export
xtable.Retention <- function(x, caption=NULL, label=NULL, align=NULL, digits=NULL,
								   display=NULL, 
								   retentionMonths=c(15), 
							 	   completionMonths=c(36, 48, 72, 96),
								   ...) {
	retsum = summary(x, months=c(retentionMonths, completionMonths))
	retsum$Month = as.integer(retsum$Month)
	
	if(is.null(retsum$Group)) {
		names(retsum) = c('Month', 'Retention Rate', 'Completion Rate', 'n')
		x = xtable(retsum, caption=caption, label=label, digits=digits)
		print(x, include.rownames=FALSE, ...)
	} else {
		r = cast(retsum, Group ~ Month, value='RetentionRate')
		g = cast(retsum, Group ~ Month, value='GraduationRate')
		e = cast(retsum, Group ~ Month, value='Enrollments')
		tab = data.frame(Group=r[,1])
		header = ''
		for(i in retentionMonths) {
			tab = cbind(tab, r[,as.character(i)], 
						e[,as.character(i)])
			names(tab)[(ncol(tab)-1):ncol(tab)] = c(paste(i, '-Months', sep=''), 'n')
			header = paste(header, paste('& \\multicolumn{2}{c}{', i, '-Months}', sep=''))
		}
		for(i in completionMonths) {
			tab = cbind(tab, g[,as.character(i)], e[,as.character(i)])
			names(tab)[(ncol(tab)-1):ncol(tab)] = c(paste(i, '-Months', sep=''), 'n')
			header = paste(header, paste('& \\multicolumn{2}{c}{', i, '-Months}', sep=''))
		}
		addtorow <- list()
		addtorow$pos <- list()
		addtorow$pos[[1]] <- c(-1)
		addtorow$command <- c(paste('\\hline & \\multicolumn{', (length(retentionMonths)*2), 
									'}{c}{Retention Rate} & ',
									'\\multicolumn{', (length(completionMonths)*2), 
									'}{c}{Completion Rate} \\\\ ',
									'\\cline{2-', ncol(tab), '} ',
									header, ' \\\\ ',
									sep=''))
		x = xtable(tab, caption=caption, label=label, digits=digits, 
				   align=c('l', 'l', rep(c('r@{}', '>{ \\tiny}l'), ((ncol(tab)-1)/2)) ),
				   ...)
		print(x, include.rownames=FALSE, 
			  include.colnames=FALSE, add.to.row=addtorow, 
			  hline.after=c(0, nrow(x)), ...)
	}
}
