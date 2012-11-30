#' Prints LaTeX of a quarterly aggregate.
#' 
#' @export
quarterSummaryLaTeX <- function(quartersum, caption=NULL, label='quartersum', numQuarters=8) {
	#quartersum[is.nan(quartersum[,3]),3] = NA
	fyc = cast(quartersum, Group ~ FY, value=names(quartersum)[3])
	fyn = cast(quartersum, Group ~ FY, value=names(quartersum)[4])
	quartersum = data.frame(Group=fyc[,1])
	for(i in 2:ncol(fyn)) {
		fyc[which(fyn[,i] < 5),i] = NA
		quartersum = cbind(quartersum, fyc[,i], fyn[,i])
		names(quartersum)[ncol(quartersum)-1] = names(fyn)[i]
		names(quartersum)[ncol(quartersum)] = paste(names(fyn)[i], 'n', sep='.')
	}
	for(i in seq(3, ncol(quartersum), by=2)) {
		quartersum[!is.na(quartersum[,i]),i] = paste('(', quartersum[!is.na(quartersum[,i]),i], ')', sep='')
	}
	#quartersum[,1] = as.integer(quartersum[,1])
	quartersum = quartersum[,c(1, ((ncol(quartersum))-(2*numQuarters)+1):ncol(quartersum))]
	
	addtorow <- list()
	addtorow$pos <- list()
	addtorow$pos[[1]] <- c(-1)
	addtorow$command <- c("\\hline ")
	for(i in seq(2, ncol(quartersum), by=2)) {
		addtorow$command[1] = paste(addtorow$command[1], ' & \\multicolumn{2}{c}{', names(quartersum[i]), '}', sep='')
	}
	addtorow$command[1] = paste(addtorow$command[1], ' \\\\ \\hline', sep='')
	align = c('l', 'l', rep(c('r@{}', '>{ \\tiny}l'), (ncol(quartersum) - 1) / 2) )
	x = xtable(quartersum, caption=caption, label=label, align=align)
	print(x, table.placement='h!', include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller')
}


#' Prints LaTeX of fiscal year summary.
#' 
#' @export
fySummaryLaTeX <- function(fysum, caption=NULL, label='fysum') {
	fysum[is.nan(fysum[,3]),3] = NA
	fyc = cast(fysum, FY ~ Group, value=names(fysum)[3])
	fyn = cast(fysum, FY ~ Group, value=names(fysum)[4])
	fysum = data.frame(Group=fyc[,1])
	for(i in 2:ncol(fyn)) {
		fyc[which(fyn[,i] < 5),i] = NA
		fysum = cbind(fysum, fyc[,i], fyn[,i])
		names(fysum)[ncol(fysum)-1] = names(fyn)[i]
		names(fysum)[ncol(fysum)] = paste(names(fyn)[i], 'n', sep='.')
	}
	for(i in seq(3, ncol(fysum), by=2)) {
		fysum[!is.na(fysum[,i]),i] = paste('(', fysum[!is.na(fysum[,i]),i], ')', sep='')
	}
	fysum[,1] = as.integer(fysum[,1])
	
	addtorow <- list()
	addtorow$pos <- list()
	addtorow$pos[[1]] <- c(-1)
	addtorow$command <- c("\\hline ")
	for(i in seq(2, ncol(fysum), by=2)) {
		addtorow$command[1] = paste(addtorow$command[1], ' & \\multicolumn{2}{c}{', names(fysum[i]), '}', sep='')
	}
	addtorow$command[1] = paste(addtorow$command[1], ' \\\\ \\hline', sep='')
	align = c('l', 'l', rep(c('r@{}', '>{ \\tiny}l'), (ncol(fysum) - 1) / 2) )
	x = xtable(fysum, caption=caption, label=label, align=align)
	print(x, table.placement='h!', include.rownames=FALSE, include.colnames=FALSE, add.to.row=addtorow, hline.after=c(nrow(x)), size='smaller')
}
