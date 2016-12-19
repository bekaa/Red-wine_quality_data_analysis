plot.univar.density <- function(X)
	{
	Xcolumn <-wine[,c(X)]
	limits <- range( Xcolumn )
	breaks <- c( as.vector(limits), as.vector(quantile(Xcolumn)),
							 as.double(quantile(Xcolumn,probs = .1)),
							 as.double(quantile(Xcolumn,probs = .9)))
	
	theme <- ttheme_default(
		core=list(bg_params = list( fill = blues9[1:4], col=NA),
							fg_params=list(col = c( 'black','red','blue','black',
																			'red','black','black',
																			'DarkMagenta','DarkMagenta' ) ,
														 fontface=3,cex = 1.5)),
		rowhead=list( bg_params = list(fill = blues9[1:4]),
							fg_params=list(col = c( 'black','red','blue','black'
																				 ,'red','black','black',
																				 'DarkMagenta','DarkMagenta' ) ,
																fontface=3L,cex = 1.5)))
	
	mytable <- round(rbind(cbind(summary( Xcolumn )),
									 cbind(c('std dev.' = sd(Xcolumn),
									 				quantile(Xcolumn, probs = .1),
									 				quantile(Xcolumn, probs = .9) )) ) , 4)
	mytable <- tableGrob(mytable
											 , theme = theme)

	my.plot <- ggplot(aes( x = Xcolumn   ),data = wine )+
		ylab('')+
		ggtitle(X)+
		geom_density()+
		geom_vline(xintercept = quantile(Xcolumn)[c(2,3,4)] , linetype = 'dashed',
							 color = c('red','blue','red'))+
		geom_vline( xintercept =  quantile(Xcolumn, probs = .1),
								linetype = 2, color = 'DarkMagenta')+
		geom_vline( xintercept =  quantile(Xcolumn, probs = .9),
								linetype = 2, color = 'DarkMagenta')+
		geom_vline(xintercept = mean(Xcolumn),alpha = 0.4)+
		#theme(aspect.ratio=4/3)+
		theme(plot.title = element_text(size=22))+
		scale_x_log10(breaks = breaks)
	
	# set X-axis label.
	if( X %in% c('free sulfur dioxide','total sulfur dioxide') ){
		my.plot = my.plot + xlab(paste0( 'log10( ' ,X, ' ) (mg / dm^3)'  ))
	} else if( X == 'density'){
		my.plot = my.plot + xlab(paste0( 'log10( ' ,X, ' ) (mg / cm^3)'  ))
	} else if( X == 'pH'){
		my.plot = my.plot + xlab(paste0( 'log10( ' ,X, ' )'  ))
	} else if( X == 'alcohol'){
		my.plot = my.plot + xlab(paste0( 'log10( ' ,X, ' ) (%)'  ))
	} else if( X == 'quality'){
		my.plot = my.plot + xlab(paste0( 'log10( ' ,X, ' ) (0-10)'  ))
	} else {
		my.plot = my.plot + xlab(paste0( 'log10( ' ,X, ' ) (g / dm^3)'  ))
	}
	lay <- rbind(c(1,1,1,2))
	grid.arrange(ggplotGrob(my.plot),mytable, ncol = 2, layout_matrix = lay)
}

cols <- names(wine)[-c(1)]
for (x in cols) {
	plot.univar.density(x)
  out <- list(6)
  out[1] <- quantile(wine[,x], probs = .1)
  out[2] <- quantile(wine[,x], probs = .9)
  out[3] <- round( (out[[2]] - out[[1]]) /
  	 ( quantile(wine[,x])[5] - quantile(wine[,x])[1] ) * 100 , 1)
  out[4] <- quantile(wine[,x])[2]
  out[5] <- quantile(wine[,x])[4]
  out[6] <- round( (out[[5]] - out[[4]]) /
  								 	( quantile(wine[,x])[5] - quantile(wine[,x])[1] ) * 100 , 1)
  writeLines(paste0(
		"80% of the records lies in between ",out[1]," and ",out[2],
		" which is ",out[3],"% of the graph.\n",
		"50% of the records lies in between ",out[4]," and ",out[5],
		" which is ",out[6],"% of the graph.\n\n"
	))
	out
	typeof(quantile(wine[,cols[1]])[6])
}

rm(cols) #no further need.



