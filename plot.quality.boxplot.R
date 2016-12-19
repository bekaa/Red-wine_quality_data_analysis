plot.quality.boxplot <- function(X)
{
	Xcolumn <- wine[, c(X)] 
	limits <- range(Xcolumn)
	breaks <- c( seq(limits[1],limits[2], ((limits[2]-limits[1])/10.0)),
							 as.double(quantile(Xcolumn))[3],
							 as.double(quantile(Xcolumn,probs = .1)),
							 as.double(quantile(Xcolumn,probs = .9)))
	print(
		ggplot(aes(  y = Xcolumn, x = factor(quality) ) 
					 ,data = wine)+
			ylab( X )+
			xlab('quality')+
			ggtitle(X)+
			scale_y_continuous(limits = limits,
												 breaks = breaks)+
			geom_boxplot( width = .5, fill = blues9[4:9])+
			geom_hline(yintercept = quantile(Xcolumn)[3] , linetype = 'dashed',
								 color = 'red',alpha = 0.4)+
			geom_hline( yintercept =  quantile(Xcolumn, probs = .1),
									linetype = 2, color = 'DarkMagenta',alpha = 0.4)+
			geom_hline( yintercept =  quantile(Xcolumn, probs = .9),
									linetype = 2, color = 'DarkMagenta',alpha = 0.4)+
			geom_hline(yintercept = mean(Xcolumn),alpha = 0.4)+
			stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
			stat_summary(fun.y=mean, geom="point")
	)
}