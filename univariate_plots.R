get.plot <- function(X)
	{
	Xcolumn <- wine[,c(X)]
	limits <- range( Xcolumn )
	by <- ((limits[2]-limits[1])/5)
	breaks <- seq(limits[1],limits[2], by )
	mytable <- cbind(summary( Xcolumn ))
	plott <- ggplot(aes( x = Xcolumn   ),data = wine)+
		xlab(X)+
		ylab('')+
		geom_density()+
		geom_vline(xintercept = quantile(Xcolumn)[c(2,3,4)] , linetype = 'dashed',
							 color = c('red','blue','red'))+
		scale_x_continuous(limits = limits,
											breaks = breaks)+
		annotation_custom(tableGrob(mytable),
											xmin=limits[2]- by*2,
											xmax=limits[2], 
											ymin=-Inf, 
											ymax=Inf)
	return(plott)

}

cols <- names(wine)[-c(1)]

grid.arrange(get.plot(cols[1]),get.plot(cols[2]),ncol = 2)
grid.arrange(get.plot(cols[3]),get.plot(cols[4]),ncol = 2)
grid.arrange(get.plot(cols[5]),get.plot(cols[6]),ncol = 2)
grid.arrange(get.plot(cols[7]),get.plot(cols[8]),ncol = 2)
grid.arrange(get.plot(cols[9]),get.plot(cols[10]),ncol = 2)
grid.arrange(get.plot(cols[11]),get.plot(cols[12]),ncol = 2)

rm(cols) #no further need.