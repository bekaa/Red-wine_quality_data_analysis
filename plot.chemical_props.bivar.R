
#this function takes a vector and then remove the outlier points.
remove_outliers <- function(x, na.rm = TRUE) {
	qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
	H <- 1.5 * IQR(x, na.rm = na.rm)
	y <- x
	y[x < (qnt[1] - H)] <- NA
	y[x > (qnt[2] + H)] <- NA
	return(y)
}

plot.chemical_props.bivar <- function(X,Y)
{
	Xcolumn <- remove_outliers(wine[, c(X)])
	Ycolumn <- remove_outliers(x=wine[, c(Y)])
	Xlimits <- range(Xcolumn,na.rm = TRUE)
	Xbreaks <- c( seq(Xlimits[1],Xlimits[2], ((Xlimits[2]-Xlimits[1])/10.0)))
	Ylimits <- range(Ycolumn,na.rm = TRUE)
	Ybreaks <- c( seq(Ylimits[1],Ylimits[2], ((Ylimits[2]-Ylimits[1])/10.0)))
	
	m = lm( formula = Ycolumn ~ Xcolumn )
	
	plott <- ggplot( aes(x = Xcolumn, y = Ycolumn ), data = wine )+
		geom_point( position = 'jitter', size  = 1,alpha = .2, color = 'orange')+
		geom_line(  stat = 'summary',fun.y = mean, color = 'red',
								alpha = .8, linetype = 1)+
		geom_line(stat = 'summary', fun.y = quantile, alpha = .3,
							fun.args = list(probs = .1),linetype = 1, color = 'blue')+
		geom_line(stat = 'summary', fun.y = quantile, alpha = .3, 
							fun.args = list(probs = .9),linetype = 1, color = 'blue')+
		geom_abline( slope = coef(m)[2], intercept = coef(m)[1],
								 alpha = 1,linetype = 4 )+
		scale_x_continuous( limits = Xlimits,
												breaks = Xbreaks)+
		scale_y_continuous( limits = Ylimits ,
												breaks = Ybreaks)+
		xlab(X)+
		ylab(Y)

		#plott	
	return(plott)
	}


#quality from alcohol
quality <- plot.chemical_props.bivar( 'alcohol', 'quality' )

#alcohol from density
alcohol <- plot.chemical_props.bivar( 'density', 'alcohol' )

#density from fixed.acidity
d1 <- plot.chemical_props.bivar( 'fixed.acidity', 'density' )
# density from chlorides
d2 <- plot.chemical_props.bivar( 'chlorides', 'density' )
#density from residual.sugar
d3 <- plot.chemical_props.bivar( 'residual.sugar', 'density' )

#fixed.acidity from citric.acid
f1 <- plot.chemical_props.bivar( 'citric.acid', 'fixed.acidity' )
#fixed.acidity from pH
f2 <- plot.chemical_props.bivar( 'pH', 'fixed.acidity' )

#citric.acid from volatile.acidity
c1 <- plot.chemical_props.bivar( "volatile.acidity", "citric.acid" )
#citric.acid from pH
c2 <- plot.chemical_props.bivar( 'pH', 'citric.acid')
#citric.acid from sulphates
c3 <- plot.chemical_props.bivar( 'sulphates', 'citric.acid')




#grid.arrange(quality,ncol =1, nrow=1)
#grid.arrange(alcohol,ncol =1, nrow=1)
#grid.arrange(d1,d2,d3,ncol = 1, nrow = 3) 	#density
#grid.arrange(f1,f2,ncol = 2) 			          #fixed.acidity
#grid.arrange(c1,c2,c3,ncol = 3,nrow = 1)    #citric.acid





