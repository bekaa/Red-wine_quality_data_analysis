wine <- read.csv('./data/wineQualityReds.csv')

#quality and alcohol
m = lm(formula =  quality ~ alcohol ,data = wine)
quality <- ggplot( aes(x = alcohol, y = quality ),
						data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_x_continuous( limits = c(8.40,14.9),
											breaks = seq(8,15,.5))+
	scale_y_continuous( limits = c(0,10) ,
											breaks = seq(0,10,1))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )
#==========================================================
#==========================================================
#alcohol from density
m = lm(formula =  alcohol ~ I(density * 1000) ,data = wine)
alcohol <- ggplot( aes(x = density * 1000, y = alcohol ),
						 data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_y_continuous( limits = c(8.40,14.9),
											breaks = seq(8,15,.5))+
	scale_x_continuous( limits = c(990.1,1004.0) ,
											breaks = seq(990,1004,1))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )

#==========================================================
#==========================================================
#density from fixed.acidity
m = lm(formula =  I(density *1000) ~ fixed.acidity ,data = wine)
d1 <- ggplot( aes(x = fixed.acidity, y = density *1000 ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_x_continuous( limits = c(4.60,15.9),
											breaks = seq(4,16,1))+
	scale_y_continuous( limits = c(990.1,1004.0) ,
											breaks = seq(990,1004,2))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )


#==================================================================
# density from chlorides
m = lm(formula =  I(density *1000) ~ chlorides ,data = wine)
d2 <- ggplot( aes(x = chlorides, y = density *1000 ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_x_continuous( limits = c(.01200,.61100),
											breaks = seq(0,.62,.05))+
	scale_y_continuous( limits = c(990.1,1004.0) ,
											breaks = seq(990,1004,2))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )

#==========================================================
#density from residual.sugar
m = lm(formula =  I(density *1000) ~ residual.sugar ,data = wine)
d3 <- ggplot( aes(x = residual.sugar, y = density *1000 ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_x_continuous( limits = c(.900,15.500),
											breaks = seq(0,16,1))+
	scale_y_continuous( limits = c(990.1,1004.0) ,
											breaks = seq(990,1004,2))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )
#==========================================================
#==========================================================
#fixed.acidity from citric.acid
m = lm(formula =   fixed.acidity ~ citric.acid ,data = wine)
f1 <- ggplot( aes(x = citric.acid , y = fixed.acidity ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_y_continuous( limits = c(4.60,15.9),
											breaks = seq(4.5,16,.5))+
	scale_x_continuous( limits = c(.000,1) ,
											breaks = seq(0,1,.1))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )
#==========================================================
#fixed.acidity from pH
m = lm(formula =   fixed.acidity ~ pH ,data = wine)
f2 <- ggplot( aes(x = pH , y = fixed.acidity ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_y_continuous( limits = c(4.60,15.9),
											breaks = seq(4.5,16,.5))+
	scale_x_continuous( limits = c(2.740,4.010) ,
											breaks = seq(2.7,4.010,0.2))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )

#==========================================================
#==========================================================
#citric.acid from volatile.acid
m = lm(formula = citric.acid ~ volatile.acidity ,data = wine)
c1 <- ggplot( aes(x = volatile.acidity , y = citric.acid ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_y_continuous( limits = c(.000,1) ,
											breaks = seq(0,1,.1))+
	scale_x_continuous( limits = c(.12,1.58) ,
											breaks = seq(.1,1.6,.2))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )
#==========================================================
#citric.acid from pH
m = lm(formula = citric.acid ~ pH ,data = wine)
c2 <- ggplot( aes(x = pH , y = citric.acid ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_y_continuous( limits = c(.000,1) ,
											breaks = seq(0,1,.1))+
	scale_x_continuous( limits = c(2.740,4.010) ,
											breaks = seq(2.7,4.010,0.2))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )
#==========================================================
#citric.acid from sulphates
m = lm(formula = citric.acid ~ sulphates ,data = wine)
range(wine$sulphates)
c3 <- ggplot( aes(x = sulphates , y = citric.acid ),
							data = wine )+
	geom_point( position = 'jitter', size  = 1/2)+
	geom_line(  stat = 'summary', color = 'purple')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .1),linetype = 2, color = 'blue')+
	geom_line(stat = 'summary', fun.y = quantile, 
						fun.args = list(probs = .9),linetype = 2, color = 'blue')+
	scale_y_continuous( limits = c(.000,1) ,
											breaks = seq(0,1,.1))+
	scale_x_continuous( limits = c(.33,2.00) ,
											breaks = seq(0,2,.4))+
	geom_abline( slope = coef(m)[2], intercept = coef(m)[1] )
#==========================================================
#==========================================================


#__
grid.arrange(quality,ncol =1, nrow=1)
grid.arrange(alcohol,ncol =1, nrow=1)
grid.arrange(d1,d2,d3,ncol = 1, nrow = 3) 	#density
grid.arrange(f1,f2,ncol = 2) 			          #fixed.acidity
grid.arrange(c1,c2,c3,ncol = 3,nrow = 1)    #citric.acid

#__


