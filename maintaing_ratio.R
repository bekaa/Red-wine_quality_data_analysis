
convert.col <- function(data)
{
	old.min <- range(data)[1]
	old.max <- range(data)[2]
	#print(range(data))
	new.min <- 0
	new.max <- 10
	new.data = vector()
	old.range <- old.max - old.min
	new.range <- new.max - new.min
	for ( x in data)
	{
		new.x = (((x[1] - old.min) * new.range) / old.range) + new.min
		new.data[ length(new.data)+1 ] <- round(new.x,3) 
		}
	return(new.data)
}


wine.ratio <- data.frame(matrix(ncol = 13,nrow = 1599)) 
names(wine.ratio) <- names(wine) 

for (col in names(wine[-c(1,13,14)])) 
	{
	wine.ratio[,col] <- convert.col(wine[,col])
	}

wine.ratio$X <- wine$X
wine.ratio$quality <- wine$quality

