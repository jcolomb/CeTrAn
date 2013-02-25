mindistkeep2 <- function(x, thresholdmin)
{
    if (!inherits(x,"ltraj"))
        stop("x should be of class 'ltraj'")
    foo <- function(y) {
        ul <- 1

        if (nrow(y) > 1){
		for (i in 2:nrow(y)) {
			if(y$dist[i-1]<thresholdmin){
				ul[i]= ul[i-1]
				y$dist[i] = sqrt( (y$x[i+1] -y$x [ul[i]])*(y$x[i+1] -y$x [ul[i]])+(y$y[i+1] -y$y [ul[i]])*(y$y[i+1] -y$y [ul[i]]) )
			} else ul[i]={i}
				
			 
		}
	} else print(nrow (y))


        if (!is.null(attr(y, "infolocs"))) {
            infol <- attr(y, "infolocs")
        }
        z <- y[ul,c("x","y")]
        da <- y$date
        if (!is.null(attr(y, "infolocs"))) {
            infol <- attr(y, "infolocs")
            lt <- as.ltraj(z, da, id=attr(y,"id"), burst=attr(y,"burst"),
                           typeII=attr(x,"typeII"), infolocs=infol)
        } else {
            lt <- as.ltraj(z, da, id=attr(y,"id"), burst=attr(y,"burst"),
                           typeII=attr(x,"typeII"))
        }
        return(lt)
    }
    res <- do.call("c.ltraj", lapply(x, foo))
    class(res) <- c("ltraj","list")
    return(res)
}

mindistkeep3 <- function(x, thresholdmin,thresholdmax)
{
    if (!inherits(x,"ltraj"))
        stop("x should be of class 'ltraj'")
    foo <- function(y) {
        ul <- 1

        if (nrow(y) > 1){
		for (i in 2:nrow(y)) {
			if(y$dist[i-1]<thresholdmin || y$dist[i-1]>thresholdmax){
				ul[i]= ul[i-1]
				if(y$dist[i-1]<thresholdmin) {
					y$dist[i] = sqrt( (y$x[i+1] -y$x [ul[i]])*(y$x[i+1] -y$x [ul[i]])+(y$y[i+1] -y$y [ul[i]])*(y$y[i+1] -y$y [ul[i]]) )
					}
			} else ul[i]={i}
				
			 
		}
	} else print(nrow (y))


        if (!is.null(attr(y, "infolocs"))) {
            infol <- attr(y, "infolocs")
        }
        z <- y[ul,c("x","y")]
        da <- y$date
        if (!is.null(attr(y, "infolocs"))) {
            infol <- attr(y, "infolocs")
            lt <- as.ltraj(z, da, id=attr(y,"id"), burst=attr(y,"burst"),
                           typeII=attr(x,"typeII"), infolocs=infol)
        } else {
            lt <- as.ltraj(z, da, id=attr(y,"id"), burst=attr(y,"burst"),
                           typeII=attr(x,"typeII"))
        }
        return(lt)
    }
    res <- do.call("c.ltraj", lapply(x, foo))
    class(res) <- c("ltraj","list")
    return(res)
}

maxdistkeep2 <- function(x, threshold =25)
{
    if (!inherits(x,"ltraj"))
        stop("x should be of class 'ltraj'")
    foo <- function(y) {
        ul <- 1

        if (nrow(y) > 1){
		for (i in 2:nrow(y)) {
			if(y$dist[i-1]>threshold){
				ul[i]= ul[i-1]
				
			} else ul[i]={i}
				
			 
		}
	} else print(nrow (y))


        if (!is.null(attr(y, "infolocs"))) {
            infol <- attr(y, "infolocs")
        }
        z <- y[ul,c("x","y")]
        da <- y$date
        if (!is.null(attr(y, "infolocs"))) {
            infol <- attr(y, "infolocs")
            lt <- as.ltraj(z, da, id=attr(y,"id"), burst=attr(y,"burst"),
                           typeII=attr(x,"typeII"), infolocs=infol)
        } else {
            lt <- as.ltraj(z, da, id=attr(y,"id"), burst=attr(y,"burst"),
                           typeII=attr(x,"typeII"))
        }
        return(lt)
    }
    res <- do.call("c.ltraj", lapply(x, foo))
    class(res) <- c("ltraj","list")
    return(res)
}




# creates a circle center x,y and radius r with n points
circle <- function(x, y, r = 1, n = 100) {
    	ang <- seq(0, 2*pi, length.out=n)
    	xx <- x + r * cos(ang)
    	yy <- y + r * sin(ang)
	return(data.frame(xx,yy))
}

#creates a number string with 0 prefix of length
add.zero.prefix <- function(value,length=5) {
	length <- length-nchar(as.character(value))
	zeros <- paste(rep("0",length),collapse="")
	return( paste(zeros,value,sep="") )
}

#ceates a number string with group and id in the prefix
add.group_id.prefix <- function(value,id,group="",length=3) {
	if (group!="") prefix <- paste(as.character(group),as.character(id),sep="-") else		prefix <- as.character(id)
  
	length <- max (0,length-nchar(as.character(value)))
	zeros <- paste(rep("0",length),collapse="")
	return( paste(prefix,"_",zeros,value,sep="") )
} 

# returns the timestamp in ms precision
as.seconds <- function(date) {
	if (!inherits(date, "POSIXct")) 
        	stop("date should be of class \"POSIXct\"")
	sec = as.numeric(date)%/%1
	r =  as.numeric(format(date, "%OS3"))%%1
	return(data.frame(sec,r))
}

#returns time differenc in seconds
time.difference <- function(date1,date2) {
	if (!inherits(date1, "POSIXct")||!inherits(date2, "POSIXct")) 
        	stop("date1 and date2 should be of class \"POSIXct\"")
	t1 <- as.seconds(date1)
	t2 <- as.seconds(date2)
	t2$sec <- t2$sec - t1$sec
	t2$r <- t2$r - t1$r
	return(t2sec+t2$r)
}

# returns the duration of a date vector to the first element in seconds
get.durations <- function(dates) {
	if (!inherits(dates, "POSIXct"))
		stop("dates should be of class \"POSIXct\"")
	durations = as.seconds(dates)
	durations$s = durations$s - durations$s[1]
	durations$r = durations$r - durations$r[1]
	return(durations$s+durations$r)
}

#returns the angular difference of two angles
angular.diff <- function(a,b) {
	diff <- a-b
	
	erg <- sapply(diff,function(d) {
		
		if (is.na(d))
            return(d)

			d <- d%%(2*pi)			
			if (d > pi)
				return(-(2*pi)+d)
			else if (d < -pi)
				return((2*pi)-d)
			else
				return (d)

	})

	return(erg)
} 

#gets all the burst for the specified group
get.traj.group <- function(traj,id_table,group) {
	
	if (!inherits(traj, "ltraj")) 
        stop("traj should be of class \"ltraj\"")
		
	ids = id_table[id_table$group==group,]$id		
	return(traj[id(traj)%in%ids])
}

 
