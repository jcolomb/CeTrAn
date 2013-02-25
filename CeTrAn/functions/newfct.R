ltraj2traj <- function (x) 
{
    if (!inherits(x, "ltraj")) 
        stop("x should be of class \"ltraj\"")
    if (!attr(x, "typeII")) 
        stop("x should be of type II (time recorded")
    id <- factor(unlist(lapply(x, function(y) id <- rep(attr(y, 
        "id"), nrow(y)))))
    burst <- factor(unlist(lapply(x, function(y) id <- rep(attr(y, 
        "burst"), nrow(y)))))
    res <- do.call("rbind", x)
    res <- cbind(id, burst, res)
    class(res) <- c("traj", "data.frame")
    return(res)
}

# loads buridan data from the object created by load.buridan.xml
load.buridan.data.notransform <- function(params,id_prefix=NA,center=FALSE) {

  if (!inherits(params,"bur.xml"))
    stop("params should be of class \"bur.xml\". 
			created by the function load.buridan.xml()")
			
	# read the data files
	data <- read.table(paste(params$DATAPATH,params$DATAFILE,sep=""), header=TRUE)
	
	# TODO: check if the params are the same
	if (center) {
		data$x = data$x-as.numeric(params$ARENA_CENTER_X)
		data$y = data$y-as.numeric(params$ARENA_CENTER_Y)
				}
	
	experiment_time = as.numeric(params$TIMESTAMP)	
	fly_id <- params$FLY
	if (!is.na(id_prefix))
		fly_id <- paste(id_prefix,params$FLY,sep="_")
	
	#create xy coordinates
	res <- as.numeric(params$ARENA_DIAMETER_MM)/(as.numeric(params$ARENA_RADIUS)*2)
	xy <- data.frame(data$x*res,data$y*res)

	
	
	#create time
	time <- ISOdatetime(1970,1,1,0,0,0) + experiment_time + data$time/1000
	if (!is.na(id_prefix))
		data$burst <- sapply(data$burst,function(v) { add.group_id.prefix(v,id_prefix) })
	else
		data$burst <- sapply(data$burst,function(v) { add.zero.prefix(v) })
	new_traj = as.ltraj(xy,time,id=fly_id, burst = data$burst,slsp="missing")
	
	#do a lowpass to 10 Hz
	#new_traj <- set.sampling.rate(new_traj,0.1)

	#take data only if distance >,< tresholds
	#new_traj2 <- mindistkeep3(new_traj, thresholdmin= g_treshold/10, thresholdmax = 35)
	return(new_traj)
	
}

load.buridan.data.fmeta <- function(params,id_prefix=NA,center=FALSE) {

	if (!inherits(params,"bur.xml"))
		stop("params should be of class \"bur.xml\". 
			created by the function load.buridan.xml()")
			
	# read the data files
	data <- read.csv(params$DATAFILE, header=TRUE)
	
	# # TODO: check if the params are the same
	# if (center) {
		# data$x = data$x-as.numeric(params$ARENA_CENTER_X)
		# data$y = data$y-as.numeric(params$ARENA_CENTER_Y)
				# }
	
	experiment_time = as.numeric(params$TIMESTAMP)	
	fly_id <- params$FLY
	 #if (!is.na(id_prefix))
		 #fly_id <- paste(id_prefix,params$FLY,sep="_")
	xy <- data.frame(data$x,data$y)
	

	A= getwd()

setwd(outputpath)
save.image(file= paste(outputfile,"_workspace.rdata"))


setwd(A)
	
	#create time
	time <- ISOdatetime(1970,1,1,0,0,0) + experiment_time + as.numeric(data$date)/1000
	
	if (!is.na(id_prefix))
		{data$burst <- sapply(data$burst,function(v) { add.group_id.prefix(v,id_prefix)})
	}else{
		data$burst <- sapply(data$burst,function(v) { add.zero.prefix(v) })
	}	
	new_traj = as.ltraj(xy,time,id=fly_id, burst = data$burst,slsp="missing")
	
	#do a lowpass to 10 Hz
	new_traj2 <- set.sampling.rate(new_traj,0.1)

	#take data only if distance >,< tresholds
	new_traj2 <- mindistkeep3(new_traj2, thresholdmin= g_treshold/10, thresholdmax = 90)
		
	


	return(new_traj2)

	
}
### old adehabitat functions
gdltraj_old<- function (x, min, max, type = c("POSIXct", "sec", "min", "hour", 
                                "mday", "mon", "year", "wday", "yday")) 
{
  if (!inherits(x, "ltraj")) 
    stop("x should be of class \"ltraj\"")
  if (!attr(x, "typeII")) 
    stop("x should be of type II (time recorded)")
  type <- match.arg(type)
  x_ori=x
  if (type == "POSIXct") {
    x <- lapply(x, function(y) y[(y$date > min) & (y$date < 
      max), ])
  }
  else {
    x <- lapply(x, function(y) {
      da <- as.POSIXlt(y$date)[[type]]
      return(y[(da >= min) & (da < max), ])
    })
  }
  if (all(sapply(x, nrow) == 0)) {
    print("No relocations within the specified interval")
    x <- x_ori
  }else{
  x[sapply(x, nrow) == 0] <- NULL
  }
  class(x) <- c("ltraj", "list")
  attr(x, "typeII") <- TRUE
  attr(x, "regular") <- is.regular(x)
  return(x)
}



as.area <- function (x) 
{
  if (!inherits(x, "data.frame")) 
    stop("x should be of class \"data.frame\"")
  if (ncol(x) != 3) 
    stop("x should have three columns")
  if (!is.factor(x[, 1])) 
    x[, 1] <- factor(x[, 1])
  class(x) <- c("area", "data.frame")
  return(x)
}

plot.ltraj <- function (x, id = unique(unlist(lapply(x, attr, which = "id"))), 
          burst = unlist(lapply(x, attr, which = "burst")), asc = NULL, 
          area = NULL, xlim = NULL, ylim = NULL, colasc = gray((240:1)/256), 
          colpol = "green", addpoints = TRUE, addlines = TRUE, perani = TRUE, 
          final = TRUE, ...) 
{
  polygon <- area
  if (!is.null(area)) {
    if (!inherits(area, "area")) 
      stop("area should be an object of class area")
  }
  if (!inherits(x, "ltraj")) 
    stop("x should be an object of class ltraj")
  x <- x[id = id]
  x <- x[burst = burst]
  typeII <- attr(x, "typeII")
  x <- lapply(x, function(i) {
    jj <- i[!is.na(i$x), ]
    attr(jj, "id") <- attr(i, "id")
    attr(jj, "burst") <- attr(i, "burst")
    return(jj)
  })
  class(x) <- c("ltraj", "list")
  attr(x, "typeII") <- typeII
  attr(x, "regular") <- is.regular(x)
  uu <- lapply(x, function(i) {
    i[, c("x", "y")]
  })
  if (!perani) 
    idc <- "burst"
  else idc <- "id"
  id <- unique(unlist(lapply(x, function(i) {
    attr(i, idc)
  })))
  if (length(id) > 1) 
    opar <- par(mar = c(0.1, 0.1, 2, 0.1), mfrow = n2mfrow(length(id)))
  if (is.null(xlim)) {
    if (perani) {
      idtt <- unique(id(x))
      oo <- lapply(idtt, function(i) unlist(lapply(x[id = i], 
                                                   function(j) j$x)))
      maxxl <- max(unlist(lapply(oo, function(kk) diff(range(kk)))))
      xlim <- lapply(oo, function(ki) c(min(ki), min(ki) + 
        maxxl))
    }
    else {
      maxxl <- max(unlist(lapply(x, function(ki) diff(range(ki$x)))))
      xlim <- lapply(x, function(ki) c(min(ki$x), min(ki$x) + 
        maxxl))
    }
  }
  else {
    xlim <- split(rep(xlim, length(id)), gl(length(id), 2))
  }
  if (is.null(ylim)) {
    if (perani) {
      idtt <- unique(id(x))
      oo <- lapply(idtt, function(i) unlist(lapply(x[id = i], 
                                                   function(j) j$y)))
      maxyl <- max(unlist(lapply(oo, function(kk) diff(range(kk)))))
      ylim <- lapply(oo, function(ki) c(min(ki), min(ki) + 
        maxyl))
    }
    else {
      maxyl <- max(unlist(lapply(x, function(ki) diff(range(ki$y)))))
      ylim <- lapply(x, function(ki) c(min(ki$y), min(ki$y) + 
        maxyl))
    }
  }
  else {
    ylim <- split(rep(ylim, length(id)), gl(length(id), 2))
  }
  names(xlim) <- id
  names(ylim) <- id
  for (i in id) {
    if (!is.null(asc)) {
      if (length(id) == 1) {
        image(asc, col = colasc, xlim = xlim[i][[1]], 
              ylim = ylim[i][[1]], ...)
      }
      else {
        image(asc, col = colasc, xlim = xlim[i][[1]], 
              ylim = ylim[i][[1]], main = i, axes = (length(id) == 
                1), ...)
      }
    }
    else {
      if (length(id) == 1) {
        plot(1, 1, type = "n", asp = 1, xlim = xlim[i][[1]], 
             ylim = ylim[i][[1]], ...)
      }
      else {
        plot(1, 1, type = "n", asp = 1, xlim = xlim[i][[1]], 
             ylim = ylim[i][[1]], axes = (length(id) == 
               1), main = i, ...)
      }
    }
    if (length(id) > 1) 
      box()
    if (!is.null(polygon)) {
      pol <- split(polygon[, 2:3], factor(polygon[, 1]))
      for (j in 1:length(pol)) polygon(pol[[j]], col = colpol)
    }
    if (addlines) {
      if (idc == "burst") {
        lines(x[burst = i][[1]]$x, x[burst = i][[1]]$y)
      }
      else {
        xtmp <- x[id = i]
        for (j in 1:length(xtmp)) {
          lines(xtmp[[j]]$x, xtmp[[j]]$y)
        }
      }
    }
    if (addpoints) {
      if (idc == "burst") {
        points(x[burst = i][[1]]$x, x[burst = i][[1]]$y, 
               pch = 21, col = "black", bg = "white")
      }
      else {
        xtmp <- x[id = i]
        for (j in 1:length(xtmp)) {
          points(xtmp[[j]]$x, xtmp[[j]]$y, pch = 21, 
                 col = "black", bg = "white")
        }
      }
    }
    if (final) {
      if (idc == "burst") {
        points(x[burst = i][[1]]$x[c(1, length(x[burst = i][[1]]$x))], 
               x[burst = i][[1]]$y[c(1, length(x[burst = i][[1]]$y))], 
               pch = c(2, 14), col = c("blue", "red"), cex = 2, 
               lwd = 2)
      }
      else {
        xtmp <- x[id = i]
        for (j in 1:length(xtmp)) {
          points(xtmp[[j]]$x[c(1, length(xtmp[[j]]$x))], 
                 xtmp[[j]]$y[c(1, length(xtmp[[j]]$x))], pch = c(2, 
                                                                 14), col = c("blue", "red"), cex = 2, lwd = 2)
        }
      }
    }
  }
  if (length(id) > 1) 
    par(opar)
}
