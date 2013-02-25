# loads the buridans xml file
load.buridan.xml <- function(xmlpath) {
  doc <- xmlInternalTreeParse(xmlpath)
	data <- xmlChildren(xmlChildren(doc)$HEADER)
	values <- sapply(data, xmlValue)
	erg <- lapply(values,unlist)
	
	datapath = strsplit(xmlpath,"/")	
	datapath = paste(c(datapath[[1]][1:length(datapath[[1]])-1],""),collapse="/")
	erg$DATAPATH = datapath	
	
	class(erg) <- "bur.xml"
	return(erg)
}

# loads buridan data from the object created by load.buridan.xml
load.buridan.data <- function(params,id_prefix=NA,center=FALSE) {

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
		{data$burst <- sapply(data$burst,function(v) { add.group_id.prefix(v,id_prefix) })
	}else data$burst <- sapply(data$burst,function(v) { add.zero.prefix(v) })
  
	new_traj = as.ltraj(xy,time,id=fly_id, burst = data$burst,slsp="missing")
	
	#do a lowpass to 10 Hz
	new_traj <- set.sampling.rate(new_traj,0.1)

	#take data only if distance >,< tresholds
	new_traj2 <- mindistkeep3(new_traj, thresholdmin= g_treshold/10, thresholdmax = 70)
		
	


	return(new_traj2)

	
}
# loads buridan data from the object created by load.buridan.xml
load.buridan.data.22 <- function(params,id_prefix=NA,center=TRUE) {

	if (!inherits(params,"bur.xml"))
		stop("params should be of class \"bur.xml\". 
			created by the function load.buridan.xml()")
			
	# read the data files
	data <- read.table(paste(params$DATAPATH,params$DATAFILE,sep=""), header=TRUE)
	
	# TODO: check if the params are the same
	
	experiment_time = as.numeric(params$TIMESTAMP)	
	fly_id <- params$FLY
	if (!is.na(id_prefix))
		fly_id <- paste(id_prefix,params$FLY,sep="_")
	
	#create xy coordinates
	res <- as.numeric(params$ARENA_DIAMETER_MM)/(as.numeric(params$ARENA_RADIUS)*2)
	xy <- data.frame(data$x*res,data$y*res)

	if (center) {
		data$x = data$x-params$CENTER_X
		data$y = data$y-params$CENTER_Y
		
	}
	
	#create time
	time <- ISOdatetime(1970,1,1,0,0,0) + experiment_time + data$time/1000
	if (!is.na(id_prefix))
		data$burst <- sapply(data$burst,function(v) { add.group_id.prefix(v,id_prefix) })
	else
		data$burst <- sapply(data$burst,function(v) { add.zero.prefix(v) })
	new_traj = as.ltraj(xy,time,id=fly_id, burst = data$burst,slsp="missing")
	
	#do a lowpass to 10 Hz
	new_traj2 <- set.sampling.rate(new_traj,0.1)

	#take data only if distance >,< tresholds
	new_traj3 <- mindistkeep3(new_traj2, thresholdmin= g_treshold/10, thresholdmax = 35)
		
	


	return(list(new_traj3, new_traj))

	
}



create.arena.poly <- function(arena,outer_circle=FALSE) {
	
	if (!inherits(arena,"bur.env"))
		stop("Arena should be of class \"bur.env\".")

	# create circles
	poly <- circle(arena$cx,arena$cy,arena$r)
	p_id <- rep(1,100)
	if (outer_circle) {
		poly <- rbind(poly, circle(arena$cx,arena$cy,arena$outer_r))
		p_id <- c(p_id,rep(0,100))
	}
	return(data.frame(p_id,poly))	
}

# creates black stripe polygon
create.stripe.poly <- function(env) {
	pos = env$stripe_pos[[1]]
	w = env$stripe_w
	r = env$r+4
		
	px = matrix(c(env$cx+(cos(pos+w)*r),env$cx+(cos(pos-w))*r),ncol=2)
	py = matrix(c(env$cy+(sin(pos+w)*r),env$cy+(sin(pos-w))*r),ncol=2)

	p_id <- c()
	x <- c()
	y <- c()

	for (i in c(1:length(pos))) {
		p_id <- c(p_id,rep(i-1,2))
		x <- c(x,px[i,])
		y <- c(y,py[i,])
	}
	
	return(data.frame(p_id,x,y))
}

#creates a data frame with all the environment variables
create.env.vars <- function(params,res=0) {
	
	if (res==0)
		res <- as.numeric(params$ARENA_DIAMETER_MM)/(as.numeric(params$ARENA_RADIUS)*2)

	# TODO: resolution
	erg <- data.frame(
		cx = as.numeric(params$ARENA_CENTER_X)*res,
		cy = as.numeric(params$ARENA_CENTER_Y)*res,
		r = as.numeric(params$ARENA_DIAMETER_MM)/2,
		outer_r = as.numeric(params$OUTER_DIAMETER_MM)/2,
		stripe_pos = I(list
			(as.numeric(unlist(strsplit(params$STRIPE_POS,",")))/180*pi
		)),
		stripe_w = as.numeric(params$STRIPE_WIDTH)/180*pi,
		res = res
	)

	class(erg) <- c("bur.env","data.frame")
	return(erg)
}
#creates a data frame with all the environment variables +center
create.env.vars.center <- function(params,res=0) {
	
	if (res==0)
		res <- as.numeric(params$ARENA_DIAMETER_MM)/(as.numeric(params$ARENA_RADIUS)*2)

	# TODO: resolution
	erg <- data.frame(
		cx = 0,
		cy = 0,
		r = as.numeric(params$ARENA_DIAMETER_MM)/2,
		outer_r = as.numeric(params$OUTER_DIAMETER_MM)/2,
		stripe_pos = I(list
			(as.numeric(unlist(strsplit(params$STRIPE_POS,",")))/180*pi
		)),
		stripe_w = as.numeric(params$STRIPE_WIDTH)/180*pi,
		res = res
	)

	class(erg) <- c("bur.env","data.frame")
	return(erg)
}

get5min <- function (ltraj) {
  if (!inherits(ltraj, "ltraj")) 
        stop("ltraj should be of class \"ltraj\"")
   newtraj="a"     
  gotone=FALSE
  for (i in 1:length(ltraj)){
    if (!gotone){
     if (nrow(ltraj[[i]]) > 3000){
          newtraj = c(ltraj[i])
          gotone= TRUE
          
        }
        
    }   
 
  }
  newtraj2=c()
  if (newtraj !="a")
  newtraj2 = gdltraj(newtraj,newtraj[[1]]$date [1],newtraj[[1]]$date [3002], type ="POSIXct") 
  return(newtraj2)
}
get5minR <- function (ltraj) {
  if (!inherits(ltraj, "ltraj")) 
        stop("ltraj should be of class \"ltraj\"")
        
  gotone=FALSE
  for (i in length(ltraj):1){
    if (!gotone){
     if (nrow(ltraj[[i]]) > 3000){
          newtraj = c(ltraj[i])
          gotone= TRUE
          
        }
        
    }   
 
  }
  newtraj2 = gdltraj(newtraj,newtraj[[1]]$date [length(newtraj[[1]]$date)-3001],newtraj[[1]]$date [length(newtraj[[1]]$date)], type ="POSIXct")
  return(newtraj2)
}
# creates a trajectorie with a sampling rate in dt secs out of the given ltraj object, adds interpolated points
#TODO: make this work with bigger dt
set.sampling.rate <- function(ltraj,dt=0.1) {

	if (dt!=0.1)
		stop("currently works only with dt=0.1")

	if (!inherits(ltraj, "ltraj")) 
        stop("ltraj should be of class \"ltraj\"")
	if (!attr(ltraj, "typeII")) 
        stop("ltraj should be of type II (time recorded)")
		
	## for each burst
	for (oo in c(1:length(ltraj))) {

		data = ltraj[[oo]]
		date <- data$date
		x <- data$x
		y <- data$y	

		# get the ref date and duration
		date.ref <- as.seconds(date[1])
		date.ref <- ISOdatetime(1970,1,1,0,0,0) + date.ref$s+date.ref$r
		
		duration <- as.seconds(date[length(date)])-as.seconds(date[1])
		duration <- duration$sec+duration$r
		offset <- as.seconds(date[1])$r

		# create new dates
		first_date <- as.seconds(date[1])
		da <- as.seconds(date)
		da$sec <- da$sec-first_date$sec
		da$r <- da$r - first_date$r
		da <- da$sec + da$r
	
		# creates a sequence for the new dates:
		start <- (offset%/%dt)*dt+dt
		start <- start - offset

		if (start>duration) {
			next()
		}

		time_seq = seq(start,duration,dt)
		ndates <- date.ref + time_seq
		
		#TODO: round to 1/10th second
				
		class(ndates) <- c("POSIXct", "POSIXt")
		
		#interpolates the curve at given positions
		nx <- approx(da,x,method="linear",xout= time_seq,ties="mean")$y
		ny <- approx(da,y,method="linear",xout= time_seq,ties="mean")$y

		if (length(nx)!=length(ny)||length(ndates)!=length(ny))
			stop("Something went wrong!")

		#create id
		id <-  id(ltraj[oo])
		id <- rep(id,length(ndates))
		burst <- burst(ltraj[oo])
		burst <- rep(burst,length(ndates))
		
		#adds the data to the new trajectorie
		if (oo==1) {
			new_x <- nx
			new_y <- ny
			new_date <- ndates	
			new_burst <- burst
			new_id <- id
		} else {
			new_x <- c(new_x,nx)
			new_y <- c(new_y,ny)
			new_date <- c(new_date,ndates)
			new_burst <- c(new_burst,burst)
			new_id <- c(new_id,id)
		}
	}

	# create the new trajectorie
	new_traj = as.ltraj(data.frame(new_x,new_y),new_date,id=new_id, burst = new_burst)

	attr(new_traj, "typeII") <- TRUE
    attr(new_traj, "regular") <- TRUE

	return(new_traj)

}

# computes the deviation of the flies walking direction towards the black stripe direction
c.angle.dev <- function(traj,env) {

	if (!inherits(env,"bur.env"))
		stop("env should be of class \"bur.env\".")

	if (!inherits(traj, "ltraj")) 
        stop("ltraj should be of class \"ltraj\"")
	
	deviation <- c()
	dates <- c()
	
	#calculate the position of the black stripes
	stripe_pos <- env$stripe_pos[[1]]
	stripe_radius <- env$outer_r
	stripes <- matrix(c(cos(stripe_pos)*stripe_radius,sin(stripe_pos)*stripe_radius),ncol=2)

	center <- c(env$cx,env$cy)
	
	for (oo in c(1:length(traj))) {	
	
		data <- traj[[oo]]

		date <- data$date
		angles <- data$abs.angle
		# take only when there is more than one point in the burst
		if (length (angles)>1) {
			points <- matrix(c(data$x,data$y),ncol=2)
		
			# center the points at the arena center
			if (center [1]>0){
			points[,1] <- points[,1] - center[1]
			points[,2] <- points[,2] - center[2]}

				
			#now calculate the vectors angle pointing towards the column	
			v <- apply(points,1,function(p) {
				res <- t(t(stripes)-p)

				return (atan2(res[,2],res[,1]))
			})

		
			#calculate the deviation from the fly direction to the v vectors
			if (class(v)=="numeric") {
				tv <- t(v)
			} else {
				tv <- v
			}

			angle_diff <- apply(tv,1,function(c) {
				return(angular.diff(c,angles))
			})
			dev <- apply(abs(angle_diff),1,min)

		
			# add to the result vector
			deviation <- c(deviation,dev)


			dates <- c(dates,date)

		}

	}

	
	erg <- data.frame(date=dates,dev=deviation/pi*180)	

	return(erg)	
}

# calculates speed in unit/s, averages the speed over np points (works only with regular trajectories)
c.speeds <- function(traj,np=1) {

	speeds <- c()
	dates <- c()
	speed_detail <- c()

	for (oo in c(1:length(traj))) {	
		data = traj[[oo]]
		dt <- data$dt
		ddist <- data$dist
		
		date <- data$date
		
		speed <- ddist/dt
		

		average_speeds <- c()
		
		
		le <- length(speed)	
		if (np>1) {
			#smooth the curve by taking the average of np points
			for (i in c(1:le)) {
				low <- max(i-np/2,0)
				high <- min(i + np/2,le)
				average_speed <- speed[low:high]
				average_speed[is.na(average_speed)] <- 0
				average_speed <- sum(average_speed)/length(average_speed)
				average_speeds <- c(average_speeds,average_speed)
			}
		speed <- average_speeds
		} 
		
		speeds <- c(speeds,speed)
		dates <- c(dates,date)
		speed_detail <- c(speed_detail,speed)
	}
	
	
	erg <- data.frame(date=dates,speed=speeds, speed_det=speed_detail)
	class(erg$date) <- "POSIXct"
	return(erg)
}

c.distance <- function(traj) {
  dists <- c()
  time <-c()
	for (oo in c(1:length(traj))) {	
		data = traj[[oo]]	
		dist <- data$dist
		dist <- dist[!is.na(dist)]
		dtime <- data$dt
		dtime <- dtime [!is.na(dtime)]
		dists <- c(dists,sum(dist))
		time <- c(time,sum(dtime))
	}	
	return(sum(dists)/sum(time)*60)
}



c.activity <- function(traj,min_pause_length) {

	activities <- c()
	distances <- c()
	timestart <- c()
	timestop <- c()
	Xstart <- c()
	Ystart <- c()
	Xend <- c()
	Yend <- c()
		
	for (oo in c(1:length(traj))) {

		data = traj[[oo]]
		dt <- data$dt
		xx <- data$x
		yy <- data$y
		ddist <- data$dist
		
		date <- data$date

		moving <- data$dist
		moving[is.na(moving)] <- 0
		dt[is.na(dt)] <- 0

		activity <- c()
		Distance <- c()
		datesstart <- c()
		datesend <- c()
		dataXstart <- c()
		dataYstart <- c()
		dataXend <- c()
		dataYend <- c()


		pause_length <- 0
		active_length <- 0
		Distance_traveled <- 0
		dtempP <- date [1]
		dtempA <-date [1]
		xP<-xx [1]
		yP<-yy [1]
		xA<-xx [1]
		yA<-yy [1]
		active <- FALSE

		# TODO: use rapply here!
		for (i in c(1:length(dt))) {			
			if (moving[i]==0) {
				pause_length <- pause_length + dt[i]
				dtempP= ifelse(pause_length > dt [i],dtempP,date [i])## end of bout temporary
				xP = ifelse(pause_length > dt[i],xP,xx [i])
				yP = ifelse(pause_length > dt[i],yP,yy [i])
				if (active_length>0 && pause_length>=min_pause_length) {
					activity <- c(activity,active_length)
					Distance <- c(Distance,Distance_traveled)
					datesstart <- c(datesstart,dtempA)
					datesend <- c(datesend,dtempP)
					dataXstart <- c(dataXstart,xA)
					dataYstart <- c(dataYstart,yA)
					dataXend <- c(dataXend,xP)
					dataYend <- c(dataYend,yP)
					active_length <- 0
					Distance_traveled <- 0
				}
				
			} else {
				active_length <- active_length + dt[i]
				Distance_traveled <- Distance_traveled + ddist [i]
				if (pause_length>0) {
					dtempA= ifelse( active_length > dt [i],dtempA,date [i]) #start of bout temporary, change if active_length was 0(bout def depend on what happen next)
					xA = ifelse(active_length > dt [i],xA,xx [i])
					yA = ifelse(active_length > dt [i],yA,yy [i])
					if (pause_length<min_pause_length){
						active_length <- active_length + pause_length
						pause_length <- 0
					} else {
						activity <- c(activity,-pause_length)
						Distance <- c(Distance,0)
						datesstart <- c(datesstart,dtempP)
						datesend <- c(datesend,date [i])
						dataXstart <- c(dataXstart,xP)
						dataYstart <- c(dataYstart,yP)
						dataXend <- c(dataXend,xx[i])
						dataYend <- c(dataYend,yy[i])
						pause_length <- 0
					}
				}
				
			}
		}
		# add activitity for the last bout
		i=length(dt)
		dataXend <- c(dataXend,xx[i])
		dataYend <- c(dataYend,yy[i])
		datesend <- c(datesend,date [i])
		if (!pause_length<min_pause_length){
			activity <- c(activity,-pause_length)
			Distance <- c(Distance,0)
			datesstart <- c(datesstart,dtempP)
			dataXstart <- c(dataXstart,xP)
			dataYstart <- c(dataYstart,yP)
		}else{
			active_length <- active_length + pause_length
			activity <- c(activity,active_length)
			Distance <- c(Distance,Distance_traveled)
			datesstart <- c(datesstart,dtempA)
			dataXstart <- c(dataXstart,xA)
			dataYstart <- c(dataYstart,yA)
			}				
						
						
		
		#write activities for this burst
		activities <- c(activities,activity)			
		distances <- c(distances, Distance)
		timestart <- c(timestart, datesstart)
		timestop <- c(timestop, datesend)
		Xstart <- c(Xstart, dataXstart)
		Ystart <- c(Ystart, dataYstart)
		Xend <- c(Xend, dataXend)
		Yend <- c(Yend, dataYend)
	}
	
	erg <- data.frame(datestart=timestart, datesend=timestop,Xs=Xstart,Xe= Xend,Ys=Ystart, Ye=Yend,dist_traveled = distances,act=activities)
	class(erg$datestart) <- "POSIXct"
	class(erg$datesend) <- "POSIXct"

	return(erg)
}


c.activity_speed <- function(traj,min_pause_length) {

	activities <- c()
	distances <- c()
	timestart <- c()
	timestop <- c()
	Xstart <- c()
	Ystart <- c()
	Xend <- c()
	Yend <- c()
	speedmax_all<- c()

	for (oo in c(1:length(traj))) {

		data = traj[[oo]]
		dt <- data$dt
		xx <- data$x
		yy <- data$y
		ddist <- data$dist
		
		date <- data$date

		moving <- data$dist
		moving[is.na(moving)] <- 0
		dt[is.na(dt)] <- 0

		activity <- c()
		Distance <- c()
		datesstart <- c()
		datesend <- c()
		dataXstart <- c()
		dataYstart <- c()
		dataXend <- c()
		dataYend <- c()
		speedmax_burst <- c()

		pause_length <- 0
		active_length <- 0
		Distance_traveled <- 0
		speedmax_temp <- 0
		dtempP <- date [1]
		dtempA <-date [1]
		xP<-xx [1]
		yP<-yy [1]
		xA<-xx [1]
		yA<-yy [1]
		active <- FALSE

		# TODO: use rapply here!
		for (i in c(1:length(dt))) {
cond = (ddist[i]/dt[i]> 250)
cond[is.na (cond)] <- "FALSE"
if (cond) {print (c(moving [i],i,id(traj[oo]) ))}	
		
			if (moving[i]==0) {

				pause_length <- pause_length + dt[i]
				dtempP= ifelse(pause_length > dt [i],dtempP,date [i])## end of bout temporary
				xP = ifelse(pause_length > dt[i],xP,xx [i])
				yP = ifelse(pause_length > dt[i],yP,yy [i])
				if (active_length>0 && pause_length>=min_pause_length) {
					activity <- c(activity,active_length)
					Distance <- c(Distance,Distance_traveled)
					speedmax_burst <- c(speedmax_burst,speedmax_temp)
					datesstart <- c(datesstart,dtempA)
					datesend <- c(datesend,dtempP)
					dataXstart <- c(dataXstart,xA)
					dataYstart <- c(dataYstart,yA)
					dataXend <- c(dataXend,xP)
					dataYend <- c(dataYend,yP)
					active_length <- 0
					Distance_traveled <- 0
					speedmax_temp <-0
				}
				
			} else {
				active_length <- active_length + dt[i]
				Distance_traveled <- Distance_traveled + ddist [i]
				speedmax_temp <- ifelse (ddist[i]/dt[i] >speedmax_temp,ddist [i]/dt[i] ,speedmax_temp)
				if (pause_length>0) {
					dtempA= ifelse( active_length > dt [i],dtempA,date [i]) #start of bout temporary, change if active_length was 0(bout def depend on what happen next)
					xA = ifelse(active_length > dt [i],xA,xx [i])
					yA = ifelse(active_length > dt [i],yA,yy [i])
					if (pause_length<min_pause_length){
						active_length <- active_length + pause_length
						pause_length <- 0
					} else {
						activity <- c(activity,-pause_length)
						Distance <- c(Distance,0)
						speedmax_burst <- c(speedmax_burst,speedmax_temp)
						datesstart <- c(datesstart,dtempP)
						datesend <- c(datesend,date [i])
						dataXstart <- c(dataXstart,xP)
						dataYstart <- c(dataYstart,yP)
						dataXend <- c(dataXend,xx[i])
						dataYend <- c(dataYend,yy[i])
						pause_length <- 0
						speedmax_temp <-0
					}
				}
				
			}
		}
		# add activitity for the last bout
		i=length(dt)
		dataXend <- c(dataXend,xx[i])
		dataYend <- c(dataYend,yy[i])
		datesend <- c(datesend,date [i])
		speedmax_burst <- c(speedmax_burst,speedmax_temp)
		if (!pause_length<min_pause_length){
			activity <- c(activity,-pause_length)
			Distance <- c(Distance,0)
			datesstart <- c(datesstart,dtempP)
			dataXstart <- c(dataXstart,xP)
			dataYstart <- c(dataYstart,yP)
		}else{
			active_length <- active_length + pause_length
			activity <- c(activity,active_length)
			Distance <- c(Distance,Distance_traveled)
			datesstart <- c(datesstart,dtempA)
			dataXstart <- c(dataXstart,xA)
			dataYstart <- c(dataYstart,yA)
			}				
						
						
		
		#write activities for this burst
		activities <- c(activities,activity)			
		distances <- c(distances, Distance)
		speedmax_all<- c(speedmax_all,speedmax_burst)
		timestart <- c(timestart, datesstart)
		timestop <- c(timestop, datesend)
		Xstart <- c(Xstart, dataXstart)
		Ystart <- c(Ystart, dataYstart)
		Xend <- c(Xend, dataXend)
		Yend <- c(Yend, dataYend)
	}
	
	erg <- data.frame(datestart=timestart, datesend=timestop,Xs=Xstart,Xe= Xend,Ys=Ystart, Ye=Yend,dist_traveled = distances,speedmax_inbout = speedmax_all,act=activities)
	class(erg$datestart) <- "POSIXct"
	class(erg$datesend) <- "POSIXct"

	return(erg)
}



# straightness averaged over bin_size points, bin_size=1 returns the relative angles between all relocation, 
# abs = TRUE doesnt differ between right or left turns
c.straightness.ori <- function(traj,bin_size=1,abs=FALSE) {
	
	#if (!attr(traj, "regular")) 
    #    stop("traj should have regular time differences between relocations")
	
	erg_angles <- c()
	erg_dates <- c()

	for (oo in c(1:length(traj))) {
		
		angles <- traj[[oo]]$rel.angle
		dates <- traj[[oo]]$date
		angles[is.na(angles)] <- 0	
	
		
		if (bin_size>length(angles))
			next();
			
		seq = seq(bin_size,length(angles),1)

		angle_sum <-sapply(seq,function(i) {
			if (!abs)
				sum(angles[c((i-bin_size+1):i)]) #/bin_size
			else
				sum(abs(angles[c((i-bin_size+1):i)])) #/bin_size
		})


		
		dates <- dates[seq]		
		erg_angles <- c(erg_angles,angle_sum)
		erg_dates <- c(erg_dates,dates)
	}
	
	erg <- data.frame(date=erg_dates,angle=erg_angles)
	class(erg$date) <- "POSIXct"
	
	return(erg)
}

# straightness averaged over bin_size points, bin_size=1 returns the relative angles between all relocation, 
# abs = TRUE doesnt differ between right or left turns
c.straightness <- function(traj,bin_size=1,abs=FALSE) {
	
	#if (!attr(traj, "regular")) 
    #    stop("traj should have regular time differences between relocations")
	
	erg_angles <- c()
	erg_dates <- c()
	erg_meander <- c()

	for (oo in c(1:length(traj))) {
		
		angles <- traj[[oo]]$rel.angle*180/pi
		
		dates <- traj[[oo]]$date
		speed <-traj[[oo]]$dist/traj[[oo]]$dt
		beta = data.frame(angles,speed,dates)
		is.na (beta$speed [beta$speed>30])
		beta = na.omit(beta)
		

		angles <- beta$angles
		dates <- beta$dates
		meander <- beta$angles/beta$speed
		
		erg_angles <- c(erg_angles,angles)
		erg_dates <- c(erg_dates,dates)
		erg_meander <- c(erg_meander,meander)
	}
	
	erg <- data.frame(date=erg_dates,angle=erg_angles, meander=erg_meander)
	class(erg$date) <- "POSIXct"
	
	return(erg)
}



c.straightness.fast <- function(traj,abs=FALSE) {
	
	#if (!attr(traj, "regular")) 
    #    stop("traj should have regular time differences between relocations")
	
	erg_angles <- c()
	erg_dates <- c()

	for (oo in c(1:length(traj))) {
		
		angles <- traj[[oo]]$rel.angle
		dates <- traj[[oo]]$date
		angles[is.na(angles)] <- 0				

		erg_angles <- c(erg_angles,angles)
		erg_dates <- c(erg_dates,dates)
	}
	
	erg <- data.frame(date=erg_dates,angle=erg_angles)
	class(erg$date) <- "POSIXct"
	
	return(erg)
}

# compute number of walks from trajectorie
c.nwalks<- function(traj,env) {

	if (!inherits(env,"bur.env"))
		stop("env should be of class \"bur.env\".")

		
	if (length(env$stripe_pos[[1]])==0)
		return(data.frame());

	if (!inherits(traj, "ltraj")) 
        stop("ltraj should be of class \"ltraj\"")
	
	erg <- c()

	#define the areas between the fly circles to count as a walk
	center <- c(env$cx,env$cy)
	stripe_pos <- env$stripe_pos[[1]]
	
	
	a_normvec <- matrix(c(cos(stripe_pos),sin(stripe_pos)),ncol=2)
	a_dist <- env$r*0.8
		
	start_date <- c()
	end_date <- c()
	distances <- c()
	
	for (oo in c(1:length(traj))) {
		
		data <- traj[[oo]]
		points <- matrix(c(data$x,data$y),ncol=2)
				
		dist <- data$dist
		dist[is.na(dist)] <- 0
		
		# center the points at the arena center
		if (center [1] >0){
		points[,1] <- points[,1] - center[1]
		points[,2] <- points[,2] - center[2]}
		
		# now test if the fly comes near the arena border where the stripe is attached
		dp <- points%*%t(a_normvec) - a_dist #dotproduct
		in_area <- dp>0

		mask <- seq(TRUE,nrow(a_normvec))
		in_btw <- TRUE
		first_run <- TRUE

		distance <- 0
		
		# count the walks	
		for (i in c(1:nrow(in_area))) {
			# fly leaves the stripe area
			if (!in_btw&&sum(in_area[i,])==0) {
				in_btw <- TRUE
				start_date_c <- data$date[i]
				distance <- 0
			}
			# fly enters the stripe area
			if (in_btw&&sum(in_area[i,]&mask)>0) {
				if (first_run)
					first_run <- FALSE
				else {
					end_date <- c(end_date,I(data$date[i]))
					start_date <- c(start_date,I(start_date_c))
					distances <- c(distances,distance)
				}
				in_btw <- FALSE
				mask <- !(in_area[i,]&mask)
			}
			distance <- distance + dist[i]
		}				
	}
	
	if (length(start_date)==0)
		return(data.frame(c()))

	erg <- data.frame(start=start_date,end=end_date,dt=end_date-start_date,dist=distances)
	class(erg$start) <- "POSIXct"
	class(erg$end) <- "POSIXct"

	return(erg)
}

# creates a mean table of a data_table, containing one column with the group 
# and one ore more columns containg the data which should be averaged 
create.mean.table <- function(data_table,groups,data_cols=1,group_col=ncol(data_table)) {	
	mean_table = data.frame()
	sd_table = data.frame()
	se_table = data.frame()
	for (g in groups) {
		data = data_table[data_table[group_col]==g,]
		mean_table = rbind(mean_table,data.frame(t(colMeans(data[data_cols],na.rm=TRUE))))
		sd_table = rbind(sd_table,data.frame(t(sapply(data[data_cols],na.rm=TRUE,sd))))
		se_table = sd_table/sqrt(nrow(data))
	}
	rownames(mean_table) = groups
	rownames(sd_table) = groups
	
	erg <- list(mean_table,sd_table,se_table)
	names(erg) <- c("means","sds","ses")
	return(erg)
}


####individual data: plot trajectories /individual analyses
## trajectories
plot.circle <- function (traj, env, g_traj_color1=TRUE) {
  traj_title = paste(c("Trajectorie for ", id(traj[1])),collapse="")
  
  if (!inherits(env,"bur.env"))
      stop("env should be of class \"bur.env\".")
  
  		
  	if (!inherits(traj, "ltraj")) 
          stop("ltraj should be of class \"ltraj\".")
          
  
  umg <- as.area(create.arena.poly(env,outer_circle=FALSE))
  
  radius=env$r
  xdim = c((env$cx-radius),(env$cx+radius))
  ydim = c((env$cy-radius),(env$cy+radius))
  s = create.stripe.poly(env)
  plot.ltraj(traj, area=umg, xlab="", ylab="y [mm]", main=traj_title, 
    xlim=xdim, ylim=ydim, colpol="white", addpoints=FALSE,perani=TRUE,final=FALSE,frame=FALSE,xaxt = "n")
  # paint black stripe positions
  
  lines(s$x[s$p_id==0],s$y[s$p_id==0],lwd=4)
  lines(s$x[s$p_id==1],s$y[s$p_id==1],lwd=4) 
  #colour the walks
  walks = c.nwalks(traj,env)
  if (g_traj_color1) {
    if (nrow(walks)>0) 
  		for (i in c(1:nrow(walks))) {
  			lt = gdltraj_old(traj, min = walks$start[i],
  				max = walks$end[i], type="POSIXct")		
  			t = ltraj2traj(lt)			
  			lines(t$x,t$y,col=2+i%%7);		
  	
        }
     }
}


####individual data: plot trajectories /individual analyses
## individual analyses
plotindividtests <- function (traj, g_speed_average=10) {
  
  speed_title =paste("speed average per sec: ", id(traj))
  
  speeds=c()
  speeds = c.speeds(traj,g_speed_average)
   
  durations = get.durations(speeds$date)
  hist(log(speeds$speed), breaks =200)
  layout(matrix (1:2,2,1))
  plot(durations,speeds$speed,type='l',main=speed_title,
    ylab="Speed [mm/s]",xlab="Experiment Duration [s]", xlim=c(0,450))
  plot(durations,speeds$speed,type='l',main=speed_title,
    ylab="Speed [mm/s]",xlab="Experiment Duration [s]", xlim=c(450,900))  
  
  
   
  
  ###angle relative- and speed
    rel.angle2=c()
    dist= c()
  for (oo in c(1:length(traj))){
    rel.angle2 =c(rel.angle2,traj[[oo]]$rel.angle /pi*180)
    dist= c(dist,traj[[oo]]$dist)
    }
  layout(matrix (1:3,3,1)) 
  plot(durations,rel.angle2, ylim=c(-180,180), type= "h", pch=21, cex =speeds$speed/10, xlim=c(0,300))
  points(durations,rel.angle2, ylim=c(-180,180), pch=21, col =speeds$speed/5)
  points(durations,speeds$speed*2-180, col=speeds$speed/5,pch=19)
  abline(h=0, col=2)
  abline(h=90, col=3)
  abline(h=-90, col=3)
  
  plot(durations,rel.angle2, ylim=c(-180,180), type= "h", pch=21, cex =speeds$speed/10, xlim=c(300,600))
  points(durations,rel.angle2, ylim=c(-180,180), pch=21, col =speeds$speed/5)
  points(durations,speeds$speed*2-180, col=speeds$speed/5,pch=19)
  abline(h=0, col=2)
  abline(h=90, col=3)
  abline(h=-90, col=3)
  
  plot(durations,rel.angle2, ylim=c(-180,180), type= "h", pch=21, cex =speeds$speed/10, xlim=c(600,900))
  points(durations,rel.angle2, ylim=c(-180,180), pch=21, col =speeds$speed/5)
  points(durations,speeds$speed*2-180, col=speeds$speed/5,pch=19)
  
  abline(h=0, col=2)
  abline(h=90, col=3)
  abline(h=-90, col=3)
  layout(matrix (1:2,2,1))
  h=hist(rel.angle2, breaks=seq(-181,181,1), xlim= c(-180,180),freq=FALSE,main="relative angle histogram")
  g=hist(rel.angle2/(dist/0.75), breaks=seq(-181,181,1), xlim= c(-180,180),freq=FALSE, main="meander histogram (*7.5)")
  
  h=hist(abs(rel.angle2), breaks=seq(0,180,1), xlim= c(0,180),freq=FALSE,main="relative angle (abs) histogram", plot=FALSE)
  g=hist(abs(rel.angle2/(dist/0.75)), breaks=seq(-1,180,1), xlim= c(0,180),freq=FALSE, main="meander (abs)histogram", plot=FALSE)
  
  plot(h$density~h$mids,  ylim= c(-0.2,0.2),xlim= c(0,180),main="relative angle (red),meanderx7.5(green) histogram", col=2, type="h")
  
  points(g$mids,-g$density-0.02, col=3, pch=19,cex=0.5)
  abline(h=0)
  abline(h=-0.02)
  layout(matrix (1:1,1,1))
}

function (corr, ...) 
{
    corrplot(corr = corr, method = "circle", ...)
}