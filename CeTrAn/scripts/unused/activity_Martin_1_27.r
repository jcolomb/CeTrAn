
c.activity_martin <- function(traj, min_pause_length=0.2) {

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
		if (length(ddist)<12) next

		date <- data$date

		#moving <- data$dist
		#moving[is.na(moving)] <- 0
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
		movprec <- FALSE

		speedmax_temp <- 0
		dtempP <- date [1]
		dtempA <-date [1]
		xP<-xx [1]
		yP<-yy [1]
		xA<-xx [1]
		yA<-yy [1]
		active <- FALSE
    DDD=c()
    DD=c()
		# TODO: use rapply here!
		for (i in c(6:(length(dt)-6))) {

		disttrav1s = (ddist[i-5] +ddist[i-4] +ddist[i-3] +ddist[i-2] +ddist[i-1] +ddist[i] +ddist[i+1] +ddist[i+2] +ddist[i-+3] +ddist[i+4] +ddist[i+5])
		DD=c(DD,disttrav1s)
		movprec =ifelse (disttrav1s <1, FALSE,movprec)
		moving =ifelse (disttrav1s >2.7, TRUE,movprec) 
		#moving is true if >4, false if <2 and the same as in the last loop otherwise
		movprec = moving
			
			if (!moving) {

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
				Distance_traveled <- ifelse(active_length > dt[i],Distance_traveled + dt[i+5], disttrav1s)
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













# takes g_inputdir,g_filetable and g_outputdir
##and g_duration_slider, g_bin_size
# return activity plots


message("starting activity_martin.r")
### compute the activities
	
act_table = data.frame()

#compute activities for each individum
for (i in c(1:nrow(id_table))) {
	act = c.activity_martin(traj[id(traj)==id_table$id[i]])
	act_id = rep(id_table$id[i],nrow(act))
	act_group = rep(id_table$group[i],nrow(act))
	act_table = rbind(act_table,data.frame(act,id=act_id,group=act_group))

}

# filter activities which are smaller than the act slider
act_table_ori = act_table
pause_table = act_table[act_table$act<0,]
pause_table$pause = - pause_table$act
act_table = act_table[act_table$act>0,]
########act_table = act_table[act_table$act>g_duration_slider/10*4,]
#



#calculation of linearity of curve for each bout
#########################
#act_table$lin = sqrt( (act_table$Xe-act_table$Xs)^2+(act_table$Ye-act_table$Ys)^2)/act_table$dist_traveled





#calculate total activity time for each individual
sum_act = c()
median_act = c()
median_pause = c()
number_pause = c()
#median_lin = c()
#mean_lin = c()
for (i in c(1:nrow(id_table))) {
	act = act_table[act_table$id==id_table$id[i],]$act
	pause = pause_table[pause_table$id==id_table$id[i],]$pause
	#lin = act_table[act_table$id==id_table$id[i],]$lin
	sum_act = c(sum_act,sum(act))
	median_act = c(median_act,median(act))
	median_pause = c(median_pause, median(abs(pause)))
	number_pause = c(number_pause,length(pause))
	#median_lin = c(median_lin, quantile(lin, probs =0.5, na.rm=TRUE))
	
	
}
act_table_m2 = data.frame(sum_act,median_act,median_pause,number_pause,id=id_table$id,group=id_table$group)

f_table = data.frame (f_table,sactivitytime_DistT=sum_act,act_bouts_DistT=median_act,
pause_length_DistT=median_pause,numb_pause_mDistT=number_pause)

message("starting writing activities log.txt")



setwd(rgghome)

#if (g_bin_size==0)
#	{g_bin_size = 0.5}	
#	
#v = act_table$act
#bins = seq(min(v[!is.na(v)])-g_bin_size,max(v[!is.na(v)])+g_bin_size,g_bin_size)

#create mean and sd table
mean_table = create.mean.table(act_table_m2,group_ids,data_cols=1:4)


message("starting plots activity")
### create plots
mybarplot(mean_table$means$sum_act,mean_table$ses$sum_act,rownames(mean_table$means),
	main="Total activity time Martin 27-10",ylab="Total activity time [s]", ylim= c(0,ylim_acttime))
mybarplot(mean_table$means$median_act,mean_table$ses$median_act,rownames(mean_table$means),
	main="Mean of medians of bouts length Martin 27-10",ylab="median of bouts length [s]",ylim = c(0,ylim_pausetime))

mybarplot(mean_table$means$number_pause,mean_table$ses$number_pause,rownames(mean_table$means),
	main="Number of pauses Martin 27-10",ylab="Number of pauses", ylim = c(0,ylim_pauses))

mybarplot(mean_table$means$median_pause,mean_table$ses$median_pause,rownames(mean_table$means),
	main="Mean of medians of pause time Martin 27-10",ylab="Median pause time [s]", ylim =c(0, ylim_pausetime))

##mybarplot(mean_table$means$median_lin,mean_table$ses$median_lin,rownames(mean_table$means),
##	main="Mean of medians of linearity score Martin 2_1",ylab="Lin score [0 to 1]", ylim =c(0, 1))

#
#print(plot(hplot1, xlim=c(0,1),freq = FALSE))
#
#bins = 0.11
#
#hplot4 = hist(act_table$act,breaks= c(seq(0,max(act_table$act)+bins*5, bins*5)), plot=FALSE)
#print(plot(hplot4, freq = FALSE, xlim=c(0,100)))
#
#
#
#plot (act_table$lin, act_table$dist_traveled, ylim =c(0,1000))
#
#plot (act_table$lin, act_table$dist_traveled, ylim =c(0,35))
#
#plot (act_table_ori$speedmax_inbout, act_table_ori$dist_traveled)
#hist(act_table_ori$speedmax_inbout)
#
#hplot2 = hist(pause_table$pause,breaks= c(seq(0,200, 0.05),  seq(220,1000,20)), plot=FALSE)
#print(plot(hplot2, xlim=c(0,100)))