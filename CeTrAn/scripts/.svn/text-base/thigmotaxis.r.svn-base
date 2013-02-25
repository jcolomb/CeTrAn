## caculate thigmotaxis/centrophobisms indices

c.thigm<- function(traj,env) {
	
	if (!inherits(env,"bur.env"))
		stop("env should be of class \"bur.env\".")

	if (!inherits(traj, "ltraj")) 
        stop("ltraj should be of class \"ltraj\"")
	
	thigmotaxis <- c()
	thigmotaxis_dist <- c()
	dates <- c()
  realdistancelist = c()
	for (oo in c(1:length(traj))) {	
		date =c()
		thigmo <-c()
		thigmo_ori <- c()
    realdistancelist_oo = c()
		
		data <- traj[[oo]]
		dist <- data$dist
		if (length (dist)>1) {
		

		date <- data$date
		xx <- data$x
		yy <- data$y
		distcarree <- xx*xx +yy*yy
		Arenaradiuscarre <- env$r*env$r
    
		
		
		# take only when there is more than one point in the burst
		
			thigmoplus = ifelse (distcarree> Arenaradiuscarre/2,(dist+500),-(dist+500))
			thigmo_ori_plus = ifelse (distcarree> Arenaradiuscarre/2,dist,-dist)
			
			thigmo = c(thigmo,thigmoplus)
			thigmo_ori = c(thigmo_ori, thigmo_ori_plus)
      #return distance to the center, positive value if fly moving, negative value otherwise
      D1=ifelse(sqrt(distcarree)/env$r >1, NA, sqrt(distcarree)/env$r)
      D = ifelse( dist>0,D1,-D1)
      realdistancelist_oo = c(realdistancelist_oo, D)
			}
			
			
		thigmotaxis <- c(thigmotaxis,thigmo)
		thigmotaxis_dist <- c(thigmotaxis_dist,thigmo_ori )
    realdistancelist = c(realdistancelist, realdistancelist_oo)
		dates <- c(dates,date)

	}
	

	
	erg <- data.frame(date=dates, thigmotaxis,thigmotaxis_dist, distances =realdistancelist)	

	return(erg)	
}




#compute thigmotaxis total, and thigmotaxis when moving for each animal
thigm_table = data.frame()

#compute angle deviations for each individum
for (i in c(1:nrow(id_table))) {	
	thigm = c.thigm(traj[id(traj)==id_table$id[i]],env[[i]])
	id = rep(id_table$id[i],nrow(thigm))
	group = rep(id_table$group[i],nrow(thigm))
	thigm_table = rbind(thigm_table,data.frame(thigm,id,group))
}


thigmotaxisratio_total <- c()
thigmotaxisratio_notmoving <- c()
thigmotaxisratio_moving <- c()
thigmotaxisratio_ditancetrav <- c()
for (i in c(1:nrow(id_table))) {
	thigm_table_i = thigm_table[thigm_table$id==id_table$id[i],]
	
	totPLUS = length(thigm_table_i[thigm_table_i$thigmotaxis>=500,]$thigmotaxis)
  nomovPLUS = length(thigm_table_i[thigm_table_i$thigmotaxis==500,]$thigmotaxis)
	movePLUS = length(thigm_table_i[thigm_table_i$thigmotaxis>500,]$thigmotaxis )
	totMIN = length(thigm_table_i[thigm_table_i$thigmotaxis<= (-500),]$thigmotaxis)
  nomovMIN = length(thigm_table_i[thigm_table_i$thigmotaxis==(-500),]$thigmotaxis)
	moveMIN = length(thigm_table_i[thigm_table_i$thigmotaxis<(-500),]$thigmotaxis)
	

	thigmotaxisratio_total_i = (totPLUS-totMIN)/(totPLUS+totMIN)
	thigmotaxisratio_moving_i = (movePLUS-moveMIN)/(movePLUS+moveMIN)
  thigmotaxisratio_notmoving_i = (nomovPLUS-nomovMIN)/(nomovPLUS+nomovMIN)
	thigmotaxisratio_ditancetrav_i= sum (thigm_table_i$thigmotaxis_dist, na.rm=TRUE)
	
	thigmotaxisratio_total = c(thigmotaxisratio_total,thigmotaxisratio_total_i)
	thigmotaxisratio_moving <- c(thigmotaxisratio_moving,thigmotaxisratio_moving_i)
  thigmotaxisratio_notmoving <- c(thigmotaxisratio_notmoving,thigmotaxisratio_notmoving_i)
	thigmotaxisratio_ditancetrav <- c(thigmotaxisratio_ditancetrav,thigmotaxisratio_ditancetrav_i)
	
}
thigm_table_2 = data.frame(thigmotaxisratio_total,thigmotaxisratio_moving,thigmotaxisratio_notmoving,thigmotaxisratio_ditancetrav ,id=id_table$id,group=id_table$group)

f_table_index = data.frame(f_table_index,centrophobism_moving=thigmotaxisratio_moving,centrophobism_sitting=thigmotaxisratio_notmoving)
f_table = data.frame(f_table,centrophobism_moving=thigmotaxisratio_moving,centrophobism_sitting=thigmotaxisratio_notmoving)

# message("starting plotting thigmotaxis")

# setwd(rgghome)

# mean_table = create.mean.table(thigm_table_2,group_ids,data_cols=1:4)

# ### create plots
# #mybarplot(mean_table$means$thigmotaxisratio_total,mean_table$ses$thigmotaxisratio_total,rownames(mean_table$means),
# #	main="Thigmotaxis index total paints",ylab="Index (mean+sem)", ylim= c(-1,1))

# mybarplot(mean_table$means$thigmotaxisratio_notmoving,mean_table$ses$thigmotaxisratio_notmoving,rownames(mean_table$means),
# main="centrophobism index not moving paints",ylab="Index (mean+sem)", ylim= c(-1,1))
	
# mybarplot(mean_table$means$thigmotaxisratio_moving,mean_table$ses$thigmotaxisratio_moving,rownames(mean_table$means),
	# main="centrophobism index moving paints",ylab="Index (mean+sem)", ylim= c(-1,1))
# M=	max(max(mean_table$means$thigmotaxisratio_ditancetrav)+max(mean_table$ses$thigmotaxisratio_ditancetrav)*1.3,0)
# M2=  min(min(mean_table$means$thigmotaxisratio_ditancetrav)-min(mean_table$ses$thigmotaxisratio_ditancetrav)*1.3,0)
# mybarplot(mean_table$means$thigmotaxisratio_ditancetrav,mean_table$ses$thigmotaxisratio_ditancetrav,rownames(mean_table$means),
	# main="distance traveled outside - inside",ylab="distance [mm]", ylim = c(M2,M))		


for (i in c(1:length(levels(id_table[,2])))) {
    dist = thigm_table$distances[thigm_table$group == levels(id_table[,2])[i]]
    dist=na.omit(dist)
    #hist(dist^2, main=levels(id_table[,2])[i], freq=FALSE, breaks= 15)
    #abline (v=0.5, col=2)
    #abline( v= median(dist^2), col=3)
    hist(dist*abs(dist), main=levels(id_table[,2])[i], freq=FALSE, breaks=30)
    dist2= dist[dist>0]
    abline( v= median(dist2*abs(dist2)), col=3)
    dist2= dist[dist<0]
    abline( v= median(dist2*abs(dist2)), col=3)
}
    
    
    