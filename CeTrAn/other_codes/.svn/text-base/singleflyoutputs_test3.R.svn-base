g_traj_color1 =TRUE
g_speed_average=10
speed_title ="speed average per sec"

i=1
##

##

plot.circle <- function (traj, env, g_traj_color1=TRUE) {
  traj_title = paste(c("Trajectorie for ", id(traj[1])),collapse="")
  
  if (!inherits(env,"bur.env"))
    	stop("env should be of class \"bur.env\".")
  
  		
  	if (!inherits(traj, "ltraj")) 
          stop("ltraj should be of class \"ltraj\".")
          
  
  umg <- as.area(create.arena.poly(env,outer_circle=TRUE))
  
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
  			lt = gdltraj(traj, min = walks$start[i],
  				max = walks$end[i], type="POSIXct")		
  			t = ltraj2traj(lt)			
  			lines(t$x,t$y,col=2+i%%7);		
  	
        }
     }
}


##speeds
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
setwd(outputpath)
pdf("individual_tests2.pdf")
for (i in c(1:nrow(id_table))) {
  plot.circle(traj[id(traj)==id_table$id[i]],env[[i]],g_traj_color1=TRUE)
  #plotindividtests(traj[id(traj)==id_table$id[i]])
}
for (i in c(1:nrow(id_table))) {
  #plot.circle(traj[id(traj)==id_table$id[i]],env[[i]],g_traj_color1=TRUE)
  plotindividtests(traj[id(traj)==id_table$id[i]])
}


dev.off()