require(adehabitat)
require(CircStats)

"simm.crw2" <- function(date=1:100, h = 1, r = 0, R=57.5,
                        x0=c(0,0), id="A1", burst=id,
                        typeII=TRUE)
 {
     if (!require(CircStats))
         stop("package CircStats required")
     if (typeII)
         class(date) <- c("POSIX","POSIXct")
     n <- length(date)
     dt <- c(diff(unclass(date)))
     if (all(dt-dt[1]>1e-7))
         stop("the time lag between relocations should be constant")
 
          
         if (h>0) {
            v=sqrt(dt)*rchi(n-1) * h
          } else {
            v=dt/dt*(-h)
		
          }
		ang<-rwrpnorm(n-2,0,r)
         ang=cumsum(c(runif(1,0,2*pi),ang))
x=c(1:n)
y=x
x[1]= x0[1]
y[1] =x0[2]
for (i in c(2: n)){
		x[i]= x[i-1]+v[i-1]*cos(ang[i-1])
		y[i]= y[i-1]+v[i-1]*sin(ang[i-1])
		RR=(x[i]^2+y[i]^2)
		if ((RR> R^2) && (i< (n-1)) ){
			
			x[i]= R*x[i]/sqrt(RR)
			y[i]= R*y[i]/sqrt(RR)
			ang2<-rwrpnorm(n-2-i,0,r)
			ang2=cumsum(c(runif(1,0,2*pi),ang2))
			j=c(1:(n-i-1))
			ang[i+j]=ang2[j]

			}
		}


    res <- as.ltraj(data.frame(x,y),date, id, burst,
                     typeII=typeII)
     return(res)
 }
 
#  
#  
#  now levy walks!
 
 simm.levy2 <- function (date = 1:500, mu = 2, l0 = 1, x0 = c(0, 0), id = "A1", R=57.5,
 
    burst = id, typeII = TRUE)
 {
    if (typeII)
        class(date) <- c("POSIX", "POSIXct")
    n <- length(date)
    dt <- c(diff(unclass(date)))
    if (all(dt - dt[1] > 1e-07))
        stop("the time lag between relocations should be constant")
    ang <- runif(n - 2, -pi, pi)
    v = dt * (l0 * (runif(n - 1)^(1/(1 - mu))))
        ang = cumsum(c(runif(1, 0, 2 * pi), ang))
#    newcode     
    x=c(1:n)
    y=x
    x[1]= x0[1]
    y[1] =x0[2]
    for (i in c(2: n)){
      	x[i]= x[i-1]+v[i-1]*cos(ang[i-1])
    		y[i]= y[i-1]+v[i-1]*sin(ang[i-1])
    		RR=(x[i]^2+y[i]^2)
    		if ((RR> R^2) && (i< (n-1)) ){
    			
    			x[i]= R*x[i]/sqrt(RR)
    			y[i]= R*y[i]/sqrt(RR)
    			ang2<-rwrpnorm(n-2-i,0,r)
    			ang2=cumsum(c(runif(1,0,2*pi),ang2))
    			j=c(1:(n-i-1))
    			ang[i+j]=ang2[j]
    
    			}
    		}
    res <- as.ltraj(data.frame(co, si), date, id, burst, typeII = typeII)
    return(res)
 }
 
  simm.levycorr <- function (date = 1:500, mu = 2, l0 = 1, x0 = c(0, 0), id = "A1", R=57.5,r=0.6,
  
     burst = id, typeII = TRUE)
  {
     if (typeII)
         class(date) <- c("POSIX", "POSIXct")
     n <- length(date)
     dt <- c(diff(unclass(date)))
     if (all(dt - dt[1] > 1e-07))
         stop("the time lag between relocations should be constant")
    ang<-rwrpnorm(n-2,0,r)
    ang=cumsum(c(runif(1,0,2*pi),ang))
     v = dt * (l0 * (runif(n - 1)^(1/(1 - mu))))
         
 #    newcode     
     x=c(1:n)
     y=x
     x[1]= x0[1]
     y[1] =x0[2]
     for (i in c(2: n)){
         x[i]= x[i-1]+v[i-1]*cos(ang[i-1])
     		y[i]= y[i-1]+v[i-1]*sin(ang[i-1])
     		RR=(x[i]^2+y[i]^2)
     		if ((RR> R^2) && (i< (n-1)) ){
     			
     			x[i]= R*x[i]/sqrt(RR)
     			y[i]= R*y[i]/sqrt(RR)
     			ang2<-rwrpnorm(n-2-i,0,r)
           ang2=cumsum(c(runif(1,0,2*pi),ang2))
     			j=c(1:(n-i-1))
     			ang[i+j]=ang2[j]
     
     			}
     		}
     res <- as.ltraj(data.frame(co, si), date, id, burst, typeII = typeII)
     return(res)
  }
  
  "simm.crw2pauses" <- function(date=1:100, h = 1, r = 0, R=57.5,
                          x0=c(0,0), id="A1", burst=id,
                          typeII=TRUE,act =0.5)
   {
       if (!require(CircStats))
           stop("package CircStats required")
       if (typeII)
           class(date) <- c("POSIX","POSIXct")
       n <- length(date)
       dt <- c(diff(unclass(date)))
       if (all(dt-dt[1]>1e-7))
           stop("the time lag between relocations should be constant")
     #activity of probability 1-ttt
   t= runif(n - 1,0,1)
   t
   t=ifelse(t>act,1,0)
   t
            
           if (h>0) {
              v=sqrt(dt)*rchi(n-1) * h*t
            } else {
              v=dt/dt*(-h)*t
    	
            }
  		ang<-rwrpnorm(n-2,0,r)
           ang=cumsum(c(runif(1,0,2*pi),ang))
  x=c(1:n)
  y=x
  x[1]= x0[1]
  y[1] =x0[2]
  for (i in c(2: n)){
  		x[i]= x[i-1]+v[i-1]*cos(ang[i-1])
  		y[i]= y[i-1]+v[i-1]*sin(ang[i-1])
  		RR=(x[i]^2+y[i]^2)
  		if ((RR> R^2) && (i< (n-1)) ){
  			
  			x[i]= R*x[i]/sqrt(RR)
  			y[i]= R*y[i]/sqrt(RR)
  			ang2<-rwrpnorm(n-2-i,0,r)
  			ang2=cumsum(c(runif(1,0,2*pi),ang2))
  			j=c(1:(n-i-1))
  			ang[i+j]=ang2[j]
  
  			}
  		}
  
  
      res <- as.ltraj(data.frame(x,y),date, id, burst,
                       typeII=typeII)
       return(res)
   }
   
   simm.levycorrpauses <- function (date = 1:500, mu = 2, l0 = 1, x0 = c(0, 0), id = "A1", R=57.5,r=0.6,act=0.5,
   
      burst = id, typeII = TRUE)
   {
      if (typeII)
          class(date) <- c("POSIX", "POSIXct")
      n <- length(date)
      dt <- c(diff(unclass(date)))
      if (all(dt - dt[1] > 1e-07))
          stop("the time lag between relocations should be constant")
     ang<-rwrpnorm(n-2,0,r)
     ang=cumsum(c(runif(1,0,2*pi),ang))
     #activity of probability 1-ttt
     t= runif(n - 1,0,1)
     t
     t=ifelse(t>act,1,0)
     t
      v = t*dt * (l0 * (runif(n - 1)^(1/(1 - mu))))
          
  #    newcode     
      x=c(1:n)
      y=x
      x[1]= x0[1]
      y[1] =x0[2]
      for (i in c(2: n)){
          x[i]= x[i-1]+v[i-1]*cos(ang[i-1])
        	y[i]= y[i-1]+v[i-1]*sin(ang[i-1])
      		RR=(x[i]^2+y[i]^2)
      		if ((RR> R^2) && (i< (n-1)) ){
      			
      			x[i]= R*x[i]/sqrt(RR)
      			y[i]= R*y[i]/sqrt(RR)
      			ang2<-rwrpnorm(n-2-i,0,r)
            ang2=cumsum(c(runif(1,0,2*pi),ang2))
      			j=c(1:(n-i-1))
      			ang[i+j]=ang2[j]
      
      			}
      		}
      res <- as.ltraj(data.frame(x, y), date, id, burst, typeII = typeII)
      return(res)
   }
 
#############
############
#########
# i=1
# rrr=0.9
# ttt=0.4
# u <- simm.crw2pauses(1:9000, r = rrr, h=0.75, act=ttt,burst =paste(rrr,ttt,i))
# trajdyn(u)
# plut (sliwinltr(u)
###########
###########
###########
# setwd("d:/dokumente/My Dropbox/buridan/buridan_v2.3/simulated_data/simulated_all")
# 
# for (rrr in c(0.8,0.6)) {
# group =ifelse(rrr==0.8,"corrw_r0.8","corrw_r0.6")
#   for (hhh in c(0.75,1)) {
#   group2 =ifelse(hhh==0.75,"_h0.75","_h1")
#     for (i in 0:9){
#       set.seed(872+i)
# 
#       u <- simm.crw2(1:9000, r = rrr, h=hhh, burst =paste(rrr,hhh,i))
# 
# 
#       data=data.frame(c(1:9000),100*c(1:9000),u[[1]]$x,u[[1]]$y,0)
#       names(data)=c("frame","time","x","y", "burst")
# 
#       write.table(data,file=paste(group,group2,"_",i,".txt",sep="",collapse = NULL),append = FALSE,sep = "\t", quote= FALSE, row.names= FALSE
#       )
#     }
#   }
# }
# 
# for rrr in c(0,0.8)) {
# group =ifelse(rrr==0.8,"levycorrelated08","uncorrelated")
# 
#  #for (hhh in c(0.75,1)) {
#  # group2 =ifelse(hhh==0.75,"_h0.75","_h1")
#     for (i in 0:9){
#       set.seed(872+i)
# 
#       u <- simm.levycorr(1:9000, r = rrr, mu=3,l0 = 1, burst =paste(rrr,i))
# 
# 
#       data=data.frame(c(1:9000),100*c(1:9000),u[[1]]$x,u[[1]]$y,0)
#       names(data)=c("frame","time","x","y", "burst")
# 
#       write.table(data,file=paste(group,group2,"_",i,".txt",sep="",collapse = NULL),append = FALSE,sep = "\t", quote= FALSE, row.names= FALSE
#       )
#    # }
#   }
# }
# ######with activity
# 
# setwd("D:/dokumente/data/buridan/simulated_data/new")
# for (rrr in c(0.985,0.983)) {
# group =ifelse(rrr==0.985,"A","B")
# 
#  for (ttt in c(0.4,0.3)) {
#  group2 =ifelse(ttt==0.3,"C","D")
#     for (i in 0:19){
#       set.seed(899+i)
# 
#       u <- simm.levycorrpauses(1:9000, r = rrr, mu=3,l0 = 1, burst =paste(rrr,i), act=ttt)
#       
# 
#       data=data.frame(c(1:9000),100*c(1:9000),u[[1]]$x,u[[1]]$y,0)
#       names(data)=c("frame","time","x","y", "burst")
# 
#       write.table(data,file=paste(group,group2,"_",i,".txt",sep="",collapse = NULL),append = FALSE,sep = "\t", quote= FALSE, row.names= FALSE
#       )
#    }
#   }
# }
# 
# for (rrr in c(0.95,0.9)) {
# group =ifelse(rrr==0.9,"corrw_r0.8","corrw_r0.6")
#   for (ttt in c(0.66,0.8)) {
#   group2 =ifelse(ttt==0.8,"_h0.75","_h1")
#     for (i in 0:9){
#       set.seed(872+i)
# 
#       u <- simm.crw2pauses(1:9000, r = rrr, h=0.75, act=ttt,burst =paste(rrr,ttt,i))
# 
# 
#       data=data.frame(c(1:9000),100*c(1:9000),u[[1]]$x,u[[1]]$y,0)
#       names(data)=c("frame","time","x","y", "burst")
# 
#       write.table(data,file=paste(group,group2,"_",i,".txt",sep="",collapse = NULL),append = FALSE,sep = "\t", quote= FALSE, row.names= FALSE
#       )
#     }
#   }
# }

# traj=data
# newtraj= mindistkeep3(traj,0.6,35)
# traj=new_traj
# #####get env ???
# i=c(1:length(traj))
# env$ = env[1]
# 
# g_outputdir= "D:/dokumente/data"
# rgghome="D:/dokumente/My Dropbox/buridan/buridan_v2.2/analysis"
# ###
# 
# bla = paste(c(outputfile,"t",g_treshold,"Pt",g_duration_slider,".pdf"),collapse="")
# pdf(bla)
# setwd(rgghome)
# source ("scripts/general.r")
# source ("scripts/straightness.r")
# source ("scripts/occupancy.r")
# source ("scripts/thigmotaxis.r")
# source ("scripts/angledev.r")
# source ("scripts/activity_martin.r")
# g = dev.off()
# source ("scripts/saveworkspace2.r")
# 
# 

setwd("D:/dokumente/data/buridan/simulated_data/totestagainstcs_rrr=0.985_ttt=0.3")
#########
########## make 20 correlated walks

K=1.5*c(-4:4,-1:1,-3:3,0)
K[1]=2.5
K[2]=-0.5
K[13]=1
J= sample(K,20)
L= sample(K,20)
#for (rrr in c(0.9963)) {  #levy
for (rrr in c(0.9965)) {  #corrwalk
group ="A"

 for (ttt in c(0.85)) { #corr
  # for (ttt in c(0.875)) {  #levy
 group2 ="C"
    for (i in 0:19){
      set.seed(920+i)
      k= 0.7+ K[i+1]/10
     

#       u <- simm.levycorrpauses(1:9000, r = rrr, mu=3,l0 = 1, burst =paste(rrr,i), act=ttt)
#       u[[1]]$dist[u[[1]]$dist == 0]=NA
#       median(u[[1]]$dist, na.rm=TRUE)
    # u <- simm.levycorrpauses(1:9000, r = rrr+ J[i+1]/1000000, mu=2.6,l0 = 0.8+ K[i+1]/10, burst =paste(rrr,i), act=ttt)
     u <- simm.crw2pauses(1:9000, r = rrr, h=k, act=ttt,burst =paste(rrr,ttt,i))
      #print(median(u[[1]]$dist, na.rm=TRUE))
      
      data=data.frame(c(1:9000),100*c(1:9000),u[[1]]$x,u[[1]]$y,0)
      names(data)=c("frame","time","x","y", "burst")

      write.table(data,file=paste(group,group2,"_",i,".txt",sep="",collapse = NULL),append = FALSE,sep = "\t", quote= FALSE, row.names= FALSE
      )
   }
  }
}
#########
########## make 20 levy walks

K=1.5*c(-4:4,-1:1,-3:3,0)
K[1]=2.5
K[2]=-0.5
K[13]=1
J= sample(K,20)
L= sample(K,20)
for (rrr in c(0.9963)) {  #levy
#for (rrr in c(0.9965)) {  #corrwalk
group ="B"

 #for (ttt in c(0.85)) { #corr
   for (ttt in c(0.875)) {  #levy
 group2 ="D"
    for (i in 0:19){
      set.seed(920+i)
      k= 0.7+ K[i+1]/10
     

#       u <- simm.levycorrpauses(1:9000, r = rrr, mu=3,l0 = 1, burst =paste(rrr,i), act=ttt)
#       u[[1]]$dist[u[[1]]$dist == 0]=NA
#       median(u[[1]]$dist, na.rm=TRUE)
     u <- simm.levycorrpauses(1:9000, r = rrr+ J[i+1]/1000000, mu=2.6,l0 = 0.8+ K[i+1]/10, burst =paste(rrr,i), act=ttt)
    # u <- simm.crw2pauses(1:9000, r = rrr, h=k, act=ttt,burst =paste(rrr,ttt,i))
      #print(median(u[[1]]$dist, na.rm=TRUE))
      
      data=data.frame(c(1:9000),100*c(1:9000),u[[1]]$x,u[[1]]$y,0)
      names(data)=c("frame","time","x","y", "burst")

      write.table(data,file=paste(group,group2,"_",i,".txt",sep="",collapse = NULL),append = FALSE,sep = "\t", quote= FALSE, row.names= FALSE
      )
   }
  }
}


##### other tests

ttt= 0.88


rrr=0.9962
u <- simm.crw2pauses(1:9000, r = rrr, h=0.6, act=ttt,burst =paste(rrr,ttt,i))
v <- simm.levycorrpauses(1:9000, r = rrr, mu=2.6,l0 = 1, burst =paste(rrr,i), act=0.85)
new_traj <- v
    new_traj2 <- mindistkeep3(new_traj, thresholdmin = g_treshold/10, 
        thresholdmax = 35)
u= new_traj2
v= new_traj2
median (abs(u[[1]]$rel.angle), na.rm=TRUE)/pi*180
X=c.activity(u,min_pause_length = 10)
Y=c.activity(v,min_pause_length = 10)
#head(X)
sum(X$act[X$act>0])
sum(Y$act[Y$act>0])
speeds = c.speeds(u)$speed_det
speeds2 = c.speeds(v)$speed_det
speeds = speeds[speeds>0]
speeds2 = speeds2[speeds2>0]
B= c(0:12, 2000)
B=B/2
B
hist(speeds, main="correlated", xlim=c(0,5), breaks = B)
hist(speeds2, main="levy", xlim=c(0,5), breaks = B)
length(speeds[speeds>0])
length(speeds2[speeds2>0])

speeds = speeds[speeds>0]
  speeds = speeds[!speeds>50]

median(speeds, na.rm=TRUE)


