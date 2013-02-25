  pdf("test_all4.pdf")
for (k in c(6,8,10)){  
  angle = c()
  angle2 = c()
  newtraj =  mindistkeep3(traj, thresholdmin= k/10, thresholdmax = 65)
  for (i in c(1:nrow (id_table))) {
        xx=newtraj[id(newtraj)==id_table$id[i]]
        for (j in c(1:length(xx))){
          angle = c(angle,xx[[j]]$abs.angle/pi)
        } 
        angle2=c(angle2,angle)
  }
  diff =length (na.omit(angle2 [angle2== 0.25]))- length (na.omit(angle2 [angle2== 0.26]))
  hist (angle, main= k, breaks=200, xlab= paste("difference 025to026 =",diff))
  
   
  


  act_table = data.frame()
  for (i in c(1:nrow(id_table))) {
    act = c.activity(newtraj[id(newtraj)==id_table$id[i]],g_duration_slider/10)
    act_id = rep(id_table$id[i],nrow(act))
	  act_group = rep(id_table$group[i],nrow(act))
	  act_table = rbind(act_table,data.frame(act,id=act_id,group=act_group))

  }
  #######normal initial
  for (g in c(-0.5, -0.4, -0.3, -0.2, -0.1,0)){
    pause_table = act_table[act_table$act<(g),]
    G=as.factor(levels(pause_table$group))
    BBa = seq(6,-1, by= -0.5)
    BB= (-exp(BBa))
    BB=BB+g
    BB=c(BB,0)
    for (i in c(1:length(G))) {
      pause = pause_table[pause_table$group==G[i],]$act
      h= hist (pause, plot=FALSE, breaks = BB)
      h$counts = cumsum(h$counts)
      #h$counts[h$counts ==0]=NA
      inv= c(1:(length(BB)-1))
      h$inv [inv] = h$counts[length(BB)-inv]
      max1= max(log(h$inv),na.rm=TRUE)
      LIM = ifelse (max1> 8,max1,8)
      h$inv[h$inv==0] =NA
       plot(log(h$inv), main=paste("pauses: distthreshold=",k,"pausethresh=",-g),ylim=c(5,LIM),xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals",)
      m=lm(log(h$inv)~inv,intercept=TRUE)
      abline(m)
      RR=format((summary(m)$r.squared), digits = 3)
      text (6,5.5, labels=paste("r^2=",RR))
      text (6,5, labels=paste("r=",m$coefficients[2]))
      }
  
    ###
    #for activity
    ###
    
    pause_table = act_table[act_table$act>(-g),]
    G=as.factor(levels(pause_table$group))
    BBa = seq(-1,6, by= 0.5)
    BB= (exp(BBa))
    BB=BB-g
    BB=c(0,BB)
    for (i in c(1:length(G))) {
      pause = pause_table[pause_table$group==G[i],]$act
      h= hist (pause, plot=FALSE, breaks = BB)
      h$counts[h$counts ==0]=NA
      h$counts = cumsum(h$counts)
      inv= c(1:(length(BB)-1))
      h$inv [inv] = h$counts[length(BB)-inv]
      max1= max(log(h$inv),na.rm=TRUE)
      LIM = ifelse (max1> 8,max1,8)
      h$inv[h$inv==0] =NA
       
      plot(log(h$inv), main=paste("activities: distthreshold=",k,"pausethresh=",-g),ylim=c(5,LIM),xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals",)
      m=lm(log(h$inv)~inv,intercept=TRUE)
      abline(m)
      RR=format((summary(m)$r.squared), digits = 3)
      text (6,5.5, labels=paste("r^2=",RR))
      text (6,5, labels=paste("r=",m$coefficients[2]))
      
      
      }
    
    #######stop here
    } 
}    
    dev.off()
    
    ### with id- colestuff correction
    for (g in c(-0.5, -0.4, -0.3, -0.2, -0.1,0)){
    pause_table = act_table[act_table$act<(g),]
    G=as.factor(levels(pause_table$id))
    BBa = seq(6,-1, by= -0.5)
    BB= (-exp(BBa))
    BB=BB+g
    BB=c(BB,0)
    coeff= c()
    for (i in c(1:length(G))) {
      pause = pause_table[pause_table$id==G[i],]$act
      h= hist (pause, plot=FALSE, breaks = BB)
      #h$counts[h$counts==0] =NA
      h$counts = cumsum(h$counts)
      inv= c(1:(length(BB)-1))
      h$inv [inv] = h$counts[length(BB)-inv]
      LIM = ifelse (max(h$inv)> 3000,max(h$inv),3000)
      h$inv[h$inv==0] =NA
      plot(h$inv, log="y",main= paste("threshold =",k,"limit for pause =",g), xlab=BB,ylim=c(1,LIM)) 
      plot(log(h$inv)~inv, xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals")
      #plot(h$inv~BB[inv], xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals")
      m=lm(log(h$inv)~inv)
      abline(m)
      plot(log(h$counts)~inv, xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals")
      
      n=lsfit(inv,log(h$inv))
      abline(n)
      text (3,3, labels=paste(G[i],"r=",m$coefficients[2]))
      coeff= c(coeff,m$coefficients[2])
      
    }
    
    
    ### with id- colestuff correction
    for (g in c(-0.5, -0.4, -0.3, -0.2, -0.1,0)){
    pause_table = act_table[act_table$act<(g),]
    G=as.factor(levels(pause_table$id))
    BBa = seq(6,-1, by= -0.5)
    BB= (-exp(BBa))
    BB=BB+g
    BB=c(BB,0)
    coeff= c()
    for (i in c(1:length(G))) {
      pause = pause_table[pause_table$id==G[i],]$act
      h= hist (pause, plot=FALSE, breaks = BB)
      #h$counts[h$counts==0] =NA
      #h$counts = cumsum(h$counts)
      inv= c(1:(length(BB)-1))
      h$inv [inv] = h$counts[length(BB)-inv]
      #h$inv= cumsum(h$inv)
      LIM = ifelse (max(h$inv)> 3000,max(h$inv),3000)
      h$inv[h$inv==0] =NA
      plot(h$inv, log="y",main= paste("threshold =",k,"limit for pause =",g), xlab=BB,ylim=c(1,LIM)) 
      plot(log(h$inv)~inv, xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals")
      #plot(h$inv~BB[inv], xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals")
      m=lm(log(h$inv)~inv)
      abline(m)
      plot(log(h$counts)~inv, xlab = "length of interval: 1 is exp(-2,5) in o.5 intervals")
      
      n=lsfit(inv,log(h$inv))
      abline(n)
      text (3,3, labels=paste(G[i],"r=",m$coefficients[2]))
      coeff= c(coeff,m$coefficients[2])
      
    }
    #
    
    
  }
}  
dev.off()
