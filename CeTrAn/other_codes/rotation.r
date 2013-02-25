# chemotaxis
newtraj= traj[1]
burst(newtraj)=0

for (j in c(1: nrow(id_table))){
 Ttraj = traj[id(traj)==id_table$id[j]] 
   alpha= env[[j]]$stripe_pos[[1]][1]
   for (i in c(1: length(Ttraj))){
    x=Ttraj[[i]]$x
    y=Ttraj[[i]]$y
    da=Ttraj[[i]]$date
  
    

    x2= x*sin(alpha)- y*cos(alpha)
    y2= y*sin(alpha)+ x*cos(alpha)
    z= data.frame(x2,y2)

    lt <- as.ltraj(z, da, id = attr(Ttraj[[i]], "id"), burst = attr(Ttraj[[i]], 
                "burst"), typeII = attr(Ttraj[i], "typeII"))

    
    newtraj= c(newtraj,lt)
   }
  env[[j]]$stripe_pos[[1]][1] = pi/2
  env[[j]]$stripe_pos[[1]][2] = pi/2
 }

newtraj= newtraj[2:length(newtraj)]

##dist to odor
setwd(outputpath)
pdf("distance.pdf")
Distance = c()
for (j in c(1: nrow(id_table))){
 Ttraj = traj[id(traj)==id_table$id[j]] 
   alpha= env[[j]]$stripe_pos[[1]][1]
   for (i in c(1: length(Ttraj))){
    x=Ttraj[[i]]$x
    y=Ttraj[[i]]$y
    
    Dy= env[[j]]$outer_r 
    
    Dist = sqrt( (Dy+y)^2 + x^2)
    Distance= c(Distance,Dist)
    plot(Dist, ylim=c(0,140))
    abline(h=Dy)
    abline(h=Dy-env[[j]]$r, col=2)
    abline(h=Dy+env[[j]]$r, col=2)
   }
}
dev.off()


for (j in c(1:12)){
 Ttraj = traj[id(traj)==id_table$id[j]] 
   alpha= env[[j]]$stripe_pos[[1]][1]
   for (i in c(1: length(Ttraj))){
    x=Ttraj[[i]]$x
    y=Ttraj[[i]]$y
    
    Dy= env[[j]]$outer_r 
    
    Dist = sqrt( (Dy+y)^2 + x^2)
    Distance= c(Distance,Dist)
    plot(Dist, ylim=c(0,140))
    abline(h=Dy)
    abline(h=Dy-env[[j]]$r, col=2)
    abline(h=Dy+env[[j]]$r, col=2)
   }
}
plot(Distance, ylim=c(0,140))
    abline(h=Dy)
    abline(h=Dy-env[[j]]$r, col=2)
    abline(h=Dy+env[[j]]$r, col=2)
boxplot(Distance)
violinplot(Distance)