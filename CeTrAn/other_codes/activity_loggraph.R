

#compute activities for each individum
for (i in c(1:nrow(id_table))) {
  act = c.activity(traj[id(traj)==id_table$id[i]],g_duration_slider/10)
	act_id = rep(id_table$id[i],nrow(act))
	act_group = rep(id_table$group[i],nrow(act))
	act_table = rbind(act_table,data.frame(act,id=act_id,group=act_group))

}
pdf("tests.pdf")
pause_table = act_table[act_table$act<(-0.5),]
G=as.factor(levels(pause_table$group))
BB= c(-exp(6),-exp(5), -exp(4),-exp(3),-exp(2),-exp(1), -exp(0),-exp(-1))
for (i in c(1:length(G))) {
  pause = pause_table[pause_table$group==G[i],]$act
  h= hist (pause, plot=FALSE, breaks = BB)
  h$counts = cumsum(h$counts)
  k= c(1:(length(BB)-1))
  h$inv [k] = h$counts[length(BB)-k]

  plot(h$inv, log="y",main= G[i], xlab=BB,ylim=c(1,3000)) 

}  
i=1
pause = pause_table[pause_table$group==G[i],]$act
  h= hist (pause, plot=FALSE, breaks = BB)
  h$counts = cumsum(h$counts)
  k= c(1:(length(BB)-1))
  h$inv [k] = h$counts[length(BB)-k]
plot(h$inv, log="y", xlab=BB,ylim=c(1,4500))
for (i in c(1:length(G))) {
  pause = pause_table[pause_table$group==G[i],]$act
  h= hist (pause, plot=FALSE, breaks = BB)
  h$counts = cumsum(h$counts)
  k= c(1:(length(BB)-1))
  h$inv [k] = h$counts[length(BB)-k]

  lines(h$inv,col=i) 

}
  
dev.off()
  
  
  points(h$inv, col=i)
  
  
  
  plot(-BB, log="y",main= G[i], xlab=BB,ylim=c(1,3000))
  
  
  
  
x <- c(rnorm(100), rnorm(50, mean=2,sd=.5))
h <- hist(x, plot=FALSE, breaks=20)

# quick hack to transform histogram into cumulative 
# histogram
#  actually, only the first command is required
#  but this is cleaner to do the 3 of them
h$counts     <- cumsum(h$counts)
h$density    <- cumsum(h$density)
h$itensities <- h$density


plot(h, freq=TRUE, main="(Cumulative) histogram of x", 
 col="navajowhite2", border="turquoise3")
box()
