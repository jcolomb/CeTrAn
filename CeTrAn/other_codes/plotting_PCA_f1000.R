# we get Mean_PCA_3d, PCA_res from the PCA script


##abc is the color for the groups, need to be changed such that one can set which group has color. this code makes the last group in color:
abc= c(rep(1,length(levels(PCA_res$group))-1),2)

M=max(Mean_PCA$means)+max(Mean_PCA$ses)*1.1

pdf("test.pdf")
layout(matrix(c(1,3,2,4), 2, 2, byrow=TRUE), respect=TRUE)
#plot -1 versus 2
 plot(-scores[,1], scores[,2], xlab="PCA 1", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

for (i in 1:length(levels(PCA_res$group))){
  X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
#legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
#   cex=0.7) 
   x=-Mean_PCA_3d$means$PCA_res.PC1[i]
	y=Mean_PCA_3d$means$PCA_res.PC2[i]
	x2=-Mean_PCA_3d$ses$PCA_res.PC1[i]
	y2=Mean_PCA_3d$ses$PCA_res.PC2[i]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)
	
	
#plot -1 versus -3	
 plot(-scores[,1], -scores[,3], xlab="PCA 1", ylab="PCA 3", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)

for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
	#legend("topleft", legend=(levels(PCA_res$group)[i]), fill= 	abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
	#   cex=0.7) 
   x=-Mean_PCA_3d$means$PCA_res.PC1[i]
	y=-Mean_PCA_3d$means$PCA_res.PC3[i]
	x2=-Mean_PCA_3d$ses$PCA_res.PC1[i]
	y2=-Mean_PCA_3d$ses$PCA_res.PC3[i]
	segments (x-x2,y,x+x2,y, col=abc[i])
	segments (x,y-y2,x,y+y2, col=abc[i])
  
	}
	abline (v=0, h=0)	
	
	
	
	
	
#plot 3 versus 2	
 plot(scores[,3], scores[,2], xlab="PCA 3", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)

for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
	#legend("topleft", legend=(levels(PCA_res$group)[i]), fill= 	abc[i], bty="n", inset = c(0,i/30))
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
	#   cex=0.7) 
   x=Mean_PCA_3d$means$PCA_res.PC3[i]
	y=Mean_PCA_3d$means$PCA_res.PC2[i]
	x2=Mean_PCA_3d$ses$PCA_res.PC3[i]
	y2=Mean_PCA_3d$ses$PCA_res.PC2[i]
	segments (x-x2,y,x+x2,y, col=abc[i])
	segments (x,y-y2,x,y+y2, col=abc[i])
  
	}
	abline (v=0, h=0)	
	
#plot legend	
 plot(scores[,3], scores[,2], xlab="legend", ylab="PCA 2", 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M))

#plot(PCA_res$PC2~PCA_res$PC1, type= "n", add=TRUE)

for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC2~ X$PC1, col=i, pch= i+10)
	legend("topleft", legend=(levels(PCA_res$group)[i]), fill= 	abc[i], bty="n", inset = c(0,i/10-0.15)
	)
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i, 
	#   cex=0.7) 
   x=Mean_PCA_3d$means$PCA_res.PC3[i]
	y=Mean_PCA_3d$means$PCA_res.PC2[i]
	x2=Mean_PCA_3d$ses$PCA_res.PC3[i]
	y2=Mean_PCA_3d$ses$PCA_res.PC2[i]
	#segments (x-x2,y,x+x2,y, col=abc[i])
	#segments (x,y-y2,x,y+y2, col=abc[i])
  
	}
	
dev.off()	
