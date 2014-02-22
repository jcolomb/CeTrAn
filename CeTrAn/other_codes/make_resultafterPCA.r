f_table2= subset(f_table,f_table$distance_traveled_mm_p_min>50 )

PCA_res$other=f_table2$ZD_other
xxx= aov(PCA_res$PC1~PCA_res$group+PCA_res$other)
summary(xxx)
TukeyHSD(xxx)

xxx= aov(PCA_res$PC2~PCA_res$group*PCA_res$other)
summary(xxx)
TukeyHSD(xxx)

fm= lm(PCA_res$PC2~PCA_res$group, data=PCA_res)
shapiro.test(residuals(fm)) 

 pairwise.wilcox.test(PCA_res$PC1, PCA_res$group, p.adjust.method = "bonf")
 pairwise.wilcox.test(PCA_res$PC3, PCA_res$group, p.adjust.method = "bonf")

xxx= shapiro.test(PCA_res$PC1~PCA_res$group)#*PCA_res$other)
summary(xxx)

Mean_PCA_3d = create.mean.table(PCA1to3,levels(PCA1to3$PCA_res.group),1:3)

pdf("pcaresults.pdf")
layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE))
#layout.show(4)
dleg=15

M=max(Mean_PCA_3d$means)+max(Mean_PCA_3d$ses)*1.05
ii=1
jj=2
plot(scores[,ii], scores[,jj], xlab=paste("PCA ",ii), ylab=paste("PCA ",jj), 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M)) 
for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC3~ X$PC1, col=i)
	#legend(list(x=-6,y=-i/2+5), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i,  cex=0.7)
legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/dleg)) 

   x=Mean_PCA_3d$means[i,ii]
y=Mean_PCA_3d$means[i,jj]
x2=Mean_PCA_3d$ses[i,ii]
y2=Mean_PCA_3d$ses[i,jj]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)
	
ii=1
jj=3
plot(scores[,ii], scores[,jj], xlab=paste("PCA ",ii), ylab=paste("PCA ",jj), 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M)) 
for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC3~ X$PC1, col=i)
	#legend(list(x=-6,y=-i/2+5), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i,  cex=0.7)
legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/dleg)) 

   x=Mean_PCA_3d$means[i,ii]
y=Mean_PCA_3d$means[i,jj]
x2=Mean_PCA_3d$ses[i,ii]
y2=Mean_PCA_3d$ses[i,jj]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)


ii=3
jj=2
plot(scores[,ii], scores[,jj], xlab=paste("PCA ",ii), ylab=paste("PCA ",jj), 
   type="n", main="distance biplot",xlim=c(-M, M), 
   ylim=c(-M, M)) 
for (i in 1:length(levels(PCA_res$group))){
	X = subset(PCA_res,PCA_res$group == levels(PCA_res$group)[i])
	#points(X$PC3~ X$PC1, col=i)
	#legend(list(x=-6,y=-i/2+5), legend=(levels(PCA_res$group)[i]), fill= i, bty="n")
	 #text(x+1,y-0.1, levels(PCA_res$group)[i], col=i,  cex=0.7)
legend("topleft", legend=(levels(PCA_res$group)[i]), fill= abc[i], bty="n", inset = c(0,i/dleg)) 

   x=Mean_PCA_3d$means[i,ii]
y=Mean_PCA_3d$means[i,jj]
x2=Mean_PCA_3d$ses[i,ii]
y2=Mean_PCA_3d$ses[i,jj]
segments (x-x2,y,x+x2,y, col=abc[i])
segments (x,y-y2,x,y+y2, col=abc[i])
	}
	abline (v=0, h=0)
dev.off()

###make stripe dev plot
pdf("stripedev.pdf")
plot= ggplot(f_table2, aes(x=f_table2$ZA_genotype, y=f_table2$AC_stripe_deviation , fill=f_table2$ZD_other))

plot+ geom_boxplot()+
ylab("stripe deviation [°]")+
xlab("genotype")+
scale_fill_manual(values=c("white", "grey"),name="session") +
theme_bw()+
geom_hline(yintercept=44, colour="red")
dev.off()

plot= ggplot(f_table2, aes(x=f_table2$ZA_genotype, y=f_table2$H_speeds , fill=f_table2$ZD_other))


plot=ggplot(cdata, aes(x=xvalue, y=mean, fill=legendvalue)) + 
    geom_bar(position=position_dodge(),col="black", stat="identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                  width=.2,                    # Width of the error bars
                  position=position_dodge(.9)) +
    ylab(title) +
    xlab(xname) +
    scale_fill_manual(values=c("white", "grey"),name=legname) + # column fill and legend title/ -lables
    #scale_y_continuous(breaks=0:16*4, limits = c(0,28)) +  # 4 sind die Schritte auf der y-Achse, 
    # limits ist range der y-Achse
    theme_bw() 
