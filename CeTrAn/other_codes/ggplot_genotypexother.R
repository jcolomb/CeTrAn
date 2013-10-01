#library("plyr")
#library("ggplot2")
#require(gridExtra)

data <- f_table
#attach(data)



#bestimmung von error usw
xname="genotype"
legname="other"
cdatamean <- ddply(data, .(genotype,other), colwise(mean, is.numeric))
cdatalength <- ddply(data, .(genotype,other), colwise(length, is.numeric))
cdatasd <- ddply(data, .(genotype,other), colwise(sd, is.numeric))


setwd(outputpath)
pdf("ggplotoutput.pdf")
for (i in c(3: ncol(cdatamean))){
  title= names(cdatamean[i])
  
  cdata =data.frame (cdatalength[,1],cdatalength[,2],cdatalength[,i],cdatamean[,i],cdatasd[,i]/sqrt(cdatalength[,i]))
  colnames(cdata)= c("xvalue", "legendvalue", "N", "mean", "se")
  
  
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
    theme_bw() #+    # to make background white, has to be before "opts"
    #opts(axis.title.x = theme_text(size=20),   #(face="bold", colour="black")
         #axis.title.y = theme_text(angle=90,size=20),
         #axis.text.x  = theme_text(hjust=0.5, size=16)) 
  grid.arrange(plot)
}
dev.off()



