#pdf("test.pdf")
#GRvar=3
#group_ids=as.character(levels(factor(f_table[,GRvar])))
par(cex.axis=0.6)

MINCOL=ncol(add_table)+ncol(id_table)-1
GRvar=2
message("writeoutput_positive")
save.image(file= paste(outputfile,"_datainputbeforewrite.rdata"))
if (ncol(f_table_positive)>MINCOL){
for (i in c((MINCOL+1): ncol(f_table_positive))){
    thisdatatable= data.frame(values=f_table_positive[,i], group=f_table_positive[,GRvar])
	  thisdataname = names(f_table_positive)[i]
	  meanstable = create.mean.table(thisdatatable,group_ids)
  	ymax = max(meanstable$means$values)+1.2*max(meanstable$ses$values)
  	ymin = min(0,min(meanstable$means$values)-1.2*max(meanstable$ses$values))
  	ymin=ifelse(is.na(ymin),0,ymin)
  	ymax=ifelse(is.na(ymax),0,ymax)
	  mybarplot(meanstable$means$values,meanstable$ses$values,rownames = group_ids,main=paste("mean of ", thisdataname),ylab=thisdataname,ylim= c(ymin,ymax))
  }
}

message("writeoutput index")
if (ncol(f_table_index)> MINCOL){
for (i in c((MINCOL+1):(ncol(f_table_index)-1))){
  	thisdatatable= data.frame(values=f_table_index[,i], group=f_table_positive[,GRvar])
  	thisdataname = names(f_table_index)[i]
  	meanstable = create.mean.table(thisdatatable,group_ids)
  	ymax = 1
  	ymin = -1
  	mybarplot(meanstable$means$values,meanstable$ses$values,rownames = group_ids,main=paste("mean of ", thisdataname),ylab=thisdataname,ylim= c(ymin,ymax))
  }
}
##plot stripe deviation
if (ncol(f_table_index)> MINCOL+2){
  i= MINCOL+3
    thisdatatable= data.frame(values=f_table_index[,i], group=f_table_positive[,GRvar])
	  thisdataname = names(f_table_index)[i]
	  meanstable = create.mean.table(thisdatatable,group_ids)
	  ymax = 50
	  ymin = 0
	  mybarplot(meanstable$means$values,meanstable$ses$values,rownames = group_ids,main=paste("mean of ", thisdataname),ylab="stripe deviation [degrees]",ylim= c(ymin,ymax))
	  abline(h=44, col=2)

}
# ###
###now with boxplots
groupn=length(levels(f_table_index$group))
genotypen=length(levels(as.factor(f_table_index$genotype)))
if (genotypen<2){genotypen=groupn}
  
rep=groupn/genotypen


colors=c()
for (i in c(0,2:genotypen)){
  colors=c(colors,rep(i,rep))
}

if (ncol(f_table_positive)> MINCOL){
  for (i in c((MINCOL+1): ncol(f_table_positive))){
    message(i)
    thisdatatable= data.frame(values=f_table_positive[,i], group=f_table_positive[,2])
    thisdataname = names(f_table_positive)[i]
    
    
    
    ymax =  max(thisdatatable$values)
    ymin = min(0,thisdatatable$values)
    ymin=ifelse(is.na(ymin),0,1.05*ymin)
    ymax=ifelse(is.na(ymax),0,1.05*ymax)
    boxplot(thisdatatable$values~thisdatatable$group,col=colors,las=2, main= thisdataname, ylab=thisdataname,ylim= c(ymin,ymax))
    
  }
}
message("writeoutput index")
if (ncol(f_table_index)>2){
  for (i in c(MINCOL+1,MINCOL+2)){
    thisdatatable= data.frame(values=f_table_index[,i], group=f_table_index[,2])
    thisdataname = names(f_table_index)[i]
    meanstable = create.mean.table(thisdatatable,group_ids)
    ymax = 1
    ymin = -1
    boxplot(thisdatatable$values~thisdatatable$group,col=colors,las=2, main= thisdataname, ylab=thisdataname,ylim= c(ymin,ymax))
  abline(h=0, col=2)
    }
}
##plot stripe deviation
if (ncol(f_table_index)>MINCOL+2){
  i=MINCOL+3
  thisdatatable= data.frame(values=f_table_index[,i], group=f_table_index[,2])
  thisdataname = names(f_table_index)[i]
  meanstable = create.mean.table(thisdatatable,group_ids)
  ymax = 60
  ymin = 0
  boxplot(thisdatatable$values~thisdatatable$group,col=colors, main= thisdataname, ylab=thisdataname,ylim= c(ymin,ymax),las=2)
  abline(h=45, col=2)
  #legend(x=1,y=60,legend=levels(as.factor(f_table_index$genotype)), fill=c(0:5))
}







# #dev.off()
	
	
	
