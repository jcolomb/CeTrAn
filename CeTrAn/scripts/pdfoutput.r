#pdf("test.pdf")
#GRvar=3
#group_ids=as.character(levels(factor(f_table[,GRvar])))


MINCOL=ifelse(exists("extended_table"),8,2)
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
# ###now with boxplots

# if (ncol(f_table_positive)> MINCOL){
  # for (i in c(MINCOL+1: ncol(f_table_positive))){
    # thisdatatable= data.frame(values=f_table_positive[,i], group=f_table_positive[,2])
    # thisdataname = names(f_table_positive)[i]
    
    
    
    # ymax = max(thisdatatable$values)
    # ymin = min(0,thisdatatable$values)
    # ymin=ifelse(is.na(ymin),0,ymin)
    # ymax=ifelse(is.na(ymax),0,ymax)
    # boxplot(thisdatatable$values~thisdatatable$group, main= thisdataname, ylab=thisdataname,ylim= c(ymin,ymax))
    
  # }
# }
# message("writeoutput index")
# if (ncol(f_table_index)>2){
  # for (i in c(3,4)){
    # thisdatatable= data.frame(values=f_table_index[,i], group=f_table_index[,2])
    # thisdataname = names(f_table_index)[i]
    # meanstable = create.mean.table(thisdatatable,group_ids)
    # ymax = 1
    # ymin = -1
    # boxplot(thisdatatable$values~thisdatatable$group, main= thisdataname, ylab=thisdataname,ylim= c(ymin,ymax))
  # abline(h=0, col=2)
    # }
# }
# ##plot stripe deviation
# if (ncol(f_table_index)>4){
  # i=5
  # thisdatatable= data.frame(values=f_table_index[,i], group=f_table_index[,2])
  # thisdataname = names(f_table_index)[i]
  # meanstable = create.mean.table(thisdatatable,group_ids)
  # ymax = 60
  # ymin = 0
  # boxplot(thisdatatable$values~thisdatatable$group, main= thisdataname, ylab=thisdataname,ylim= c(ymin,ymax))
  # abline(h=45, col=2)
  
# }



# #dev.off()
	
	
	
