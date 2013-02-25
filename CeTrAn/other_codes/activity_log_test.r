# takes g_inputdir,g_filetable and g_outputdir
##and g_duration_slider, g_bin_size
# return activity plots


message("starting activity.r")
### compute the activities
	
act_table = data.frame()

#compute activities for each individum
for (i in c(1:nrow(id_table))) {
	act = c.activity_speed(traj[id(traj)==id_table$id[i]],g_duration_slider/10)
	act_id = rep(id_table$id[i],nrow(act))
	act_group = rep(id_table$group[i],nrow(act))
	act_table = rbind(act_table,data.frame(act,id=act_id,group=act_group))

}

# filter activities which are smaller than the act slider
act_table_ori = act_table
pause_table = act_table[act_table$act<0,]
pause_table$pause = - pause_table$act
act_table_2 = act_table_ori[act_table_ori$act>g_duration_slider/10*4,]
act_table = act_table_ori[act_table_ori$act>0,]

#print(head(act_table_ori))

#calculation of linearity of curve for each bout
#########################
act_table$lin = sqrt( (act_table$Xe-act_table$Xs)^2+(act_table$Ye-act_table$Ys)^2)/act_table$dist_traveled

#print(head(act_table))

### get rid of act of lenght 1
act_table_choice = act_table[act_table$dist_traveled>10,]
#print(head(act_table))


g_duration_slider=i
#calculate total activity time for each individual
sum_act = c()
#sum_act2 =c()
sum_act_choice =c()
median_act = c()
 #median_act2 = c()
 median_act_choice = c()
median_pause = c()
number_pause = c()
median_lin = c()
mean_lin = c()
ppause = c()
for (i in c(1:nrow(id_table))) {
	act = act_table[act_table$id==id_table$id[i],]$act
 #act2= act_table_2[act_table_2$id==id_table$id[i],]$act
 act_choice= act_table_choice[act_table_choice$id==id_table$id[i],]$act
	pause = pause_table[act_table$id==id_table$id[i],]$act
	lin = act_table_choice[act_table_choice$id==id_table$id[i],]$lin
    ppause = c(ppause,act)
	sum_act = c(sum_act,sum(act))
 #sum_act2 =c(sum_act2, sum(act2))
 sum_act_choice=c(sum_act_choice, sum(act_choice))
	median_act = c(median_act,median(act))
 #median_act2 = c(median_act2,median(act2))
 median_act_choice = c(median_act_choice,median(act_choice))
	median_pause = c(median_pause, median(abs(pause)))
	number_pause = c(number_pause,length(pause))
	median_lin = c(median_lin, quantile(lin, probs =0.25, na.rm=TRUE))
	
	
}

hist(log(ppause2), main= paste("log hist act length, pause > 0.", i))

for (i in c(1:15)){
ppause2= ppause[ppause>(i/10)]
hist(log(ppause2), main= paste("log hist act length, pause > 0.", i))
}
act_table_2_2 = data.frame(sumact1=sum_act,#sumact_longtime=sum_act2,
                           sumact_dist=sum_act_choice,
                           median_act2=median_act,#median_act_longtime=median_act2,
                           median_act_dist=median_act_choice,
                           median_pause2=median_pause,number_pause2=number_pause,median_lin2= median_lin,id=id_table$id,group=id_table$group)


f_table=data.frame(f_table,act_table_2_2 [,1:7])

message("starting writing activities log.txt")



setwd(rgghome)

if (g_bin_size==0)
	{g_bin_size = 0.5}	
	
v = act_table$act
bins = seq(min(v[!is.na(v)])-g_bin_size,max(v[!is.na(v)])+g_bin_size,g_bin_size)

#create mean and sd table
mean_table = create.mean.table(act_table_2_2,group_ids,data_cols=1:7)


message("starting plots activity")
### create plots
mybarplot(mean_table$means$sumact1,mean_table$ses$sumact1,rownames(mean_table$means),
	main="Total activity time",ylab="Total activity time [s]", ylim= c(0,ylim_acttime))

# mybarplot(mean_table$means$sumact_longtime,mean_table$ses$sumact_longtime,rownames(mean_table$means),
#  main="Total activity time, activity bouts>g_duration_slider/10*4",ylab="Total activity time [s]", ylim= c(0,ylim_acttime))


mybarplot(mean_table$means$sumact_dist,mean_table$ses$sumact_dist,rownames(mean_table$means),
  main="Total activity time, bouts distance > 10mm",ylab="Total activity time [s]", ylim= c(0,ylim_acttime))
#####
mybarplot(mean_table$means$median_act2,mean_table$ses$median_act2,rownames(mean_table$means),
	main="Mean of medians of bouts length",ylab="median of bouts length [s]",ylim = c(0,5))

##  mybarplot(mean_table$means$median_act_longtime,mean_table$ses$median_act_longtime,rownames(mean_table$means),
##  main="Mean of medians of bouts length bouts >g_duration_slider/10*4",ylab="median of bouts length [s]",ylim = c(0,20))

mybarplot(mean_table$means$median_act_dist,mean_table$ses$median_act_dist,rownames(mean_table$means),
  main="Mean of medians of bouts length_ bouts >10mm",ylab="median of bouts length [s]",ylim = c(0,10))
#####

mybarplot(mean_table$means$number_pause,mean_table$ses$number_pause,rownames(mean_table$means),
	main="Number of pauses threshold",ylab="Number of pauses", ylim = c(0,ylim_pauses))

mybarplot(mean_table$means$median_pause,mean_table$ses$median_pause,rownames(mean_table$means),
	main="Mean of medians of pause time",ylab="Median pause time [s]", ylim =c(0, ylim_pausetime))

mybarplot(mean_table$means$median_lin,mean_table$ses$median_lin,rownames(mean_table$means),
	main="Mean of low quartile of linearity score",ylab="Lin score [0 to 1]", ylim =c(0, 1))


#
bins = 0.05
#hplot1 = hist(act_table$lin,breaks= c(seq(0,1, bins)), plot=FALSE)
#
#print(plot(hplot1, xlim=c(0,1),freq = FALSE))
#
#bins = 0.11
#
hplot4 = hist(act_table$act,breaks= c(seq(0,max(act_table$act)+bins*5, bins*5)), plot=FALSE)
plot(hplot4, freq = FALSE, xlim=c(0,10))
#
#
#
#plot (act_table$lin, act_table$dist_traveled, ylim =c(0,1000))
#
#plot (act_table$lin, act_table$dist_traveled, ylim =c(0,35))
#
#plot (act_table_ori$speedmax_inbout, act_table_ori$dist_traveled)
#hist(act_table_ori$speedmax_inbout)

########

# 
# tail(act_table[act_table$lin==1,])
# hist(act_table_choice$dist_traveled[act_table_choice$group=="real_fly"], breaks= 800, xlim= c(0,50))
# act_table_choice2=subset(act_table_choice,act_table_choice$group=="real_fly")
# plot(act_table_choice2$dist_traveled,act_table_choice2$lin)
# abline( h=quantile(act_table_choice2$lin, probs =0.25, na.rm=TRUE), col=2)
#act_table$bla= (sqrt( (act_table$Xe-act_table$Xs)^2+(act_table$Ye-act_table$Ys)^2))
#plot((act_table$bla) ~act_table$group)
#hist(act_table$bla)

