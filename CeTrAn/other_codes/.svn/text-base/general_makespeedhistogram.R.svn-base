# takes g_inputdir,g_filetable and g_outputdir
##and g_supress_paints
# return general plots, speed walks,..


### R CODE ###
##############
message("starting general.r")

### speed boxplot	
average_speeds = c()
speeds_grouped = c()
speed_hist= c()
k="not given"
#compute speeds for each individum
for (i in c(1:nrow(id_table))) {
	
	speeds = c.speeds(traj[id(traj)==id_table$id[i]])$speed_det
	speeds = speeds[speeds>0]
	speeds = speeds[!speeds>50]
	k2= id_table$group[i]
	if (k2==k) speed_hist= c(speed_hist,speeds)
	k=k2
	hist (speeds, breaks=50, main = id_table$id[i])
	average_speeds = c(average_speeds,mean(speeds,na.rm=TRUE))
}

speed_table = data.frame(speeds=average_speeds,group=id_table$group)
f_table = data.frame(f_table,speeds=average_speeds)
speed_mtable = create.mean.table(speed_table,group_ids)

mybarplot(speed_mtable$means$speeds,speed_mtable$ses$speeds, rownames = group_ids,
	main="Speed plot",ylab="Average walking speed [mm/s]", ylim = c(0,ylim_speed))
for (i in c(1:length(group_ids)))	{
	
	
	
### distance boxplot
#compute distance for each individum
distances = c()
for (i in c(1:nrow(id_table))) {
	dist = c.distance(traj[id(traj)==id_table$id[i]])
	distances = c(distances,dist)
}
dist_table = data.frame(dist=distances,group=id_table$group)
dist_mtable = create.mean.table(dist_table,group_ids)

f_table = data.frame(f_table,dist=distances)

mybarplot(dist_mtable$means$dist,dist_mtable$ses$dist, rownames = group_ids,
	main="Walking distance plot",ylab="Total walking distance [mm]", ylim = c(0,ylim_walkdist))

#### activity boxplot	erased here-activity script
#act_table = data.frame()
##compute activities for each individum
#for (i in c(1:nrow(id_table))) {
#	act = c.activity(traj[id(traj)==id_table$id[i]],g_duration_slider/10)
#	act_id = rep(id_table$id[i],nrow(act))
#	act_group = rep(id_table$group[i],nrow(act))
#	act_table = rbind(act_table,data.frame(act,id=act_id,group=act_group))
#}
## filter activities which are smaller than the act slider
#act_table = act_table[act_table$act>0,]
##calculate total activity time for each individual
#sum_act = c()
#for (i in c(1:nrow(id_table))) {
#	act = act_table[act_table$id==id_table$id[i],]$act
#	sum_act = c(sum_act,sum(act))
#}
#sum_table = data.frame(act_sum=sum_act,group=id_table$group)
#sum_mtable = create.mean.table(sum_table,group_ids)


### number of walks between stripes
#compute walks for each individum
walks = c()
for (i in c(1:nrow(id_table))) {
	walks = c(walks,nrow(c.nwalks(traj[id(traj)==id_table$id[i]],env[[i]])))
}
walk_table = data.frame(walks,group=id_table$group)
walk_mtable = create.mean.table(walk_table,group_ids)
f_table= data.frame(f_table, walks)
mybarplot(walk_mtable$means$walks,walk_mtable$ses$walks,rownames = group_ids,
	main="Walks between stripes",ylab="Number of walks", ylim = c(0,ylim_walks))





setwd(rgghome)



