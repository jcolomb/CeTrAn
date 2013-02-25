#id_talbeori= id_table


f_table = subset (f_table, f_table$group != "rut2080")
f_table = subset (f_table, f_table$group != "rut1")
f_table = subset (f_table, f_table$group != "dunce1")
setwd(outputpath)
pdf("rsh_CS.pdf")


f_table =drop.levels(f_table)
group_ids =levels(f_table$group)

speed_table = data.frame(speeds=f_table$speeds,group=f_table$group)
speed_mtable = create.mean.table(speed_table,group_ids)

mybarplot(speed_mtable$means$speeds,speed_mtable$ses$speeds, rownames = group_ids,
  main="Speed plot",ylab="Average walking speed [mm/s]", ylim = c(0,ylim_speed))

speed_table = data.frame(speeds=f_table$sum_act_martin,group=f_table$group)
speed_mtable = create.mean.table(speed_table,group_ids)

mybarplot(speed_mtable$means$speeds,speed_mtable$ses$speeds, rownames = group_ids,
  main="Time spent active",ylab="Time spent active [s]", ylim = c(0,400))
  
speed_table = data.frame(speeds=f_table$median_dev,group=f_table$group)
speed_mtable = create.mean.table(speed_table,group_ids)

mybarplot(speed_mtable$means$speeds,speed_mtable$ses$speeds, rownames = group_ids,
  main="Deviation from stripe",ylab="mean+/-sem of median deviation [°]", ylim = c(0,50))
  abline(h=45,col=2)

dev.off()

f_table$group
