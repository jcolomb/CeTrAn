



hplot1 = histogram(~ act | id ,data=act_table,breaks=bins, type="percent",col=0)
hplot1 = update(hplot1,main="Activity Histogram (Individuals)", ylab="frequency", xlab="activity time [s]",layout=c(3,3))
print(hplot1)
