
if (!all(times[,1]==f_table[,1])){ stop ("there is a problem with the new f_table (times), please report to developer and do not use the data obtained")}


	temptable= f_table[,-c(1,2)]
	newtable = cbind(id_table,times[,-1],temptable)
	if (!all(f_table[,1]==newtable[,1])){ stop ("there is a problem with the new f_table (groups), please report to developer and do not use the data obtained")}
	
	f_table=newtable
	
	temptable= f_table_positive[,-c(1,2)]
	newtable = cbind(id_table,times[,-1],temptable)
	if (!all(f_table[,1]==newtable[,1])){ stop ("there is a problem with the new f_table (groups), please report to developer and do not use the data obtained")}
	
	f_table_positive=newtable
	
	
	temptable= f_table_index[,-c(1,2)]
	newtable = cbind(id_table,times[,-1],temptable)
	if (!all(f_table[,1]==newtable[,1])){ stop ("there is a problem with the new f_table (groups), please report to developer and do not use the data obtained")}
	
	f_table_index=newtable
	extended_table="extended_table"