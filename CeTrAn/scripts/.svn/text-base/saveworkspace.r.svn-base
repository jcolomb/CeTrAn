A= getwd()

setwd(outputpath)
result_table =cbind (f_table,f_table_index[,c(-1,-2)])
save.image(file= paste(outputfile,"_workspace.rdata"))
write.csv(result_table, file =paste(outputfile,"_table.csv"))


setwd(A)