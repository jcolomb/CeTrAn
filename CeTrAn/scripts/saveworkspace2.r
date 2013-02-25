A= getwd()

setwd(outputpath)

save.image(file= paste(outputfile,"_workspace.rdata"))
write.csv(f_table, file =paste(outputfile,"_PCA_table.csv"))


setwd(A)