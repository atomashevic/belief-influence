data_lr <- readRDS('data/lr5_influence.Rds')
data_polintr <- readRDS('data/polintr_influence.Rds')
data_nwspl <- readRDS('data/nwspol_influence.Rds')

df <- rbind(data_lr,data_polintr,data_nwspl)
df = df[grepl("gics",rownames(df)),]
rownames(df) = NULL
df = rapply(object = df, f = round, classes = "numeric", how = "replace", digits = 2) 
write.csv2(df,file = 'csv/table-1-gics.csv',row.names = FALSE)

df <- rbind(data_lr,data_polintr,data_nwspl)
df = df[grepl("gsms",rownames(df)),]
rownames(df) = NULL
df = rapply(object = df, f = round, classes = "numeric", how = "replace", digits = 2) 
write.csv2(df,file = 'csv/table-1-gsms.csv',row.names = FALSE)
