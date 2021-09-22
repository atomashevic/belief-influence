data_lr <- readRDS('data/lr5_influence.Rds')
data_polintr <- readRDS('data/polintr_influence.Rds')
data_nwspl <- readRDS('data/nwspol_influence.Rds')

df <- rbind(data_lr,data_polintr,data_nwspl)
df = df[grepl("gics",rownames(df)),]
rownames(df) = NULL
write.csv2(df,file = 'csv/table-1.csv',row.names = FALSE)
