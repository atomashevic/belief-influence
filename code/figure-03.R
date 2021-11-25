library(ggplot2)

data_lr <- readRDS('data/lr5_summary.Rds')
data_polintr <- readRDS('data/polintr_summary.Rds')
data_nwspl <- readRDS('data/nwspol_summary.Rds')
rownames(data_nwspl) <- c('0-30','30-60','60-90','90+')

df <- as.data.frame(rbind(data_lr,data_polintr,data_nwspl))


gg_1 <- ggplot(df, aes(x=`Temperature`,y=`Avg. Energy`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()
gg_2 <- ggplot(df, aes(x=`Avg. Energy`,y=`GIC Mean`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_3 <- ggplot(df, aes(x=`Temperature`,y=`GIC Mean`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 


library(patchwork)

r1 = gg_1

r2 = gg_2 | gg_3

plot = r1 / r2


png(filename = 'figures/figure-03.png',width = 15,height=20,units='cm',res=300)
plot
dev.off()

