library(ggplot2)

data_lr <- readRDS('data/lr5_summary.Rds')
data_polintr <- readRDS('data/polintr_summary.Rds')
data_nwspl <- readRDS('data/nwspol_summary.Rds')
rownames(data_nwspl) <- c('0-30','30-60','60-90','90+')

df <- as.data.frame(rbind(data_lr,data_polintr,data_nwspl))

# cor.test(df$Temperature,df$`GIC Mean`) #BAAAM! 0.52
# 
# cor.test(df$`Avg. Energy`,df$`GIC Mean`) #BAAAM 0.69
# 
# 
cor.test(df$Temperature,df$`GIC SD`) #INTERESTING BAAAM 0.53

cor.test(df$Temperature,df$`GIC Kurtosis`) # BAAM 0.69
# 
# cor.test(df$`Avg. Energy`,df$`GI Kurtosis`) #BAAM 0.57
# 
# cor.test(df$Temperature,df$`GSM Mean `) #BAAM 0.5
# cor.test(df$Temperature,df$`GSM SD`) #BAAM 0.41
# cor.test(df$Temperature,df$`GSM Kurtosis`) #NICE 0.33
# cor.test(df$`Avg. Energy`,df$`GSM Mean `) #BAAM 0.57
# cor.test(df$`Avg. Energy`,df$`GSM SD`) # 0.52
# cor.test(df$`Avg. Energy`,df$`GSM Kurtosis`) #0.31

gg_1 <- ggplot(df, aes(x=`Temperature`,y=`Avg. Energy`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 
  # ylim(c(0, 500000)) + 
  # labs(subtitle="Area Vs Population", 
  #      y="Population", 
  #      x="Area", 
  #      title="Scatterplot", 
  #      caption = "Source: midwest")

gg_2 <- ggplot(df, aes(x=`Avg. Energy`,y=`GIC Mean`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_3 <- ggplot(df, aes(x=`Temperature`,y=`GIC Mean`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_4 <- ggplot(df, aes(x=`Avg. Energy`,y=`GSM Mean `)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_5 <- ggplot(df, aes(x=`Temperature`,y=`GSM Mean `)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_6 <- ggplot(df, aes(x=`Avg. Energy`,y=`GSM SD`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_7 <- ggplot(df, aes(x=`Temperature`,y=`GSM SD`)) + 
  geom_point() + 
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

library(patchwork)

r1 = gg_1

r2 = gg_2 | gg_3

plot = r1 / r2

r3 = gg_4| gg_5

plot = plot / r3

r4 = gg_6| gg_7

plot = plot/r4


png(filename = 'figures/figure-03.png',width = 15,height=32,units='cm',res=300)
plot
dev.off()

