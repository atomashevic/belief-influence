library(ggplot2)
library(dplyr)
data_lr <- readRDS('data/lr5_summary.Rds')
data_polintr <- readRDS('data/polintr_summary.Rds')
data_nwspl <- readRDS('data/nwspol_summary.Rds')
rownames(data_nwspl) <- c('0-30','30-60','60-90','90+')


df <-  as.data.frame(data_lr)
df$group <- factor(rownames(df),levels=c('Far Left','Left','Center','Right', 'Far Right'))
df <- df |> select(c('Temperature','Avg. Energy','group'))
colnames(df) <- c('temperature','energy','group')
df$energy <- abs(df$energy)

g_lr <- ggplot(df,aes(x=as.numeric(group))) +
  geom_line(aes(y = temperature), color = "#FC4E07") +
  geom_point(aes(y = temperature),cex = 3, colour = "black") +
  geom_line(aes(y = energy),color = "#00AFBB") +
  geom_point(aes(y = energy),cex = 3, colour = "black") + theme_bw()  +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks =  1:5, labels = levels(df$group), expand = c(0.1,0.1)) +
  scale_y_continuous( limits = c(0.3,0.57)) +
  theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  ggtitle("Left/Right dimension") + theme(legend.position = "")

df = as.data.frame(data_polintr)
df$group <- factor(rownames(df),levels=c('Very','Quite','Hardly','Not'))
df <- df |> select(c('Temperature','Avg. Energy','group'))
colnames(df) <- c('temperature','energy','group')
df$energy <- abs(df$energy)

  
g_polintr <- ggplot(df,aes(x=as.numeric(group))) +
  geom_line(aes(y = temperature), color = "#FC4E07") +
  geom_point(aes(y = temperature),cex = 3, colour = "black") +
  geom_line(aes(y = energy),color = "#00AFBB") +
  geom_point(aes(y = energy),cex = 3, colour = "black") + theme_bw()  +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks =  1:4, labels = levels(df$group), expand = c(0.1,0.1)) +
  scale_y_continuous( limits = c(0.3,0.57)) +
  theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  ggtitle("Interest in politics") + theme(legend.position = "")

  # theme(legend.position = "right") +
  # scale_color_identity(name = "",labels = c( " Negative of \n Avg. Energy"," Temperature"), guide= "legend")

df = as.data.frame(data_nwspl)
df$group <- factor(rownames(df),levels=c('0-30','30-60','60-90','90+'))
df <- df |> select(c('Temperature','Avg. Energy','group'))
colnames(df) <- c('temperature','energy','group')
df$energy <- abs(df$energy)


g_nwspol <- ggplot(df,aes(x=as.numeric(group))) +
  geom_line(aes(y = temperature,colour = "#FC4E07")) + #, color = "#FC4E07"
  geom_point(aes(y = temperature),cex = 3, colour = "black") +
  geom_line(aes(y = energy,colour = "#00AFBB")) + #,color = "#00AFBB"
  geom_point(aes(y = energy),cex = 3, colour = "black") + theme_bw()  +
  xlab("") + ylab("") + 
  scale_x_continuous(breaks =  1:4, labels = levels(df$group), expand = c(0.1,0.1)) +
  scale_y_continuous( limits = c(0.3,0.57)) +
  theme( panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  ggtitle("Daily following news about politics (in minutes)") + theme(legend.position = "right") +
 scale_color_identity(name = "",labels = c( "Absolute Average Energy"," Temperature"), guide= "legend")

library(patchwork)

png(filename='figures/figure-02.png',width=25,height = 8,unit='cm',res=300)

g_lr + g_polintr + g_nwspol

dev.off()