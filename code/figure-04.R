library(ggplot2)
library(patchwork)
library(readr)


country_data <- readRDS('data/country-gics.Rds')
country_data <- country_data[order(country_data$Country),]

external_data <- as.data.frame(read_csv('csv/external-data.csv'))
external_data <- external_data[order(external_data$Country),]


df <- cbind(country_data,external_data[,-1])

#cor.test(df$Economy,df$GDP) #-0.352 p=0.06
#cor.test(df$Economy,df$GDP_L5) # -0.397 p=0.03
#cor.test(df$Economy,df$GDP_L10) #-0.447 p=0.015

# cor.test(df$GEff,df$Economy) #0.53 p=0.003
# cor.test(df$GEff_L5,df$Economy) #0.44 p=0.015
# cor.test(df$GEff_L10,df$Economy) #0.496 p=0.006

# cor.test(df$Democracy,df$LD) #-0.52 p=0.004
# cor.test(df$Democracy,df$LD_L5) #-0.39 p=0.03
# cor.test(df$Democracy,df$LD_L10) #-0.26 p=0.178


# cor.test(df$Democracy,df$ED) #-0.52 p=0.003
# cor.test(df$Democracy,df$ED_L5) #-0.39 p=0.04
# cor.test(df$Democracy,df$ED_L10) #-0.272 p=0.153

# cor.test(df$Health,df$HAQ) # -0.344 p=0.07
# cor.test(df$Health,df$HAQ_L5) # -0.31 p=0.097
# cor.test(df$Health,df$HAQ_L10) # -0.257 p=0.178
 
# cor.test(df$Pstab,df$Parliament) # 0.31 p=0.09
# cor.test(df$Pstab_L5,df$Parliament) # 0.10 p=0.58
# cor.test(df$Pstab_L10,df$Parliament) # 0.187 p=0.329




gg_1 <- ggplot(df, aes(x=GDP,y=Economy)) + 
  geom_point() + xlab("GDP per capita, thousands of US dollars") +
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_2 <- ggplot(df, aes(x=GEff,y=Economy)) + 
  geom_point() + xlab("Government Effectiveness Index, World Bank") +
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_3 <- ggplot(df, aes(x=LD,y=Democracy)) + 
  geom_point() + xlab("Liberal Democracy Index, V-Dem") + 
  geom_smooth(method="lm", se=T,,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_4 <- ggplot(df, aes(x=ED,y=Democracy)) + 
  geom_point() + xlab("Electoral Democracy Index, V-Dem") +
  geom_smooth(method="lm", se=T,,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_5 <- ggplot(df, aes(x=HAQ,y=Health)) + 
  geom_point() + xlab("Healthcare Access and Quality Index, IHME") +
  geom_smooth(method="lm", se=T,,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

gg_6 <- ggplot(df, aes(x=Pstab, y = Parliament)) + 
  geom_point() + xlab("Political Stability Index, World Bank") +
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) + 

png(filename = 'figures/figure-04.png',width = 18,height=25,units='cm',res=300)
gg_1 + gg_2 + gg_3 + gg_4 + gg_5 + gg_6 + patchwork::plot_layout(ncol=2)
dev.off()

gg_e <- ggplot(df, aes(x=GDP_L10,y=Economy)) + 
  geom_point() + xlab("GDP per capita, thousands of US dollars") +
  geom_smooth(method="lm", se=T,alpha=0.2) + theme_bw()# xlim(c(0, 0.1)) 

png(filename = 'figures/figure-A.png',width = 9,height=9,units='cm',res=300)
gg_e 
dev.off()
