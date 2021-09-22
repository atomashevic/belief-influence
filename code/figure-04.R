library(ggplot2)
library(patchwork)

country_data <- readRDS('data/country-gics.Rds')
country_data <- country_data[order(country_data$Country),]

external_data <- as.data.frame(read_csv('csv/external-data.csv'))
external_data <- external_data[order(external_data$Country),]


df <- cbind(country_data,external_data[,-1])

# cor.test(df$Economy,df$GDP) #-0.357 p=0.05
# cor.test(df$GEff,df$Economy) #0.53 p=0.003

# cor.test(df$Democracy,df$LD) #-0.52 p=0.003
# cor.test(df$Democracy,df$ED) #-0.52 p=0.003

# cor.test(df$Health,df$HAQ) # -0.344 p=0.07
# cor.test(df$Pstab,df$Parliament) # 0.31 p=0.09



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
