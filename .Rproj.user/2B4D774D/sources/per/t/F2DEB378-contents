library(dplyr)
library(tidyverse)
load('data-clean/data-clean.Rds')
ext = read.csv('data-clean/external-data.csv')
n = nrow(data)

data<- data %>% add_column(Geff=NA,Rol=NA,Vaa=NA,Pstab=NA,ROW=NA)
for(i in 1:n)
{
  c = data$Country[i]
  j = which(ext$Country==c)
  data$Geff[i] = ext$GEff[j]
  data$Rol[i] = ext$RoL[j]
  data$Vaa[i] = ext$VaA[j]
  data$Pstab[i] = ext$Pstab[j]
  data$ROW[i] = ext$ROW[j]
}

save(data,file='data-clean/data-clean.Rds')