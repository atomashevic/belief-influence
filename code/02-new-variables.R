# Filename: 02-new-variables.R
#
# This script adds new country-level variables to `data` dataframe
# Variables are added from an external CSV file `/csv/external-data.csv`
#
# New variables are:
#   - 
#   -
#   -
# TODO: Replace with variable names

ext = read.csv('csv/external-data.csv')
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