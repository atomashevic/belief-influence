library(influential)
library(igraph)
library(tidyverse)
library(EGAnet)

load('data-clean/data-reduced.Rds')
load('data-clean/data-clean.Rds')

netvars <- c(colnames(netdata))

countries <- as.data.frame(read_csv('data-clean/external-data.csv'))['Country']

row <- as.data.frame(read_csv('data-clean/external-data.csv'))['Country']

ivi_group_discrete <- function(data,netvars,filtervar,filter)
{
  labels=colnames(data)
  resmat = matrix(0,nrow(filter),length(netvars))
  for (i in 1:nrow(filter))
  {
    datai = data[which(data[filtervar]==filter[i,]),]
    ega <-
      EGA(datai[, netvars],
          model = 'glasso',
          algorithm = 'louvain',
          plot.EGA = FALSE)
    g <-  graph_from_adjacency_matrix(ega$network,
                                      weighted = TRUE,
                                      mode = "undirected")
    g_v <- V(g)
    
    ivi <- ivi(
      graph = g,
      vertices = g_v,
      directed = FALSE,
      scaled = TRUE,
      mode='all',
      weights=E(g)$weights,
      d = 1
    )
    resmat[i,] = ivi
  }
  resmat = as.data.frame(resmat)
  colnames(resmat)<-rownames(as.data.frame(ivi))
  rownames(resmat) <- (filter[,1])
  return(resmat)
}


### IVI ON A FULL SAMPLE

ega <-
  EGA(data[, netvars],
      model = 'glasso',
      algorithm = 'louvain',
      plot.EGA = FALSE)

g <-  graph_from_adjacency_matrix(ega$network,
                                  weighted = TRUE,
                                  mode = "undirected")

g_v <- V(g)

ivi <- ivi(
  graph = g,
  vertices = g_v,
  directed = FALSE,
  scaled = TRUE,
  mode='all',
  weights=E(g)$weights,
  d =1
)

png(filename = 'figures/02-ivi-full-v2.png',width = 2000,height=2000,res=300)
cent_network.vis(
  graph = g,
  cent.metric = ivi,
  directed = FALSE,
  plot.title = "Belief Network - Complete ESS R9 dataset",
  weighted = TRUE,
  legend.title = "IVI value",
  layout = 'fr',
  dist.power = 0.4,
  node.size.min = 15,
  node.size.max = 22,
  node.color = 'C',
  label.color = 'black',
  label.cex = 0.13,
  stroke.color = 'black',
  stroke.size=2,
  show.labels = TRUE,
)
dev.off()


### IVI BY COUNTRY

ivi_cntry <- ivi_group_discrete(data,netvars,'Country',countries)
write_csv(ivi_cntry,file='output/ivi-country.csv')


### plot this

library(ggplot2)
pdata <- cbind(countries[,1],ivi_cntry)
colnames(pdata)[1] = 'Country'

pdata %>% ggplot(aes(
  x = reorder(Country, Politicians),
  y = Politicians,
  group = 1
))  +geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylab("IVI of Trust in Politicians")
  theme_bw()

  pdata %>% ggplot(aes(
    x = reorder(Country, Health),
    y = Health,
    group = 1
  ))  +geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
    xlab("") +
    ylab("IVI of Satifaction with healthcare")
  theme_bw()
  
  pdata %>% ggplot(aes(
    x = reorder(Country, Economy),
    y = Economy,
    group = 1
  ))  +geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
    xlab("") +
    ylab("IVI of Satifaction with the Economy")
  theme_bw()
  
  pdata %>% ggplot(aes(
    x = reorder(Country, Democracy),
    y = Democracy,
    group = 1
  ))  +geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
    coord_flip() +
    xlab("") +
    ylab("IVI of Satifaction with the Democracy")
  theme_bw()
  
  
  
### IVI BY ROW
  
row = as.data.frame(c(1,2,3))

ivi_row <- ivi_group_discrete(data,netvars,'ROW',row)
write_csv(ivi_row,file='output/ivi-row.csv')

### IVI BY CLASS

classes = as.data.frame(levels(data$Class5))
ivi_class <- ivi_group_discrete(data,netvars,'Class5',classes)


### IVI BY STFO GOV