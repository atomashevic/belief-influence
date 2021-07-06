# Filename: 05-IVI-plot.R
# This script:
#   - Calculates IVI on complete sample and plots IVI network
#   - Calculates IVI on country subsamples and writes res to CSV
#   - Runs NetworkTree for ROW variable
#   - Plots NT difference for two levels of ROW
# Output:
#   - 'figures/02-ivi-full.png' Network plot with node size and color scaled
#     by their IVI
#   - 'figures/99-tree-ea-vs-ld.png' Result of compare function for networks
#     estimated on EA and LD subsamples
#   - 'csv/ivi-country.csv IVI per node per country subsample
#   - 'csv/country-data.csv' Combine country level IVI with external data
#     on country level (political and economic indices)
#   - 'figures/02-ivi-NODE.png' Histograms of specific node IVI per country

library(influential)
library(igraph)
library(tidyverse)
library(EGAnet)

netvars <- c(colnames(netdata))

countries <-
  as.data.frame(read_csv('csv/external-data.csv'))['Country']

row <-
  as.data.frame(read_csv('csv/external-data.csv'))['Country']

ivi_group_discrete <- function(data, netvars, filtervar, filter)
{
  labels = colnames(data)
  resmat = matrix(0, nrow(filter), length(netvars))
  for (i in 1:nrow(filter))
  {
    datai = data[which(data[filtervar] == filter[i,]),]
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
      mode = 'all',
      weights = abs(E(g)$weight),
      d = 1
    )
    resmat[i,] = ivi
  }
  resmat = as.data.frame(resmat)
  colnames(resmat) <- rownames(as.data.frame(ivi))
  rownames(resmat) <- (filter[, 1])
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
  mode = 'all',
  weights = E(g)$weight,
  d = 1 # Small diameter due to small network size
)

png(
  filename = 'figures/02-ivi-full.png',
  width = 2000,
  height = 2000,
  res = 300
)
cent_network.vis(
  graph = g,
  cent.metric = ivi,
  directed = FALSE,
  plot.title = "Belief Network IVI - Complete ESS R9 dataset",
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
  stroke.size = 2,
  show.labels = TRUE,
  show.bottom.border = FALSE,
  show.left.border = FALSE,
)
dev.off()


### IVI BY COUNTRY

ivi_cntry <- ivi_group_discrete(data, netvars, 'Country', countries)
write_csv(ivi_cntry, file = 'csv/ivi-country.csv')

country_data <- cbind(ext, ivi_cntry)
write_csv(country_data, file = 'csv/country-data.csv')


library(ggplot2)

pdata <- cbind(countries[, 1], ivi_cntry)
colnames(pdata)[1] = 'Country'

png(
  filename = 'figures/02-ivi-economy.png',
  width = 2000,
  height = 2000,
  res = 300
)

pdata %>% ggplot(aes(
  x = reorder(Country, Economy),
  y = Economy,
  group = 1
))  + geom_bar(
  stat = "identity",
  fill = "#f68060",
  alpha = .6,
  width = .4
) +
  coord_flip() +
  xlab("") +
  ylab("IVI of Satisfaction with Economy")
theme_bw()

dev.off()

png(
  filename = 'figures/02-ivi-democracy.png',
  width = 2000,
  height = 2000,
  res = 300
)

pdata %>% ggplot(aes(
  x = reorder(Country, Democracy),
  y = Democracy,
  group = 1
))  + geom_bar(
  stat = "identity",
  fill = "#f68060",
  alpha = .6,
  width = .4
) +
  coord_flip() +
  xlab("") +
  ylab("IVI of Satifaction with the Democracy")
theme_bw()
dev.off()

