library(igraph)
library(psychonetrics)


k_shell <- function(g){
  
  E(g)$weight <- normalize_weights(E(g)$weight)
  V(g)$wd <- weighted_degree(g)
  
  n = length(V(g))
  k_shells <- array(NA,dim = n) 
  k = trunc(min(V(g)$wd),0)
  
  while (any(is.na(k_shells))) {
    while (length(V(g)) > 0 & (any(V(g)$wd < k + 1))) {
      i =  which(V(g)$wd < (k + 1))[1]
      k_shells[as.integer(V(g)$label[i])] = k
      g <- delete_vertices(g,i)
      V(g)$wd <- weighted_degree(g)
    }
    k = k + 1
  }
  
  return(k_shells)
}

normalize_weights <- function(x) {
  x <- x / mean(x)
  x <- x / min(x)
  x <- round(x,digits = 0)
}

weighted_degree <- function(graph) {
  d = degree(graph)
  s = strength(graph,mode = 'total')^2
  return((d*s)^(1/3))
}

gic <- function(graph,i,k_shells)
{
  neigh = unlist(neighborhood(graph, nodes = i,order = 1))
  sum = 0
  for (j in (2:length(neigh))) {
    ki = k_shells[i]
    kj = k_shells[neigh[j]]
    d = distances(graph,v = i,to=neigh[j],mode='all',algorithm="dijkstra")
    sum = sum + (ki*kj*(d^2))
    
  }
  return(sum)
}

self_influence <- function(graph,i,kshells){
  n = length(V(graph))
  return(exp(kshells[i])/n)
}

global_influence <- function(graph,i,kshells){
  neigh = unlist(neighborhood(graph, nodes = i,order = 1))
  E(graph)$weight <- normalize_weights(E(graph)$weight)
  sum = 0
  for (j in (2:length(neigh))) {
    kj = kshells[neigh[j]]
    d = distances(graph,v = i, to = neigh[j], mode='all', algorithm="dijkstra")
    sum = sum + (kj/d)
  }
  return(sum)
}

get_influence <- function(graph){
  kshells <- k_shell(graph)
  n = length(V(graph))
  gics <- array(0, dim = n)
  sis <-  array(0, dim = n)
  gis <-  array(0, dim = n)
  gsms <- array(0, dim = n)
  for (i in 1:n){
    gics[i] = gic(graph,i,kshells)
    sis[i] = self_influence(graph,i,kshells)
    gis[i] = global_influence(graph,i,kshells)
    gsms[i] = sis[i] * gis[i]
  }
 result = as.data.frame(rbind(gics,sis,gis,gsms))
 colnames(result) <- c("PeopleAllow","Parliament","Economy","Democracy","Education","Health","CitInterest")
 return(result)
}


# matrix <- getmatrix(model,matrix = 'omega',group = '4')
# graph <-  graph_from_adjacency_matrix(matrix,
#                                       weighted = TRUE,
#                                       mode = "undirected")
# 
# V(graph)$label <- as.character(1:7)
# 
# test <- get_influence(graph)
# 
# 
