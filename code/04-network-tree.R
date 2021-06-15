library(networktree)
library(EGAnet)

load('data-clean/data-reduced.Rds')
load('data-clean/data-clean.Rds')

netvars <- c(colnames(netdata))



ega <-
  EGA(
    data[, netvars],
    model = 'glasso',
    algorithm = 'louvain',
    plot.EGA = FALSE
  )
plotargs = list(
  vsize = 25,
  alpha = 0.9,
  label.size = 3.5,
  edge.alpha = 0.8,
  legend.names = c(""),
  edge.color = "black",
  legend.size = 0
)

png(filename='figures/01-ega-full.png',width = 2750,height=2750,res=300)
plot(ega,plot.args=plotargs)
dev.off()

data$ROW <- as.factor(data$ROW)
levels(data$ROW) <- c('EA','ED','LD')

nt1 <- networktree(nodevars=data[,netvars],
                   splitvars=data[,c("ROW")],
                   transform='glasso',
                   method='mob')

plot(nt1,labels=netvars)

png(filename='figures/99-tree-ea-vs-ld.png',width = 3500,height=2000,res=300)
comparetree(nt1, id1 = 3, id2 = 5, highlights = 3, plot = TRUE,labels=netvars)
dev.off()
