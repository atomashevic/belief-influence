library(networktree)
library(EGAnet)

load('data-clean/data-reduced.Rds')
load('data-clean/data-clean.Rds')

netvars <- c(colnames(netdata),'Legal', 'Parliament', 'PeopleAllow')



ega <-
  EGA(
    data[, netvars],
    model = 'glasso',
    algorithm = 'louvain',
    plot.EGA = FALSE
  )
plotargs = list(vsize=25,alpha=0.9,label.size=3.5,edge.alpha=0.8,legend.names=c(""))

png(filename='figures/01-ega-full.png',width = 2750,height=2750,res=300)
plot(ega,plot.args=plotargs)
dev.off()

data$ROW <- as.factor(data$ROW)
levels(data$ROW) <- c('LD','ED','EA')

nt1 <- networktree(nodevars=data[,netvars],
                   splitvars=data[,c("ROW")],
                   transform='glasso',
                   method='mob')

plot(nt1,labels=netvars)
comparetree(nt1, id1 = 2, id2 = 5, highlights = 3, plot = TRUE,labels=netvars)


nt2 <- networktree(nodevars=data[,netvars],
                   splitvars=data[,c("Class5")],
                   transform='glasso',
                   method='ctree')

comparetree(nt2, id1 = 2, id2 = 9, highlights = 3, plot = TRUE,labels=netvars)
comparetree(nt2, id1 = 6, id2 = 9, highlights = 3, plot = TRUE,labels=netvars)

data$ReligionBelong <- as.factor(data$ReligionBelong)
levels(data$ReligionBelong)

nt3 <- networktree(nodevars=data[,netvars],
                   splitvars=data[,c("Age")],
                   transform='glasso',
                   method='mob')
comparetree(nt2, id1 = 2, id2 = 7, highlights = 3, plot = TRUE,labels=netvars)


nt3 <- networktree(nodevars=data[,netvars],
                   splitvars=data[,c("Rol")],
                   transform='glasso',
                   method = 'mob')

comparetree(nt3, id1 = 3, id2 = 41, highlights = 3, plot = TRUE,labels=netvars)

nt4 <- networktree(nodevars=data[,netvars],
                   splitvars=data[,c("Geff")],
                   transform='glasso',
                   method = 'mob')

comparetree(nt3, id1 = 2, id2 = 41, highlights = 3, plot = TRUE,labels=netvars)
comparetree(nt3, id1 = 3, id2 = 41, highlights = 3, plot = TRUE,labels=netvars)
