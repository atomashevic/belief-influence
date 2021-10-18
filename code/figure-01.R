# Network comparison figure
# Showing ideology networks side-by-side

library(qgraph)
library(here)

matrices <- readRDS('data/lr_matrices.Rds')

network_labels <- readRDS('data/variables_after_UVA.Rds')

network_fl <-
  qgraph(
    matrices$FL,
    layout = "spring",
    minimum = 0.05,
    maximum = 0.4,
    details = F,
    theme = "colorblind",
    border.width = 2,
    labels = network_labels,
    border.color = '#555555',
    label.color = "#555555",
    color = "#EEEEEE",
    esize = 14,
    vsize = 18,
    label.cex = 1.1,
    label.scale = T,
    font = 2,
    title = 'Far Left',
    title.cex = 1.5
  )

network_layout <- network_fl$layout

network_l <-
  qgraph(
    matrices$L,
    layout = network_layout,
    minimum = 0.05,
    maximum = 0.3,
    details = F,
    theme = "colorblind",
    border.width = 2,
    labels = network_labels,
    border.color = '#555555',
    label.color = "#555555",
    color = "#EEEEEE",
    esize = 14,
    vsize = 18,
    label.cex = 1.1,
    label.scale = T,
    font = 2,
    title = 'Left',
    title.cex = 1.5
  )

network_c <-
  qgraph(
    matrices$C,
    layout = network_layout,
    minimum = 0.05,
    maximum = 0.3,
    details = F,
    theme = "colorblind",
    border.width = 2,
    labels = network_labels,
    border.color = '#555555',
    label.color = "#555555",
    color = "#EEEEEE",
    esize = 14,
    vsize = 18,
    label.cex = 1.1,
    label.scale = T,
    font = 2,
    title = 'Center',
    title.cex = 1.5
  )

network_r <-
  qgraph(
    matrices$R,
    layout = network_layout,
    minimum = 0.05,
    maximum = 0.3,
    details = F,
    theme = "colorblind",
    border.width = 2,
    labels = network_labels,
    border.color = '#555555',
    label.color = "#555555",
    color = "#EEEEEE",
    esize = 14,
    vsize = 18,
    label.cex = 1.1,
    label.scale = T,
    font = 2,
    title = 'Right',
    title.cex = 1.5
  )

network_fr <-
  qgraph(
    matrices$FR,
    layout = network_layout,
    minimum = 0.05,
    maximum = 0.3,
    details = F,
    theme = "colorblind",
    border.width = 2,
    labels = network_labels,
    border.color = '#555555',
    label.color = "#555555",
    color = "#EEEEEE",
    esize = 14,
    vsize = 18,
    label.cex = 1.1,
    label.scale = T,
    font = 2,
    title = 'Far Right',
    title.cex = 1.5
  )

png(
  filename = 'figures/figure-01.png',
  width = 50,
  height = 15,
  units = 'cm',
  res = 300
)
par(mfrow = c(1, 5))
plot(network_fl)
plot(network_l)
plot(network_c)
plot(network_r)
plot(network_fr)
dev.off()