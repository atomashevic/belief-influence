packages <-
  c(
    'here',
    'essurvey',
    'foreign',
    'qgraph',
    'dplyr',
    'tidyverse',
    'questionr',
    'labelled',
    'networktree',
    'igraph',
    'influential',
    'fmsb',
    'devtools',
    'ggplot2'
  )

new.packages <-
  packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
{
  print(sprintf("Installing: %s", paste0(new.packages)))
  install.packages(new.packages)
}
dir.create('figures', showWarnings = FALSE)

source('code/00-download-data.R')
source('code/00-oesch-social-class.R')
source('code/01-data-clean.R')
source('code/02-new-variables.R')
source('code/03-UVA.R',print.eval=TRUE)
source('code/04-network-tree.R',print.eval=TRUE)
source('code/05-IVI-plot.R',print.eval=TRUE)
source('code/06-radar-plot.R',print.eval=TRUE)
source('code/07-scatter-plot.R',print.eval=TRUE)