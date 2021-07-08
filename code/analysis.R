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
  print(sprintf("Installing: %s", paste0(packages)))
  install.packages(new.packages)
}

#if !('EGAnet' %in% installed.packages()[, "Package"])
#{github}

dir.create('csv', showWarnings = FALSE)
dir.create('figures', showWarnings = FALSE)

#source(00-dow)