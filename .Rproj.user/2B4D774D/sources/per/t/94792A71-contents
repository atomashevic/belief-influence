library(EGAnet)
library(tidyverse)
library(dplyr)

load('data-clean/data-clean.Rds')

netvars <-
  c(
    'PeopleAllow',
    'PeopleInfluence',
    'Parliament',
    'Legal',
    'Police',
    'Politicians',
    'Parties',
    'Economy',
    'Government',
    'Democracy',
    'Education',
    'Health',
    'FairChance',
    'CitInterest',
    'Transparent'
  )

netdata <- data %>% select(netvars)


uva1 <-
  UVA(
    netdata,
    n = nrow(netdata),
    model = 'glasso',
    corr = 'cor_auto',
    method = 'wTO',
    reduce = TRUE,
    reduce.method = 'remove',
    adhoc = TRUE,
  )

# Removed: CitInterest, Transparent, People Allow, Police, Parties

netdata <- uva1$reduced$data

uva2 <-
  UVA(
    netdata,
    n = nrow(netdata),
    model = 'glasso',
    corr = 'cor_auto',
    method = 'wTO',
    reduce = TRUE,
    reduce.method = 'remove',
    adhoc = TRUE,
  )

# Removed: Legal, Parliament, PeopleAllow

netdata <- as.data.frame(uva2$reduced$data)

save(netdata,file='data-clean/data-reduced.Rds')
