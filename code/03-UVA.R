# Filename: 03-UVA.R
#
# This script performs two rounds of Unique Variable Analysis (UVA)
# and removes the redundant variables so that afterwards there are no
# pairs of variables with wTO higher than 2SD from the mean wTO of the dataset
#
# Output: `csv/reduced-data.csv`

library(EGAnet)

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


# ! Variables Removed: 'Transparent' 'FairChance' 'PeopleInfluence' 'Legal', 'Parties'
#  Before removal, 4 pairs with wTO higher than 2SD from mean.
#  Highest wTOs: 7.09, 2.90, 2.37, 2.28
#  Use uva1$redundancy$descriptives$centralTendency to see all
#  After removal, 2 paurs with wTO higher than 2SD from mean.
#  Highest wTOs: 2.877, 2.255
#  Use uva1$adhoc$descriptives$centralTendency to see all

uva2 <-
  UVA(
    uva1$reduced$data,
    n = nrow(uva1$reduced$data),
    model = 'glasso',
    corr = 'cor_auto',
    method = 'wTO',
    reduce = TRUE,
    reduce.method = 'remove',
    adhoc = TRUE,
  )

# ! Removed variables: Police, Politicians
# After removal, no wTO is more than 2SD higher from the mean.
# 7 variables remaining after 2 rounds of UVA.

netdata <- as.data.frame(uva2$reduced$data)

write_csv2(netdata,'csv/reduced-data.csv')
