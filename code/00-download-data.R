# Filename: 00-download-data
# This script downloads the ESS R9 data and stores is in data-raw directory
# as Rds file.
# Output:
#         /data-raw/data.Rds

setwd("~/Documents/work/essnet")
library(essurvey)
set_email('atomashevic@ff.uns.ac.rs')
data_raw <- import_rounds(c(9),format='spss')
save(data_raw,file='data-raw/data.Rds')
