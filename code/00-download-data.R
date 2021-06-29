# Filename: 00-download-data.R
#
# This script downloads the ESS R9 data.

setwd("~/Documents/work/essnet")
library(essurvey)
set_email('atomashevic@ff.uns.ac.rs')
data <- import_rounds(c(9),format='spss')
