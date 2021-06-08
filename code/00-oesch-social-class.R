library(dplyr)
library(questionr)
library(labelled)

factorsNumeric <-  function(d)
  modifyList(d, lapply(d[, sapply(d, is.factor)], as.numeric))

load('data-raw/data.Rds')
d <- data_raw
#### Recode occupation variable (isco08 com 4-digit) for respondents

tail(freq(d$isco08, total = T))

d$isco_mainjob <- d$isco08
d$isco_mainjob[is.na(d$isco_mainjob)] <- -9
var_label(d$isco_mainjob) <- "Current occupation of respondent - isco08 4-digit"

head(freq(d$isco_mainjob, total = T))


#### Recode employment status for respondents

freq(d$emplrel, total = T)
freq(d$emplno, total = T)

d$emplrel_r <- d$emplrel
d$emplrel_r[is.na(d$emplrel_r)] <- 9
val_label(d$emplrel_r, 9) <- "Missing"
freq(d$emplrel_r, total = T)


d$emplno_r <- d$emplno
d$emplno_r[is.na(d$emplno_r)] <- 0
d$emplno_r[d$emplno_r >= 1 & d$emplno_r <= 9] <- 1
d$emplno_r[d$emplno_r >= 10 & d$emplno_r <= 66665] <- 2
val_labels(d$emplno_r) <- c("0 employees" = 0,
                            "1-9 employees" = 1,
                            "10+ employees" = 2)
freq(d$emplno_r, total = T)

d$selfem_mainjob <- NA
d$selfem_mainjob[d$emplrel_r == 1 | d$emplrel_r == 9] <- 1
d$selfem_mainjob[d$emplrel_r == 2 & d$emplno_r == 0] <- 2
d$selfem_mainjob[d$emplrel_r == 3] <- 2
d$selfem_mainjob[d$emplrel_r == 2 & d$emplno_r == 1] <- 3
d$selfem_mainjob[d$emplrel_r == 2 & d$emplno_r == 2] <- 4

val_labels(d$selfem_mainjob) <- c("Not self-employed" = 1,
                                  "Self-empl without employees" = 2,
                                  "Self-empl with 1-9 employees" = 3,
                                  "Self-empl with 10 or more" = 4)
var_label(d$selfem_mainjob) <- "Employment status for respondants"
freq(d$selfem_mainjob, total = T)


#################################################
# Create Oesch class schema for respondents
#################################################

d$class16_r <- -9

# Large employers (1)

d$class16_r[d$selfem_mainjob == 4] <- 1

# Self-employed professionals (2)

d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2000 & d$isco_mainjob <= 2162] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2164 & d$isco_mainjob <= 2165] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2200 & d$isco_mainjob <= 2212] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob == 2250] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2261 & d$isco_mainjob <= 2262] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2300 & d$isco_mainjob <= 2330] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2350 & d$isco_mainjob <= 2352] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2359 & d$isco_mainjob <= 2432] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2500 & d$isco_mainjob <= 2619] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob == 2621] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2630 & d$isco_mainjob <= 2634] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2636 & d$isco_mainjob <= 2640] <- 2
d$class16_r[(d$selfem_mainjob == 2 | d$selfem_mainjob == 3) & d$isco_mainjob >= 2642 & d$isco_mainjob <= 2643] <- 2

# Small business owners with employees (3)

d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 1000 & d$isco_mainjob <= 1439] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2163] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2166] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2220 & d$isco_mainjob <= 2240] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2260] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2263 & d$isco_mainjob <= 2269] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2340 & d$isco_mainjob <= 2342] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2353 & d$isco_mainjob <= 2356] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2433 & d$isco_mainjob <= 2434] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2620] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2622] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2635] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob == 2641] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 2650 & d$isco_mainjob <= 2659] <- 3
d$class16_r[d$selfem_mainjob == 3 & d$isco_mainjob >= 3000 & d$isco_mainjob <= 9629] <- 3

# Small business owners without employees (4)

d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 1000 & d$isco_mainjob <= 1439] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2163] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2166] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2220 & d$isco_mainjob <= 2240] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2260] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2263 & d$isco_mainjob <= 2269] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2340 & d$isco_mainjob <= 2342] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2353 & d$isco_mainjob <= 2356] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2433 & d$isco_mainjob <= 2434] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2620] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2622] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2635] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob == 2641] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 2650 & d$isco_mainjob <= 2659] <- 4
d$class16_r[d$selfem_mainjob == 2 & d$isco_mainjob >= 3000 & d$isco_mainjob <= 9629] <- 4

# Technical experts (5)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2100 &  d$isco_mainjob <= 2162] <- 5
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2164 &  d$isco_mainjob <= 2165] <- 5
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2500 &  d$isco_mainjob <= 2529] <- 5

# Technicians (6)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3100 &  d$isco_mainjob <= 3155] <- 6
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3210 &  d$isco_mainjob <= 3214] <- 6
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3252] <- 6
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3500 &  d$isco_mainjob <= 3522] <- 6

# Skilled manual (7)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 6000 &  d$isco_mainjob <= 7549] <- 7
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8310 &  d$isco_mainjob <= 8312] <- 7
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8330] <- 7
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8332 &  d$isco_mainjob <= 8340] <- 7
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8342 &  d$isco_mainjob <= 8344] <- 7

# Low-skilled manual (8)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8000 &  d$isco_mainjob <= 8300] <- 8
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 8320 &  d$isco_mainjob <= 8321] <- 8
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8341] <- 8
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8350] <- 8
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9200 &  d$isco_mainjob <= 9334] <- 8
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9600 &  d$isco_mainjob <= 9620] <- 8
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9622 &  d$isco_mainjob <= 9629] <- 8

# Higher-grade managers and administrators (9)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1000 &  d$isco_mainjob <= 1300] <- 9
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1320 &  d$isco_mainjob <= 1349] <- 9
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2400 &  d$isco_mainjob <= 2432] <- 9
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2610 &  d$isco_mainjob <= 2619] <- 9
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2631] <- 9
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 100 &  d$isco_mainjob <= 110] <- 9

# Lower-grade managers and administrators (10)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1310 &  d$isco_mainjob <= 1312] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 1400 &  d$isco_mainjob <= 1439] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2433 &  d$isco_mainjob <= 2434] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3300 &  d$isco_mainjob <= 3339] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3343] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3350 &  d$isco_mainjob <= 3359] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3411] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5221] <- 10
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 200 &  d$isco_mainjob <= 210] <- 10

# Skilled clerks (11)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3340 &  d$isco_mainjob <= 3342] <- 11
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3344] <- 11
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4000 &  d$isco_mainjob <= 4131] <- 11
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4200 &  d$isco_mainjob <= 4221] <- 11
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4224 &  d$isco_mainjob <= 4413] <- 11
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 4415 &  d$isco_mainjob <= 4419] <- 11

# Unskilled clerks (12)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4132] <- 12
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4222] <- 12
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4223] <- 12
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5230] <- 12
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 9621] <- 12

# Socio-cultural professionals (13)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2200 &  d$isco_mainjob <= 2212] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2250] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2261 &  d$isco_mainjob <= 2262] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2300 &  d$isco_mainjob <= 2330] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2350 &  d$isco_mainjob <= 2352] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2359] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2600] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2621] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2630] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2632 &  d$isco_mainjob <= 2634] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2636 &  d$isco_mainjob <= 2640] <- 13
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2642 &  d$isco_mainjob <= 2643] <- 13

# Socio-cultural semi-professionals (14)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2163] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2166] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2220 &  d$isco_mainjob <= 2240] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2260] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2263 &  d$isco_mainjob <= 2269] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2340 &  d$isco_mainjob <= 2342] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2353 &  d$isco_mainjob <= 2356] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2620] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2622] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2635] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 2641] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 2650 &  d$isco_mainjob <= 2659] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3200] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3220 &  d$isco_mainjob <= 3230] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3250] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3253 &  d$isco_mainjob <= 3257] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3259] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3400 &  d$isco_mainjob <= 3410] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3412 &  d$isco_mainjob <= 3413] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3430 &  d$isco_mainjob <= 3433] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3435] <- 14
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 4414] <- 14

# Skilled service (15)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3240] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3251] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3258] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 3420 &  d$isco_mainjob <= 3423] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 3434] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5000 &  d$isco_mainjob <= 5120] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5140 &  d$isco_mainjob <= 5142] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5163] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5165] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5200] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5220] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5222 &  d$isco_mainjob <= 5223] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5241 &  d$isco_mainjob <= 5242] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5300 &  d$isco_mainjob <= 5321] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5400 &  d$isco_mainjob <= 5413] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5419] <- 15
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8331] <- 15

# Low-skilled service (16)

d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5130 &  d$isco_mainjob <= 5132] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5150 &  d$isco_mainjob <= 5162] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5164] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5169] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5210 &  d$isco_mainjob <= 5212] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5240] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5243 &  d$isco_mainjob <= 5249] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 5322 &  d$isco_mainjob <= 5329] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 5414] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob == 8322] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9100 &  d$isco_mainjob <= 9129] <- 16
d$class16_r[d$selfem_mainjob == 1 & d$isco_mainjob >= 9400 &  d$isco_mainjob <= 9520] <- 16


d$class16_r[d$class16_r == -9] <- NA
val_labels(d$class16_r) <- c("Large employers" = 1,
                             "Self-employed professionals" = 2,
                             "Small business owners with employees" = 3,
                             "Small business owners without employees" = 4,
                             "Technical experts" = 5,
                             "Technicians" = 6,
                             "Skilled manual" = 7,
                             "Low-skilled manual" = 8,
                             "Higher-grade managers and administrators" = 9,
                             "Lower-grade managers and administrators" = 10,
                             "Skilled clerks" = 11,
                             "Unskilled clerks" = 12,
                             "Socio-cultural professionals" = 13,
                             "Socio-cultural semi-professionals" = 14,
                             "Skilled service" = 15,
                             "Low-skilled service" = 16)
var_label(d$class16_r) <- "Respondent's Oesch class position - 16 classes"
freq(d$class16_r, total = T)


d$class8_r <- NA
d$class8_r[d$class16_r <= 2] <- 1
d$class8_r[d$class16_r == 3 | d$class16_r == 4] <- 2
d$class8_r[d$class16_r == 5 | d$class16_r == 6] <- 3
d$class8_r[d$class16_r == 7 | d$class16_r == 8] <- 4
d$class8_r[d$class16_r == 9 | d$class16_r == 10] <- 5
d$class8_r[d$class16_r == 11 | d$class16_r == 12] <- 6
d$class8_r[d$class16_r == 13 | d$class16_r == 14] <- 7
d$class8_r[d$class16_r == 15 | d$class16_r == 16] <- 8
val_labels(d$class8_r) <- c("Self-employed professionals and large employers" = 1,
                            "Small business owners" = 2,
                            "Technical (semi-)professionals" = 3,
                            "Production workers" = 4,
                            "(Associate) managers" = 5,
                            "Clerks" = 6,
                            "Socio-cultural (semi-)professionals" = 7,
                            "Service workers" = 8)
var_label(d$class8_r) <- "Respondent's Oesch class position - 8 classes"
freq(d$class8_r, total = T)



d$class5_r <- NA
d$class5_r[d$class16_r <= 2 | d$class16_r == 5 | d$class16_r == 9 | d$class16_r == 13] <- 1
d$class5_r[d$class16_r == 6 | d$class16_r == 10 | d$class16_r == 14] <- 2
d$class5_r[d$class16_r == 3 | d$class16_r == 4] <- 3
d$class5_r[d$class16_r == 7 | d$class16_r == 11 | d$class16_r == 15] <- 4
d$class5_r[d$class16_r == 8 | d$class16_r == 12 | d$class16_r == 16] <- 5
val_labels(d$class5_r) <- c("Higher-grade service class" = 1,
                            "Lower-grade service class" = 2,
                            "Small business owners" = 3,
                            "Skilled workers" = 4,
                            "Unskilled workers" = 5)
var_label(d$class5_r) <- "Respondent's Oesch class position - 5 classes"
freq(d$class5_r, total = T)


#######################################################################################
# Partner's Oesch class position
# Recode and create variables used to construct class variable for partners
# Variables used to construct class variable for partners: isco08p, emprelp
#######################################################################################

#### Recode occupation variable (isco88 com 4-digit) for partners


tail(freq(d$isco08p, total = T))

d$isco_partner <- d$isco08p
d$isco_partner[is.na(d$isco_partner)] <- -9
var_label(d$isco_partner) <- "Current occupation of partner - isco08 4-digit"

head(freq(d$isco_partner, total = T))

#### Recode employment status for partners

freq(d$emprelp, total = T)

d$selfem_partner <- NA
d$selfem_partner[d$emprelp == 1 | d$emprelp == 6 | d$emprelp == 7 | d$emprelp == 8 | d$emprelp == 9 | is.na(d$emprelp)] <- 1
d$selfem_partner[d$emprelp == 2 | d$emprelp == 3] <- 2
val_labels(d$selfem_partner) <- c("Not self-employed" = 1,
                                  "Self-employed" = 2)
var_label(d$selfem_partner) <- "Employment status for partners"
freq(d$selfem_partner, total = T)


############################################
# Create Oesch class schema for partners
############################################

d$class16_p <- -9

# Large employers (1)



# Self-employed professionals (2)

d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2000 & d$isco_partner <= 2162] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2164 & d$isco_partner <= 2165] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2200 & d$isco_partner <= 2212] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2250] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2261 & d$isco_partner <= 2262] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2300 & d$isco_partner <= 2330] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2350 & d$isco_partner <= 2352] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2359 & d$isco_partner <= 2432] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2500 & d$isco_partner <= 2619] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2621] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2630 & d$isco_partner <= 2634] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2636 & d$isco_partner <= 2640] <- 2
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2642 & d$isco_partner <= 2643] <- 2

# Small business owners with employees (3)



# Small business owners without employees (4)

d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 1000 & d$isco_partner <= 1439] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2163] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2166] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2220 & d$isco_partner <= 2240] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2260] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2263 & d$isco_partner <= 2269] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2340 & d$isco_partner <= 2342] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2353 & d$isco_partner <= 2356] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2433 & d$isco_partner <= 2434] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2620] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2622] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2635] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner == 2641] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 2650 & d$isco_partner <= 2659] <- 4
d$class16_p[d$selfem_partner == 2 & d$isco_partner >= 3000 & d$isco_partner <= 9629] <- 4

# Technical experts (5)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2100 & d$isco_partner <= 2162] <- 5
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2164 & d$isco_partner <= 2165] <- 5
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2500 & d$isco_partner <= 2529] <- 5


# Technicians (6)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3100 & d$isco_partner <= 3155] <- 6
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3210 & d$isco_partner <= 3214] <- 6
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3252] <- 6
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3500 & d$isco_partner <= 3522] <- 6

# Skilled manual (7)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 6000 & d$isco_partner <= 7549] <- 7
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 8310 & d$isco_partner <= 8312] <- 7
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 8330] <- 7
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 8332 & d$isco_partner <= 8340] <- 7
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 8342 & d$isco_partner <= 8344] <- 7

# Low-skilled manual (8)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 8000 & d$isco_partner <= 8300] <- 8
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 8320 & d$isco_partner <= 8321] <- 8
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 8341] <- 8
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 8350] <- 8
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 9200 & d$isco_partner <= 9334] <- 8
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 9600 & d$isco_partner <= 9620] <- 8
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 9622 & d$isco_partner <= 9629] <- 8

# Higher-grade managers and administrators (9)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 1000 & d$isco_partner <= 1300] <- 9
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 1320 & d$isco_partner <= 1349] <- 9
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2400 & d$isco_partner <= 2432] <- 9
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2610 & d$isco_partner <= 2619] <- 9
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2631] <- 9
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 100 & d$isco_partner <= 110] <- 9

# Lower-grade managers and administrators (10)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 1310 & d$isco_partner <= 1312] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 1400 & d$isco_partner <= 1439] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2433 & d$isco_partner <= 2434] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3300 & d$isco_partner <= 3339] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3343] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3350 & d$isco_partner <= 3359] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3411] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5221] <- 10
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 200 & d$isco_partner <= 210] <- 10

# Skilled clerks (11)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3340 & d$isco_partner <= 3342] <- 11
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3344] <- 11
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 4000 & d$isco_partner <= 4131] <- 11
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 4200 & d$isco_partner <= 4221] <- 11
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 4224 & d$isco_partner <= 4413] <- 11
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 4415 & d$isco_partner <= 4419] <- 11

# Unskilled clerks (12)

d$class16_p[d$selfem_partner == 1 & d$isco_partner == 4132] <- 12
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 4222] <- 12
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 4223] <- 12
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5230] <- 12
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 9621] <- 12

# Socio-cultural professionals (13)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2200 & d$isco_partner <= 2212] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2250] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2261 & d$isco_partner <= 2262] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2300 & d$isco_partner <= 2330] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2350 & d$isco_partner <= 2352] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2359] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2600] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2621] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2630] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2632 & d$isco_partner <= 2634] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2636 & d$isco_partner <= 2640] <- 13
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2642 & d$isco_partner <= 2643] <- 13

# Socio-cultural semi-professionals (14)

d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2163] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2166] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2220 & d$isco_partner <= 2240] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2260] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2263 & d$isco_partner <= 2269] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2340 & d$isco_partner <= 2342] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2353 & d$isco_partner <= 2356] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2620] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2622] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2635] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 2641] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 2650 & d$isco_partner <= 2659] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3200] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3220 & d$isco_partner <= 3230] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3250] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3253 & d$isco_partner <= 3257] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3259] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3400 & d$isco_partner <= 3410] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3412 & d$isco_partner <= 3413] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3430 & d$isco_partner <= 3433] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3435] <- 14
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 4414] <- 14

# Skilled service (15)

d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3240] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3251] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3258] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 3420 & d$isco_partner <= 3423] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 3434] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5000 & d$isco_partner <= 5120] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5140 & d$isco_partner <= 5142] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5163] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5165] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5200] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5220] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5222 & d$isco_partner <= 5223] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5241 & d$isco_partner <= 5242] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5300 & d$isco_partner <= 5321] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5400 & d$isco_partner <= 5413] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5419] <- 15
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 8331] <- 15

# Low-skilled service (16)

d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5130 & d$isco_partner <= 5132] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5150 & d$isco_partner <= 5162] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5164] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5169] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5210 & d$isco_partner <= 5212] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5240] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5243 & d$isco_partner <= 5249] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 5322 & d$isco_partner <= 5329] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 5414] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner == 8322] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 9100 & d$isco_partner <= 9129] <- 16
d$class16_p[d$selfem_partner == 1 & d$isco_partner >= 9400 & d$isco_partner <= 9520] <- 16


d$class16_p[d$class16_p == -9] <- NA
val_labels(d$class16_p) <- c("Large employers" = 1,
                             "Self-employed professionals" = 2,
                             "Small business owners with employees" = 3,
                             "Small business owners without employees" = 4,
                             "Technical experts" = 5,
                             "Technicians" = 6,
                             "Skilled manual" = 7,
                             "Low-skilled manual" = 8,
                             "Higher-grade managers and administrators" = 9,
                             "Lower-grade managers and administrators" = 10,
                             "Skilled clerks" = 11,
                             "Unskilled clerks" = 12,
                             "Socio-cultural professionals" = 13,
                             "Socio-cultural semi-professionals" = 14,
                             "Skilled service" = 15,
                             "Low-skilled service" = 16)
var_label(d$class16_p) <- "Partner's Oesch class position - 16 classes"
freq(d$class16_p, total = T)


d$class8_p <- NA
d$class8_p[d$class16_p <= 2] <- 1
d$class8_p[d$class16_p == 3 | d$class16_p == 4] <- 2
d$class8_p[d$class16_p == 5 | d$class16_p == 6] <- 3
d$class8_p[d$class16_p == 7 | d$class16_p == 8] <- 4
d$class8_p[d$class16_p == 9 | d$class16_p == 10] <- 5
d$class8_p[d$class16_p == 11 | d$class16_p == 12] <- 6
d$class8_p[d$class16_p == 13 | d$class16_p == 14] <- 7
d$class8_p[d$class16_p == 15 | d$class16_p == 16] <- 8
val_labels(d$class8_p) <- c("Self-employed professionals and large employers" = 1,
                            "Small business owners" = 2,
                            "Technical (semi-)professionals" = 3,
                            "Production workers" = 4,
                            "(Associate) managers" = 5,
                            "Clerks" = 6,
                            "Socio-cultural (semi-)professionals" = 7,
                            "Service workers" = 8)
var_label(d$class8_p) <- "Partner's Oesch class position - 8 classes"
freq(d$class8_p, total = T)


d$class5_p <- NA
d$class5_p[d$class16_p <= 2 | d$class16_p == 5 | d$class16_p == 9 | d$class16_p == 13] <- 1
d$class5_p[d$class16_p == 6 | d$class16_p == 10 | d$class16_p == 14] <- 2
d$class5_p[d$class16_p == 3 | d$class16_p == 4] <- 3
d$class5_p[d$class16_p == 7 | d$class16_p == 11 | d$class16_p == 15] <- 4
d$class5_p[d$class16_p == 8 | d$class16_p == 12 | d$class16_p == 16] <- 5
val_labels(d$class5_p) <- c("Higher-grade service class" = 1,
                            "Lower-grade service class" = 2,
                            "Small business owners" = 3,
                            "Skilled workers" = 4,
                            "Unskilled workers" = 5)
var_label(d$class5_p) <- "Partner's Oesch class position - 5 classes"
freq(d$class5_p, total = T)


####################################################################################################
# Final Oesch class position
# Merge two class variables (respondents and partners)
# Assign the partner's Oesch class position when the respondent's Oesch class position is missing:
####################################################################################################

d$class16 <- ifelse(!is.na(d$class16_r), d$class16_r, d$class16_p)

val_labels(d$class16) <- c("Large employers" = 1,
                           "Self-employed professionals" = 2,
                           "Small business owners with employees" = 3,
                           "Small business owners without employees" = 4,
                           "Technical experts" = 5,
                           "Technicians" = 6,
                           "Skilled manual" = 7,
                           "Low-skilled manual" = 8,
                           "Higher-grade managers and administrators" = 9,
                           "Lower-grade managers and administrators" = 10,
                           "Skilled clerks" = 11,
                           "Unskilled clerks" = 12,
                           "Socio-cultural professionals" = 13,
                           "Socio-cultural semi-professionals" = 14,
                           "Skilled service" = 15,
                           "Low-skilled service" = 16)
var_label(d$class16) <- "Final Oesch class position - 16 classes"
freq(d$class16, total = T)


d$class8 <- NA
d$class8[d$class16 <= 2] <- 1
d$class8[d$class16 == 3 | d$class16 == 4] <- 2
d$class8[d$class16 == 5 | d$class16 == 6] <- 3
d$class8[d$class16 == 7 | d$class16 == 8] <- 4
d$class8[d$class16 == 9 | d$class16 == 10] <- 5
d$class8[d$class16 == 11 | d$class16 == 12] <- 6
d$class8[d$class16 == 13 | d$class16 == 14] <- 7
d$class8[d$class16 == 15 | d$class16 == 16] <- 8
val_labels(d$class8) <- c("Self-employed professionals and large employers" = 1,
                          "Small business owners" = 2,
                          "Technical (semi-)professionals" = 3,
                          "Production workers" = 4,
                          "(Associate) managers" = 5,
                          "Clerks" = 6,
                          "Socio-cultural (semi-)professionals" = 7,
                          "Service workers" = 8)
var_label(d$class8) <- "Final Oesch class position - 8 classes"
freq(d$class8, total = T)


d$class5 <- NA
d$class5[d$class16 <= 2 | d$class16 == 5 | d$class16 == 9 | d$class16 == 13] <- 1
d$class5[d$class16 == 6 | d$class16 == 10 | d$class16 == 14] <- 2
d$class5[d$class16 == 3 | d$class16 == 4] <- 3
d$class5[d$class16 == 7 | d$class16 == 11 | d$class16 == 15] <- 4
d$class5[d$class16 == 8 | d$class16 == 12 | d$class16 == 16] <- 5
val_labels(d$class5) <- c("Higher-grade service class" = 1,
                          "Lower-grade service class" = 2,
                          "Small business owners" = 3,
                          "Skilled workers" = 4,
                          "Unskilled workers" = 5)
var_label(d$class5) <- "Final Oesch class position - 5 classes"
freq(d$class5, total = T)

d <- subset(d, select = -c(isco_mainjob, emplrel_r, emplno_r, selfem_mainjob, isco_partner, selfem_partner))


####################################################################################################
# Convert all labelled variables (haven_labelled class) to factors
# To convert a specific labelled variable to a factor: d$class16 <- to_factor(d$class16, drop_unused_labels = TRUE)
# The levels argument allows to specify what should be used as the factor levels, the labels (default), the values or the labels prefixed with values
# Example with the labels prefixed with values: d$class16 <- to_factor(d$class16, drop_unused_labels = TRUE, levels = "p")
####################################################################################################

d <-  unlabelled(d, drop_unused_labels = TRUE)
data_clean <- d
save(data_clean,file='data-clean/data-oesch.Rds')
##################################
# End
##################################
