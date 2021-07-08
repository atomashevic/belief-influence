# Filename: 05-radar-plot.R
# This script:
#   - Creates radar plots showing IVI profile for 3 countries
#   - Creates radar plots showing IVI profile for 3 social classes
# Output:
#   - 'figures/03-radar-ger-*.png` three figures subsequently showing
#     radar plots for Germany, Hungary and Serbia
#   - 'figures/03-radar-class-*.png` three figures subsequently showing
#     radar plots for three social classes

library(fmsb)


colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                  rgb(0.8, 0.2, 0.5, 0.9) ,
                  rgb(0.7, 0.5, 0.1, 0.9))
colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
              rgb(0.8, 0.2, 0.5, 0.4) ,
              rgb(0.7, 0.5, 0.1, 0.4))

country_data <- as.data.frame(read_csv('csv/country-data.csv'))


## START WITH GERMANY

countries <- c('Germany')

sel_country <-
  country_data %>% filter(country_data$Country %in% countries)
rownames(sel_country) <- (countries)

sel_country <-
  sel_country %>% select(
    c(
      "PeopleAllow",
      "Parliament",
      "Economy",
      "Democracy" ,
      "Education",
      "Health"  ,
      "CitInterest"
    )
  )

data <- rbind(rep(100, 7) , rep(0, 7) , sel_country)
rownames(data) <- c('max', 'min', countries)

png(
  filename = 'figures/03-radar-ger.png',
  width = 2000,
  height = 2000,
  res = 300
)
radarchart(
  data,
  cglty = 5,
  pcol = colors_border ,
  pfcol = colors_in ,
  plwd = 4 ,
  plty = 1,
  cglcol = "grey",
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25),
  cglwd = 0.5,
  vlcex = 0.75
)

legend(
  x = 1,
  y = 0.6,
  legend = rownames(data[-c(1, 2), ]),
  bty = "n",
  pch = 20 ,
  col = colors_in ,
  text.col = "black",
  cex = 0.8,
  pt.cex = 1
)
dev.off()

## GERMANY + HUNGARY

countries <- c('Germany', 'Hungary')


sel_country <-
  country_data %>% filter(country_data$Country %in% countries)
rownames(sel_country) <- c(countries)

sel_country <-
  sel_country %>% select(
    c(
      "PeopleAllow",
      "Parliament",
      "Economy",
      "Democracy" ,
      "Education",
      "Health"  ,
      "CitInterest"
    )
  )

data <- rbind(rep(100, 7) , rep(0, 7) , sel_country)
rownames(data) <- c('max', 'min', countries)

png(
  filename = 'figures/03-radar-ger-hu.png',
  width = 2000,
  height = 2000,
  res = 300
)
radarchart(
  data,
  cglty = 5,
  pcol = colors_border ,
  pfcol = colors_in ,
  plwd = 4 ,
  plty = 1,
  cglcol = "grey",
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25),
  cglwd = 0.5,
  vlcex = 0.75
)

legend(
  x = 1,
  y = 0.6,
  legend = c('Germany', 'Hungrary'),
  bty = "n",
  pch = 20 ,
  col = colors_in ,
  text.col = "black",
  cex = 0.8,
  pt.cex = 1
)
dev.off()

### GERMANY, HUNGARY, SERBIA

countries <- c('Germany', 'Hungary', 'Serbia')

sel_country <-
  country_data %>% filter(country_data$Country %in% countries)
rownames(sel_country) <- c(countries)

sel_country <-
  sel_country %>% select(
    c(
      "PeopleAllow",
      "Parliament",
      "Economy",
      "Democracy" ,
      "Education",
      "Health"  ,
      "CitInterest"
    )
  )

data <- rbind(rep(100, 7) , rep(0, 7) , sel_country)
rownames(data) <- c('max', 'min', countries)

png(
  filename = 'figures/03-radar-hu-rs.png',
  width = 2000,
  height = 2000,
  res = 300
)
radarchart(
  data,
  cglty = 5,
  pcol = colors_border ,
  pfcol = colors_in ,
  plwd = 4 ,
  plty = 1,
  cglcol = "grey",
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25),
  cglwd = 0.5,
  vlcex = 0.75
)

legend(
  x = 1,
  y = 0.6,
  legend = c('Germany', 'Hungrary', 'Serbia'),
  bty = "n",
  pch = 20 ,
  col = colors_in ,
  text.col = "black",
  cex = 0.8,
  pt.cex = 1
)
dev.off()


## SOCIAL CLASS

class_data <- as.data.frame(read_csv('csv/ivi-class.csv'))
rownames(class_data) <- rownames(ivi_class)


# CLASS 1

c = c('Higher-grade service class')

sel_class <-
  class_data %>% filter(row.names(class_data) %in% c)

data <- rbind(rep(100, 7) , rep(0, 7) , sel_class)
rownames(data) <- c('max', 'min', c)

png(
  filename = 'figures/04-radar-class-1.png',
  width = 2000,
  height = 2000,
  res = 300
)
radarchart(
  data,
  cglty = 5,
  pcol = colors_border ,
  pfcol = colors_in ,
  plwd = 4 ,
  plty = 1,
  cglcol = "grey",
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25),
  cglwd = 0.5,
  vlcex = 0.75
)

legend(
  x = 0.92,
  y = 0.6,
  legend = c('Higher-grade service class'),
  pch = 20 ,
  col = colors_in ,
  text.col = "black",
  cex = 0.6,
  pt.cex = 1
)
dev.off()

# CLASS 1 AND 2

c = c('Self-employed professionals and large employers',
      'Small business owners')

sel_class <-
  class_data %>% filter(row.names(class_data) %in% c)

data <- rbind(rep(100, 7) , rep(0, 7) , sel_class)
rownames(data) <- c('max', 'min', c)

png(
  filename = 'figures/04-radar-class-2.png',
  width = 2000,
  height = 2000,
  res = 300
)
radarchart(
  data,
  cglty = 5,
  pcol = colors_border ,
  pfcol = colors_in ,
  plwd = 4 ,
  plty = 1,
  cglcol = "grey",
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25),
  cglwd = 0.5,
  vlcex = 0.75
)

legend(
  x = 0.92,
  y = 0.6,
  legend = c('Large employers', 'Small business owners
'),
  bty = "n",
  pch = 20 ,
  col = colors_in ,
  text.col = "black",
  cex = 0.6,
  pt.cex = 1
)
dev.off()


# CLASS 1,2 AND 3

c = c('Self-employed professionals and large employers',
      'Production workers',
      'Unskilled workers')

sel_class <-
  class_data %>% filter(row.names(class_data) %in% c)

data <- rbind(rep(100, 7) , rep(0, 7) , sel_class)
rownames(data) <- c('max', 'min', c)

png(
  filename = 'figures/04-radar-class-3.png',
  width = 2000,
  height = 2000,
  res = 300
)
radarchart(
  data,
  cglty = 5,
  pcol = colors_border ,
  pfcol = colors_in ,
  plwd = 4 ,
  plty = 1,
  cglcol = "grey",
  axislabcol = "grey",
  caxislabels = seq(0, 100, 25),
  cglwd = 0.5,
  vlcex = 0.75
)

legend(
  x = 0.92,
  y = 0.6,
  legend = c('Large employers', 'Prod. workers', 'Unskilled workers
'),
  bty = "n",
  pch = 20 ,
  col = colors_in ,
  text.col = "black",
  cex = 0.6,
  pt.cex = 1
)
dev.off()