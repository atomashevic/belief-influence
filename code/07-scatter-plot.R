library(ggplot2)
library(hrbrthemes)
library(foreign)
library(tidyverse)
country_data <- as.data.frame(read_csv('output/country-data-v2.csv'))

## FIRST ONE

data <- country_data %>% select('Economy', 'GDP')

data <- data %>%  filter((Economy < 100) & (Economy >= 10))

png(
  filename = 'figures/05-scatter-eco.png',
  width = 2000,
  height = 1500,
  res = 300
)
ggplot(data, aes(x = GDP, y = Economy)) +
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 1,
    alpha = 0.5,
    size = 2,
    stroke = 1
  ) + geom_smooth(method = "lm", se = FALSE, col = "purple") + xlab('GDP per capita') +
  theme_minimal() + ylab('Economy IVI')
dev.off()

#cor(data$Economy,data$GDP)
#Correlation -0.4605517


# SECOND ONE

data <- country_data %>% select('Health', 'HCI')

data <- data %>%  filter((Health< 100) & (Health >= 10))

#cor(data$Health,data$HCI)
# r = -0.05
# p = 0.87


png(
  filename = 'figures/05-scatter-health.png',
  width = 2000,
  height = 1500,
  res = 300
)
ggplot(data, aes(x = HCI, y = Health)) +
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 1,
    alpha = 0.5,
    size = 2,
    stroke = 1
  ) + geom_smooth(method = "lm", se = FALSE, col = "purple") + xlab('HCI') +
  theme_minimal() + ylab('Health IVI') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()


# THIRD ONE

data <- country_data %>% select('Democracy', 'ED')


data <- data %>%  filter((Democracy< 100) & (Democracy >= 10))

png(
  filename = 'figures/05-scatter-ED.png',
  width = 2000,
  height = 1500,
  res = 300
)
ggplot(data, aes(x = ED, y = Democracy)) +
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 1,
    alpha = 0.5,
    size = 2,
    stroke = 1
  ) + geom_smooth(method = "lm", se = FALSE, col = "purple") + xlab('Electoral Democracy Score') +
  theme_minimal() + ylab('Democracy IVI')  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()


#cor(data$Democracy,data$ED)
# r = -0.293
# p = 0.14

data <- country_data %>% select('Democracy', 'DIRank')

data <- data %>%  filter((Democracy< 100) & (Democracy >= 10))

png(
  filename = 'figures/05-scatter-DI.png',
  width = 2000,
  height = 1500,
  res = 300
)
ggplot(data, aes(x = DIRank, y = Democracy)) +
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 1,
    alpha = 0.5,
    size = 2,
    stroke = 1
  ) + geom_smooth(method = "lm", se = FALSE, col = "purple") + xlab('Democracy Index Ranking') +
  theme_minimal() + ylab('Democracy IVI') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

# cor(data$Democracy,data$DIRank)
# r = 0.255
# p = 0.2


data <- country_data %>% select('Parliament', 'RoL')


data <- data %>%  filter((Parliament< 100) & (Parliament >= 10))

png(
  filename = 'figures/05-scatter-RoL.png',
  width = 2000,
  height = 1500,
  res = 300
)
ggplot(data, aes(x = RoL, y = Parliament)) +
  geom_point(
    color = "black",
    fill = "#69b3a2",
    shape = 1,
    alpha = 0.5,
    size = 2,
    stroke = 1
  ) + geom_smooth(method = "lm", se = FALSE, col = "purple") + xlab('Rule of Law') +
  theme_minimal() + ylab('Pairlament IVI') + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()

cor(data$Parliament,data$RoL)
# r = -0.221
# p = 0.27