library(ggplot2)
library(hrbrthemes)
country_data <- as.data.frame(read_csv('output/country-data.csv'))

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

data <- country_data %>% select('Health', 'HCI')

#data <- data %>%  filter((Health <= 90) & (Health > 40))

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
  theme_minimal() + ylab('Health IVI')
dev.off()
cor(data$Economy,data$GDP)
#Correlation -0.4605517