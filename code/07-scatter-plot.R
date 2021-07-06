# Filename: 07-scatter-plot.R
# This script:
#   - Plots scatter plot Economy IVI x GDP per capita
#   - Plots scatter plot Health IVI x Health Consumer Index
#   - Plots scatter plot Democracy IVI x Electoral Democracy Score
#   - Plots scatter plot Parliament IVI x Rule of Law Score
# Output:
#   - 'figures/05-scatter-eco.png' 
#   - 'figures/05-scatter-health.png' 
#   - 'figures/05-scatter-ED.png' 
#   - 'figures/05-scatter-RoL.png' 


library(ggplot2)

country_data <- as.data.frame(read_csv('output/country-data.csv'))

## FIRST ONE

data <- country_data %>% select('Economy', 'GDP')

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


data <- country_data %>% select('Health', 'HCI')

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



data <- country_data %>% select('Democracy', 'ED')

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


data <- country_data %>% select('Parliament', 'RoL')

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
