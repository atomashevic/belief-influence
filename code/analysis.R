dependencies <-
    c(
        "here",
        "essurvey",
        "foreign",
        "qgraph",
        "dplyr",
        "tidyverse",
        "questionr",
        "labelled",
        "sjlabelled",
        "networktree",
        "igraph",
        "influential",
        "fmsb",
        "devtools",
        "ggplot2"
    )

colors_border = c(rgb(0.2, 0.5, 0.5, 0.9),
                  rgb(0.8, 0.2, 0.5, 0.9) ,
                  rgb(0.7, 0.5, 0.1, 0.9))
colors_in = c(rgb(0.2, 0.5, 0.5, 0.4),
              rgb(0.8, 0.2, 0.5, 0.4) ,
              rgb(0.7, 0.5, 0.1, 0.4))

plotargs = list(
    vsize = 25,
    alpha = 0.9,
    label.size = 3.5,
    edge.alpha = 0.8,
    legend.names = c(""),
    edge.color = "black",
    legend.size = 0
)


variables_of_interest <- c(
    "psppsgva", "psppipla", "trstprl", "trstplt",
    "trstprt", "trstlgl", "trstplc",
    "stfeco", "stfgov", "stfdem",
    "stfedu", "stfhlth", "frprtpl",
    "gvintcz", "poltran", "gndr",
    "agea", "cntry", "rlgblg",
    "eduyrs","lrscale"
)

factor_variables <- c(
    "psppsgva", "psppipla", "trstprl",
    "trstlgl", "trstplc", "trstplt",
    "trstprt", "stfeco", "stfgov",
    "stfdem", "stfedu", "stfhlth",
    "frprtpl", "gvintcz", "poltran","lrscale"
)


network_variables <-
    c(  "PeopleAllow", "PeopleInfluence", "Parliament",
        "Legal", "Police", "Politicians",
        "Parties", "Economy", "Democracy",
        "Education", "Health", "FairChance",
        "CitInterest", "Transparent")


variable_names <- c(
    "PeopleAllow", "PeopleInfluence", "Parliament",
    "Legal", "Police", "Politicians", "Parties",
    "Economy", "Government", "Democracy",
    "Education", "Health", "FairChance",
    "CitInterest", "Transparent", "Gender",
    "Age", "Country", "ReligionBelong",
    "Education","lrscale"
)


library(here)

dir.create("figures", showWarnings = FALSE)

dir.create("data", showWarnings = FALSE)

source("code/functions.R")

install_dependencies(dependencies)

data <- load_ess_data()


library(dplyr)
library(tidyverse)
library(tibble)

data <- clean_data(
         data,
         variables_of_interest,
         factor_variables,
         variable_names)

data <- add_new_variables(data, "csv/external-data.csv")


library(EGAnet)

network_data <- data %>% select(network_variables)

network_variables <- remove_redundant_variables(network_data)

network_data <- data %>% select(network_variables)


plot_ega_network(network_data, plotargs, "figures/ega_network.png")

library("networktree")

regime_type <- as.factor(data$ROW)

levels(regime_type) <- c("EA", "ED", "LD")
# "EA" - Electoral Autocracy
# "ED" - Electoral Democracy
# "LD" - Liberal Democracy

network_tree_regime_type <- networktree(
    nodevars = network_data, splitvars = regime_type, transform = "glasso", method = "mob")

plot_network_tree_comparison(network_tree_regime_type,
                             id1 = 3, #Electoral Autocracies
                             id2 = 5, #Liberal Democracies
                             "figures/EA_vs_LD_comparison.png")

left_right_3 <- as.factor(data$lr3)

network_tree_left_right_3 <- networktree(
    nodevars = network_data, splitvars = left_right_3, transform = "glasso", method = "mob")

plot_network_tree_comparison(network_tree_left_right_3,
                             id1 = 2, #Left
                             id2 = 3, #Center and Right
                             "figures/L3_vs_CR3_comparison.png")

left_right_5 <- as.factor(data$lr5)

network_tree_left_right_5 <- networktree(
    nodevars = network_data, splitvars = left_right_5, transform = "glasso", method = "mob")

plot_network_tree_comparison(network_tree_left_right_5,
                             id1 = 3, #FarLeft
                             id2 = 5, #FarRight
                             "figures/FL_vs_FR_comparison.png")

ivi_ROW_data <- ivi_group_discrete(network_data,regime_type)

ivi_radar_plot(ivi_ROW_data,c("EA"),"figures/ivi-row-ea.png")
ivi_radar_plot(ivi_ROW_data,c("EA","ED"),"figures/ivi-row-ea+ed.png")
ivi_radar_plot(ivi_ROW_data,c("EA","ED","LD"),"figures/ivi-row-ea+ed+ld.png")

left_right <- as.factor(left_right_3)

ivi_lr_data <- ivi_group_discrete(network_data,left_right)

ivi_radar_plot(ivi_lr_data ,c("L"),"figures/ivi-lr-l.png")
ivi_radar_plot(ivi_lr_data ,c("L","C"),"figures/ivi-lr-l+c.png")
ivi_radar_plot(ivi_lr_data ,c("L","C","R"),"figures/ivi-lr+l+c+r.png")

left_right_5 <- as.factor(left_right_5)

ivi_lr_5_data <- ivi_group_discrete(network_data,left_right_5)

countries <- as.factor(data$Country)


ivi_country_data <- ivi_group_discrete(network_data,countries)
ivi_country_data <- ivi_country_data[order(row.names(ivi_country_data)),]

country_data <- as.data.frame(read_csv('csv/country-data.csv'))
country_data <- country_data[order(country_data$Country),]

country_data <- cbind(country_data,ivi_country_data)

country_scatterplot(country_data,'Economy','GDP')
cor.test(country_data$Economy,country_data$GDP)
# r = -0.429
# p = 0.02

country_scatterplot(country_data,'Health','HCI')
cor.test(country_data$Health,country_data$HCI)
# r = 0.215
# p = 0.2606

country_scatterplot(country_data,'Democracy','ED')
cor.test(country_data$Democracy,country_data$ED)
# r = -0.138
# p = 0.47

country_scatterplot(country_data,'Parliament','RoL')
cor.test(country_data$Parliament,country_data$RoL)
# r = -0.238
# p = 0.212

