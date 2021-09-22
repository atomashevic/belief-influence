dependencies <-
    c(
        "here",
        "essurvey",
        "foreign",
        "qgraph",
        "psychonetrics",
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
        "ggplot2",
        "gtools"
    )


variables_of_interest <- c(
    "psppsgva", "psppipla", "trstprl", "trstplt",
    "trstprt", "trstlgl", "trstplc",
    "stfeco", "stfgov", "stfdem",
    "stfedu", "stfhlth", "frprtpl",
    "gvintcz", "poltran", "gndr",
    "agea", "cntry", "rlgblg",
    "eduyrs","lrscale","polintr","nwspol"
)

factor_variables <- c(
    "psppsgva", "psppipla", "trstprl",
    "trstlgl", "trstplc", "trstplt",
    "trstprt", "stfeco", "stfgov",
    "stfdem", "stfedu", "stfhlth",
    "frprtpl", "gvintcz", "poltran","lrscale","polintr"
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
    "Education","lrscale","polintr","nwspol"
)


library(here)

dir.create("figures", showWarnings = FALSE)

dir.create("data", showWarnings = FALSE)

source(paste(here(),"/code/functions.R",sep = ""))

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

data <- add_new_variables(data)

library(gtools)
data$nwspol <- quantcut(data$nwspol)
levels(data$nwspol) <- c("0-30","30-60","60-90","90+")

network_data <- data %>% select(network_variables)

network_variables <- remove_redundant_variables(network_data)

network_data <- data %>% select(network_variables)
