install_dependencies <- function(dependencies_list) {
    # dependencies_list must include devtools and should not include EGAnet
    new_packages <- dependencies_list[
        !(dependencies_list %in% installed.packages()[, "Package"])
        ]
    if (length(new_packages) > 0) {
        install.packages(new_packages)
    } else {
       print("All Dependecies already installed!")
    }
    # EGAnet is an exception since it always has to be installed from GitHub
    if (!("EGAnet" %in% installed.packages()[, "Package"])) {
        devtools::install_github("EGAnet")
    }
}

load_ess_data <- function() {
  if (!file.exists("data/ESS9.Rds")) {
    return(download_ess_data())
    } else {
      return(readRDS("data/ESS9.Rds"))
    }
}

clean_data <- function(dirty_data, variables_of_interest,
            factor_variables, variable_names) {
    if (!file.exists("data/data_clean.Rds")) {
        d <- dirty_data
        d <- convert_factors_to_numeric(dirty_data, factor_variables)
        d <- select_variables(d, variables_of_interest)
        d <- rename_variables(d, variable_names)
        d$Country <- labelled::to_factor(d$Country, levels = 'l')
        saveRDS(d, "data/data_clean.Rds")
        return(d)
    } else {
      return(readRDS("data/data_clean.Rds"))
    }
}

add_new_variables <- function(old_data, csv_location) {
  if (!file.exists("data/data_complete.Rds")) {
    d <- add_lr_scale(old_data,3)
    d <- add_lr_scale(d,5)
    external_data <- read.csv(csv_location)
    n <- nrow(d)
    d <- d |>
      add_column(Geff = NA, Rol = NA, Vaa = NA, Pstab = NA, ROW = NA)
    for (i in 1:n) {
      c <- d$Country[i]
      j <- which(external_data$Country == c)
      d$Geff[i] <- external_data$GEff[j]
      d$Rol[i] <- external_data$RoL[j]
      d$Vaa[i] <- external_data$VaA[j]
      d$Pstab[i] <- external_data$Pstab[j]
      d$ROW[i] <- external_data$ROW[j]
    }
    saveRDS(d, "data/data_complete.Rds")
    return(d)
  } else {
    return(readRDS("data/data_complete.Rds"))
  }
}

add_lr_scale <- function(data,number_of_levels){
  d <- tidyr::tibble(data,.name_repair = "minimal")
  if (number_of_levels == 3)
  {
    d <- d |>
      add_column(lr3 = NA)
    for (i in 1:nrow(d)) {
      d$lr3[i] = lr_transform_3(d$lrscale[i])
    }
  } else {
    d <- d |>
      add_column(lr5 = NA)
    for (i in 1:nrow(d)) {
      d$lr5[i] = lr_transform_5(d$lrscale[i])
    } 
  }
  return(d)
}

lr_transform_3 <- function(lrscale_value) {
  if (!is.na(lrscale_value)) {
    if (lrscale_value < 4) {
      return("L")
    } else if (lrscale_value < 7) {
      return("C")
    } else return("R")
  } else return(NA)
}

lr_transform_5 <- function(lrscale_value) {
  if (!is.na(lrscale_value)) {
    if (lrscale_value < 2) {
      return("FL")
    } else if (lrscale_value < 4) {
      return("L")
    } else if (lrscale_value < 7) {
      return("C")
    } else if (lrscale_value < 9) {
      return("R")
    } else return("FR")
  } else return(NA)  
}


remove_redundant_variables <- function(network_data) {
  if (!file.exists("data/variables_after_UVA.Rds")) {
    sample_size <- nrow(network_data)
    print("Performing 1st UVA round!")
    first_uva <-
      UVA(
        network_data ,
        n = sample_size,
        model = "glasso",
        corr = "cor_auto",
        method = "wTO",
        reduce = TRUE,
        reduce.method = "remove",
        adhoc = TRUE,
      )
    redundant_pairs_0 <- as.data.frame(
                        first_uva$redundancy$descriptives$centralTendency[,2])
    colnames(redundant_pairs_0) <- c("wTO")
    print("Redundant pairs before First round of UVA:")
    print(redundant_pairs_0 |>  filter(wTO > 0.25))
    redundant_pairs_1 <- as.data.frame(
                        first_uva$adhoc$descriptives$centralTendency[,1])
    colnames(redundant_pairs_1) <- c("wTO")
    print("Redundant pairs after First round of UVA:")
    print(redundant_pairs_1 |> filter(wTO > 0.25))
    second_uva <-
      UVA(
        first_uva$reduced$data,
        n = nrow(first_uva$reduced$data),
        model = "glasso",
        corr = "cor_auto",
        method = "wTO",
        reduce = TRUE,
        reduce.method = "remove",
        adhoc = TRUE,
      )
    redundant_pairs_2 <- as.data.frame(
      second_uva$adhoc$descriptives$centralTendency[,1])
    colnames(redundant_pairs_2) <- c("wTO")
    print("Redundant pairs after Second round of UVA:")
    print(redundant_pairs_2 |> filter(wTO > 0.25))
    saveRDS(colnames(second_uva$reduced$data),"data/variables_after_UVA.Rds")
    return(colnames(second_uva$reduced$data))
  } else {
    return(readRDS("data/variables_after_UVA.Rds"))
    }
}

ivi_radar_plot <- function(ivi_group_data,groups,plot_filename) {
  library(fmsb)
  selected_groups <- subset(ivi_group_data,rownames(ivi_group_data) %in% groups)
  d <- rbind(rep(100, ncol(selected_groups)) , rep(0, ncol(selected_groups)) , selected_groups )
  rownames(d) <- c('max', 'min', groups)
  png(
    filename = plot_filename,
    width = 2000,
    height = 2000,
    res = 300
  )
  radarchart(
    d,
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
    legend = rownames(d[-c(1, 2), ]),
    bty = "n",
    pch = 20 ,
    col = colors_in ,
    text.col = "black",
    cex = 0.8,
    pt.cex = 1
  )
  dev.off()
}

ivi_group_discrete <- function(network_data, group_variable)
{
  columns = ncol(network_data)
  factor_levels <- levels(group_variable)
  result_matrix = matrix(0, length(factor_levels), columns)
  for (i in 1:length(factor_levels)) {
    temp_data = network_data[which(group_variable == factor_levels[i]),]
    temp_ivi <- get_ivi(temp_data)
    labels = rownames(as.data.frame(temp_ivi))
    result_matrix[i,] = temp_ivi
  }
  result_matrix = as.data.frame(result_matrix)
  colnames(result_matrix) <- labels
  rownames(result_matrix) <- factor_levels
  return(result_matrix)
}


get_ivi <- function(network_data) {
  library(influential)
  library(igraph)
  ega_network <- EGA(network_data,
                     model = 'glasso',
                     algorithm = 'louvain',
                     plot.EGA = FALSE)
  graph <-  graph_from_adjacency_matrix(ega_network$network,
                                    weighted = TRUE,
                                    mode = "undirected")
  graph_vertices <- V(graph)
  
  estimated_ivi <- ivi(
    graph = graph,
    vertices = graph_vertices,
    directed = FALSE,
    scaled = TRUE,
    mode = 'all',
    weights = abs(E(graph)$weight),
    d = 1 # Small diameter due to small network size
  )
  return(estimated_ivi)
}


plot_ega_network <- function(network_data, plot_arguments,plot_filename){
  ega_network <- EGA(network_data,
                     model = 'glasso',
                     algorithm = 'louvain',
                     plot.EGA = FALSE)
  png(
    filename = plot_filename,
    width = 2750,
    height = 2750,
    res = 300
  )
    plot(ega_network, plot.args = plotargs)
  dev.off()
}

plot_network_tree_comparison <- function(net_tree,id1,id2,plot_filename){
  png(
    filename = plot_filename,
    width = 3500,
    height = 2000,
    res = 300
  )
    comparetree(
      net_tree,
      id1 = id1,
      id2 = id2,
      highlights = 3,
      plot = TRUE,
      labels = network_variables
    )
  dev.off()
}

country_scatterplot <- function(country_data,IVI_name,variable_2_name){
  library(ggplot2)
  png(
    filename = paste('figures/scatter-',IVI_name,'-',variable_2_name,'.png',sep = ''),
    width = 2000,
    height = 1500,
    res = 300
  )
  plot <-  ggplot(country_data, aes_string(x = variable_2_name, y = IVI_name)) +
      geom_point(
        color = "black",
        fill = "#69b3a2",
        shape = 1,
        alpha = 0.5,
        size = 2,
        stroke = 1
      ) + geom_smooth(method = "lm", se = FALSE, col = "purple") + xlab(variable_2_name) +
      theme_minimal() + ylab(paste(IVI_name, 'IVI'))
  show(plot)
  dev.off()
}

download_ess_data <- function() {
  library(essurvey)
  essurvey::set_email("atomashevic@ff.uns.ac.rs")
  temp_data <- essurvey::import_rounds(c(9), format = "spss")
  data <- sjlabelled::unlabel(temp_data)
  data$cntry <- temp_data$cntry
  saveRDS(data, file = "data/ESS9.Rds")
  return(data)
}

convert_factors_to_numeric <- function(data, factor_variables) {
    data[factor_variables] <- factors_numeric(data[factor_variables])
    return(data)
}

select_variables <- function(data, variables_of_interest) {
  return(data |> dplyr::select(variables_of_interest))
}

rename_variables <- function(data, variable_names) {
  colnames(data) <- variable_names
  return(data)
}

# Source: https://stackoverflow.com/questions/8596466/r-change-all-columns-of-type-factor-to-numeric/8598410

as_numeric <- function(x) as.numeric(x)

factors_numeric <- function(d) {
  modifyList(d, lapply(
    d[, sapply(d, is.factor)],
    as_numeric
  ))
}
