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
        devtools::install_github("hfgolino/EGAnet")
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

add_new_variables <- function(old_data) {
  if (!file.exists("data/data_complete.Rds")) {
    d <- add_lr_scale(old_data,3)
    d <- add_lr_scale(d,5)
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

rescale_data <- function(x) {
  return(x |> mutate(across(network_variables,transform_range_1)))
}


transform_range_1 <- function(x){
  return(2*((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))) - 1)
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

energy_total <- function(network_group_data,matrix) {
  
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


get_country_results <- function(data,network_variables,country,type) {
  library(psychonetrics)
  library(influential)
  library(igraph)
  library(psych)
  source("code/k-shell-influence.R")
  model_data <- data %>% select(c(network_variables,Country))
  model_data <- rescale_data(model_data)
  model_data <- model_data |> filter(!is.na(model_data$Country)) |> filter(Country == country)
  model_data <- model_data |> select(-Country)
  model_1 <- ggm(model_data ,omega = "empty")
  model_1 <- model_1 %>% runmodel
  model_1b <- model_1  %>%  stepup(
    criterion = "bic",
    alpha = 0.00001,
    greedy = FALSE,
    greedyadjust = "fdr"
  )
  
  delta_matrix <-  getmatrix(model_1b, "delta")
  deltas <- sum(diag(delta_matrix)) / nrow(delta_matrix)
  SEs <- model_1b@parameters$se[model_1b@parameters$matrix == "delta"]
  se = mean(SEs)
  df <- data.frame(
    temperature = deltas,
    lower = (deltas-qnorm(0.975) * se),
    upper = (deltas+qnorm(0.975) * se),
    stringsAsFactors = FALSE
  )
  
  matrix <- getmatrix(model_1b,matrix = "omega")
  graph <-  graph_from_adjacency_matrix(matrix,
                                        weighted = TRUE,
                                        mode = "undirected")
  V(graph)$label <- as.character(1:7)
  d <- as.data.frame(get_influence(graph))
  if (type=="inf"){
    return(d)
  } else {
  gic_mean = mean(unlist(d[1,1:7]))
  gi_mean = mean(unlist(d[3,1:7]))
  gsm_mean = mean(unlist(d[4,1:7]))
  gic_sd = sd(unlist(d[1,1:7]))
  gi_sd = sd(unlist(d[3,1:7]))
  gsm_sd = sd(unlist(d[4,1:7]))
  gic_k= kurtosi(unlist(d[1,1:7]))
  gi_k = kurtosi(unlist(d[3,1:7]))
  gsm_k = kurtosi(unlist(d[4,1:7]))
  res = matrix(0,7,7)
  model_data <- model_data |> drop_na()
  n = nrow(model_data)
  for (i in 1:n) {
    for (j in 1:6) {
      for (k in (j + 1):7) {
        res[j,k] = res[j,k] + matrix[j,k]*as.numeric(model_data[i,j])*as.numeric(model_data[i,k])
      }
    }
  }
  energy <- -sum(res[upper.tri(res)]/n)
  summary <- cbind(df[,1],gic_mean,gi_mean, gsm_mean,gic_sd,
                   gi_sd, gsm_sd, gic_k, gi_k, gsm_k,energy)
  colnames(summary) <-
    c(
      "Temperature",
      "GIC Mean",
      "GIC SD",
      "GIC Kurtosis",
      "GI Mean",
      "GI SD",
      "GI Kurtosis",
      "GSM Mean ",
      "GSM SD",
      "GSM Kurtosis",
      "Avg. Energy"
    )  
    return(summary)
  }
}

k.shell   <- function(graph, start.level = 0, verbose = FALSE){
  library(igraph)
  # E(graph)$weight <- 1/E(graph)$weight
  # E(graph)$weight <- E(graph)$weight / mean(E(graph)$weight)
  # norm            <- E(graph)$weight / min(E(graph)$weight)
  # rnorm           <- round(norm, digits = 0)
  # E(graph)$weight <- rnorm
  
  rnormalize      <- function(x){
    x               <- 1/x
    x               <- x/mean(x)
    x               <- x/min(x)
    x               <- round(x, digits = 0)
    x
  }
  
  E(graph)$weight <- rnormalize(E(graph)$weight)
  
  adj             <- get.adjacency(graph, attr = "weight", sparse = TRUE)
  
  level.down     <- function(x, level){
    g            <- x
    #gs           <- graph.strength(g)
    gs           <- Matrix::rowSums(g)
    
    while (any(gs <= level) & length(gs) > level) {
      delete      <- which(gs <= level)
      g            <- g[-delete, -delete]
      gs           <- Matrix::rowSums(g)  
    }
    setdiff(rownames(x), rownames(g))
  }
  
  g               <- adj
  k.score         <- 0
  k.vector        <- rep(Inf, vcount(graph)) 
  gs              <- Matrix::rowSums(adj)
  
  if (start.level < min(gs)) start.level <- min(gs)
  
  minimum.degree  <- start.level
  
  
  while (k.score <= minimum.degree & nrow(g) != 0) {
    candidate.names <- level.down(g, level = minimum.degree)
    candidates      <- which(V(graph)$name %in% candidate.names)
    
    k.score         <- k.score + 1
    delete          <- which(rownames(g) %in% candidate.names)
    g               <- g[-delete, -delete]
    
    if (nrow(g) == 0) break
    gs              <- Matrix::rowSums(g)
    
    if (minimum.degree >= min(gs)) break
    
    minimum.degree  <- min(gs)
    
    k.vector[candidates] <- k.score
    if (identical(verbose, TRUE)) cat("Minimum degree: ", minimum.degree, "Removed: ", length(candidate.names), "Remain: ", nrow(g), "\n")
  }
  
  k.vector[is.infinite(k.vector)] <- k.score 
  
  k.vector + start.level
}