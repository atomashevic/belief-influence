# Measuring the influence of beliefs in belief networks

*ArXiv*: [https://arxiv.org/abs/2110.09154](https://arxiv.org/abs/2110.09154)

## Abstract

> Influential beliefs are crucial for our understanding of how people reason about political issues and make political decisions. This research proposes a new method for measuring the influence of political beliefs within larger context of belief system networks, based on the advances in psychometric network methods and network influence research. Using the latest round of the European Social Survey data, we demonstrate this approach on a belief network expressing support for the regime in 29 European countries and capturing beliefs related to support for regime performance, principles, institutions, and political actors. Our results show that the average influence of beliefs can be related to the consistency and connectivity of the belief network and that the influence of specific beliefs (e.g. Satisfaction with Democracy) on a country level has a significant negative correlation with external indicators from the same domain (e.g. Liberal Democracy index), which suggests that highly influential beliefs are related to pressing political issues. These findings suggest that network-based belief influence metrics estimated from large-scale survey data can be used a new type of indicator in comparative political research, which opens new avenues for integrating psychometric network analysis methods into political science methodology.

## Repository Content

-   `code/` `R` scripts and notebooks with all results presented in the paper
-   `figures/` and `data` store the latest versions of all figures and results
  
## Scripts and Notebooks
_Helper functions_
- `functions.R` contains the majority of data processing and cleaning functions
- `k-shell-influence.R` contains new implementation of k-shell algoritm as well as implementations of GIC and GSM in R using 'igraph' package

_Analysis scripts/notebooks (running order):_

1. `data-wrangling.R` Loads, cleans and prepares the data neccesary to estimate belief networks.
2. `bn-lrscale.Rmd`  Notebook containing the entire analysis of belief networks on ideology (Left-Right) based groups
3. `bn-polintr.Rmd' Notebook containing the entire analysis of belief networks on groups based on interest in politics
4. `bn-nwspol.Rmd` Notebook containing the entire analysis of belief networks on groups based on time spent following news about politics.
5. `country-results.R` Script containing the country-level analysis: estimation of belief network for each country in ESS sample.

_Figures and Tables:_
- `figure-01.R`
- `figure-02.R`
- `figure-03.R`
- `figure-04.R`
- `tables.R`
