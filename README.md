# Detecting influential beliefs in large-scale surveys

Repository containing data, code and results of the analysis for the paper **Detecting influential beliefs in large-scale surveys** presented at Networks2021 conference.

## Repository Content

-   Extended Slide deck [PDF](/deck/deck.pdf)
-   `code/` `R` scripts (analysis and functions in separate files)
-   `figures/` and `data` store the latest versions of all figures and results
-   Preprint *coming soon*
  
## Main R script

You can run clone the repository and run (from CLI or RStudio):

```bash
Rscript code/analysis.R
```

And the script will do the entire analysis: install packages (but won't update existing versions), download ESS data and produce the output in `data` and `figures` directories.

Analysis was originally done on R version 4.1.0 (2021-05-18).


## TODO:

-   [ ] Alternative estimation method `psychonetrics` approach

-   [ ] Estimate average temperature for each subsample group. Compare IVI, IVI variance and avg. temperature for subgroups (e.g. left-right X Country subgroups \~ 150 datapoints)

