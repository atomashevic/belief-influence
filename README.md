# Detecting influential beliefs in large-scale surveys

Repository containing data, code and results of the analysis for the paper **Detecting influential beliefs in large-scale surveys** presented at Networks2021 conference.

## Repository Content

-   Extended Slide deck [PDF](/deck/deck.pdf)
-   `code/` `R` scripts for each segment of the analysis and the main script
-   `figures/` and `csv` store the latest versions of all figures and results
-   Preprint *coming soon*

## Main R script

You can run clone the repository and run (from CLI or RStudio):

```bash
Rscript code/analysis
```

And the script will do the entire analysis: install packages (but won't update existing versions), download ESS data and produce the output in `csv` and `figures` directories.

Analysis was originally done on R version 4.1.0 (2021-05-18).


## TODO:

-   [ ] Add Ising model besides GGM

-   [ ] Estimate average temperature for each subsample group. Compare IVI, IVI variance and avg. temperature for subgroups (e.g. class X Country subgrupus \~ 150 datapoints)

-   [ ] Add ruling/opposition variable for each respondent who voted. Run entire analysis again for this variable X country.
