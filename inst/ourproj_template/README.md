
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

Project to facilitate phenotype extraction from the UK Biobank dataset.
Please see [project
website](https://rmgpanw.gitlab.io/ukb_extract_phenotypes) for further
details.

# Setup

-   Create an R script called `_targets_config.R` in the project root
    directory with the following objects and populate:

<!-- -->

    # Config file for targets pipeline (not tracked by git). Defines global
    # objects that may differ between local instances of a project.

    # File paths ---------------------------------------------------------------

    # input files

    ## My file - typically may be located outside this project, but can be local
    ## also
    MY_FILE <-

-   Install required R packages with `renv::restore()`
-   Run [targets pipeline](https://books.ropensci.org/targets/) with
    `tar_make()` in the R console, or using the ‘Build’ tab in RStudio.
