
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

A data science project built with
[workflowr](https://workflowr.github.io/workflowr/index.html) and
[targets](https://books.ropensci.org/targets/).

# Setup

  - Create an R script called `_targets_config.R` in the project root
    directory with the following objects and populate:

<!-- end list -->

    # Config file for targets pipeline (not tracked by git). Defines global
    # objects that may differ between local instances of a project.
    
    # File paths ---------------------------------------------------------------
    
    # input files
    
    ## My file - typically may be located outside this project, but can be local
    ## also
    MY_FILE_IN_PATH <-

  - Install required R packages, ideally using
    [renv](https://rstudio.github.io/renv/) with `renv::init()`, or
    `renv::restore()` if a `renv.lock` file is already present.
  - Run [targets pipeline](https://books.ropensci.org/targets/) with
    `tar_make()` in the R console, or using the ‘Build’ tab in RStudio.
