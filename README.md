
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ourproj

<!-- badges: start -->
<!-- badges: end -->

The goal of ourproj is to provide a data science project template that
combines [workflowr](https://workflowr.github.io/workflowr/index.html)
with [targets](https://books.ropensci.org/targets/).

## Installation

You can install the development version of ourproj from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rmgpanw/ourproj")
```

## Getting started

Create a new project either from the RStudio menu or from the R console:

``` r
library(ourproj)
## create a new project
ourproj_start(directory = "~/NEW_PROJECT_DIR",
              project_name = "My project",
              git_username = "My Gitlab username")
```

The `_targets.R` script will automatically track any new R Markdown
notebooks added to the analysis directory in your new project.
