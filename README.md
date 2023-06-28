
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ourproj

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmgpanw/ourproj/actions/workflows/check-standard.yml/badge.svg)](https://github.com/rmgpanw/ourproj/actions/workflows/check-standard.yml)
[![pkgdown](https://github.com/rmgpanw/ourproj/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/rmgpanw/ourproj/actions/workflows/pkgdown.yaml)
[![mirror-template-repos](https://github.com/rmgpanw/ourproj/actions/workflows/mirror-template-repos.yml/badge.svg)](https://github.com/rmgpanw/ourproj/actions/workflows/mirror-template-repos.yml)
<!-- badges: end -->

The goal of ourproj is to provide a data science project template that
combines [workflowr](https://workflowr.github.io/workflowr/) or
[quarto](https://quarto.org/) with
[targets](https://books.ropensci.org/targets/). Analyses are organised
in research website, which can be viewed locally and also easily
deployed to either [GitHub Pages](https://pages.github.com/) or [GitLab
Pages](https://docs.gitlab.com/ee/user/project/pages/).

## Installation

You can install the development version of ourproj from
[GitHub](https://github.com/rmgpanw/ourproj) with:

``` r
# install.packages("devtools")
devtools::install_github("rmgpanw/ourproj")
```

## Quick start

- Create a new project either from the [RStudio new project
  wizard](https://r4ds.had.co.nz/workflow-projects.html#rstudio-projects)
  or from the R console with `ourproj::ourproj_start()`.
- Install required R packages with `renv::init()`.
- Run targets pipeline with `targets::tar_make()`. This will generate a
  [workflowr](https://workflowr.github.io/workflowr/) research website
  in either the `docs` or `public` directory (for GitHub and GitLab
  Pages respectively), which may be viewed locally by double clicking on
  `index.html` in your file browser.

``` r
library(ourproj)
# create a new project
ourproj_start(directory = "~/NEW_PROJECT_DIR",
              project_title = "My project", 
              git_username = "My git username", 
              github_gitlab = "github", 
              template = "workflowr_targets_minimal")

# use renv to install required R packages
renv::init()

# run targets pipeline
targets::tar_make()
```

- Add your own workflowr rmarkdown analyses in the `analysis` folder (a
  template is provided, available from the [RStudio new rmarkdown
  wizard](https://rstudio.github.io/rstudio-extensions/rmarkdown_templates.html)).
  These will be automatically tracked by the targets pipeline.
- Re-run your analysis pipeline with `targets::tar_make()`.

## Example repositories

- [`workflowr_targets_minimal`](https://rmgpanw.github.io/workflowr_targets_minimal/)
- [`workflowr_targets`](https://rmgpanw.github.io/workflowr_targets/)
- [`quarto_website_targets`](https://rmgpanw.github.io/quarto_website_targets/)
