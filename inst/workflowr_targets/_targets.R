library(targets)
library(tarchetypes)
library(ourproj)

# create dependency on development packages (see
# https://books.ropensci.org/targets/packages.html#package-based-invalidation)
tar_option_set(packages = c("ourproj"),
               imports = c("ourproj"))

# load global objects - constants (file paths)
source("_targets_config.R")

# End this file with a list of target objects.
list(
  # Files ----------------------------------------------------------

  ## Config files ------------------------------------------------

  tar_target(TARGETS_CONFIG,
             "_targets_config.R",
             format = "file"),

  ## Raw data ----------------------------------------------------------

  tar_track_input_files(),


  # Workflowr -------------------------------------------------------

  # target factory to render all workflowr Rmd files in analysis directory
  tar_render_workflowr(),

  # README ------------------------------------------------------------------
  tar_render(name = README_RMD,
             path = "README.Rmd")

)
