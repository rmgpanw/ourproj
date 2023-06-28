library(targets)
library(tarchetypes)
library(ourproj)

# create dependency on development packages (see
# https://books.ropensci.org/targets/packages.html#package-based-invalidation)
tar_option_set(packages = c("ourproj"),
               imports = c("ourproj"))

# load global objects - constants (file paths)
source("_targets_config.R")

# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
summ <- function(dataset) {
  summarize(dataset, mean_x = mean(x))
}

# Set target-specific options such as packages.
tar_option_set(packages = "dplyr")

# Determine whether analyses should be rendered to either `docs` (GitHub Pages)
# or `public` (GitLab Pages) directory
WORKFLOWR_OUT_DIR <- file.path("analysis", "_site.yml") %>%
  yaml::read_yaml() %>%
  .$output_dir %>%
  fs::path_file()

if (!dir.exists(WORKFLOWR_OUT_DIR)) {
  dir.create(WORKFLOWR_OUT_DIR)
}

# End this file with a list of target objects.
list(
  # Files ----------------------------------------------------------

  ## Config files ------------------------------------------------

  tar_target(TARGETS_CONFIG,
             "_targets_config.R",
             format = "file"),

  ## Raw data ----------------------------------------------------------

  tar_track_input_files(),

  # Analysis targets ----------------------------------------------------------

  tar_target(data, data.frame(x = sample.int(100), y = sample.int(100))),

  tar_target(summary, summ(data)),
  # Call your custom functions as needed.

  # Workflowr -------------------------------------------------------

  # target factory to render all workflowr Rmd files in analysis directory
  tar_render_workflowr(),

  # Manuscript ------------------------------------------------------

  ## Main manuscript ------------------------------------------
  tar_render(
    MANUSCRIPT_RMD_HTML,
    "manuscript.Rmd",
    output_file =  file.path(WORKFLOWR_OUT_DIR,
                             "manuscript.nb.html"),
    output_format = "bookdown::html_notebook2",
    quiet = FALSE
  ),

  tar_render(
    MANUSCRIPT_RMD_WORD,
    "manuscript.Rmd",
    output_file =  file.path(WORKFLOWR_OUT_DIR,
                             "manuscript.docx"),
    output_format = "bookdown::word_document2",
    quiet = FALSE
  ),

  ## Figure/table captions -------------------------------------------
  tar_target(FIGURE_TABLE_CAPTIONS_XLSX,
             {
               file_path <- "figure_table_captions.xlsx"

               readxl::read_excel(file_path)

               file_path
             },
             format = "file"),

  tar_target(figure_table_captions_raw,
             {
               result <- readxl::read_excel(FIGURE_TABLE_CAPTIONS_XLSX)

               # create category col
               result <-  result %>%
                 dplyr::mutate(category = paste(main_supplementary,
                                                figure_table,
                                                sep = "_"))

               # convert to list with 4 items (main/supplementary, fig/table)
               result <- split(result,
                               result$category) %>%
                 purrr::map(~ .x %>%
                              dplyr::select(-category) %>%
                              dplyr::arrange(number))

               # return result
               result
             }),

  tar_target(figure_table_captions,
             {
               # list of captions
               figure_table_captions <- list(
                 main_figure = captioner::captioner(prefix = "Figure", auto_space = TRUE),
                 main_table = captioner::captioner(prefix = "Table", auto_space = TRUE),
                 supplementary_figure = captioner::captioner(prefix = "sFigure", auto_space = TRUE),
                 supplementary_table = captioner::captioner(prefix = "sTable", auto_space = TRUE)
               )

               # populate
               for (caption_category in names(figure_table_captions_raw)) {
                 figure_table_captions_raw[[caption_category]] %>%
                   dplyr::select(name, caption) %>%
                   purrr::pmap(.f = figure_table_captions[[caption_category]])
               }

               # return result
               figure_table_captions
             }),

  ## Figures -----
  tar_render(
    MANUSCRIPT_FIGURES_HTML,
    "figures.Rmd",
    output_file = file.path(WORKFLOWR_OUT_DIR,
                            "figures.html"),
    output_format = "rmarkdown::html_document",
    quiet = FALSE
  ),

  tar_render(
    MANUSCRIPT_FIGURES_PDF,
    "figures.Rmd",
    output_file = file.path(WORKFLOWR_OUT_DIR,
                            "figures.pdf"),
    output_format = "bookdown::pdf_document2",
    quiet = FALSE
  ),

  ## Tables -----
  tar_render(
    MANUSCRIPT_TABLES_HTML,
    "tables.Rmd",
    output_file = file.path(WORKFLOWR_OUT_DIR,
                            "tables.html"),
    output_format = "rmarkdown::html_document",
    quiet = FALSE
  ),

  tar_render(
    # all tables, no confidence intervals
    MANUSCRIPT_TABLES_DOCX,
    "tables.Rmd",
    output_file = file.path(WORKFLOWR_OUT_DIR,
                            "tables.docx"),
    output_format = "bookdown::word_document2",
    quiet = FALSE
  ),

  # README ------------------------------------------------------------------
  tar_render(name = README_RMD,
             path = "README.Rmd")

)
