library(targets)
library(tarchetypes)

if (!require(ourproj)) {
  devtools::install_github("rmgpanw/ourproj")
}

library(ourproj)

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

# End this file with a list of target objects.
list(
  # Files ----------------------------------------------------------

  ## Config files ------------------------------------------------

  tar_target(TARGETS_CONFIG,
             "_targets_config.R",
             format = "file"),

  ## Raw data ----------------------------------------------------------

  # # to track files, use 'format = "file"'
  # tar_target(
  #   RAW_DATA_CSV,
  #   file.path("data", "raw_data.csv"),
  #   format = "file"
  # ),
  #
  # # ...then read as a target
  # tar_target(
  #   raw_data,
  #   readr::read_csv(RAW_DATA_CSV)
  # ),

  # Analysis targets ----------------------------------------------------------

  tar_target(data, data.frame(x = sample.int(100), y = sample.int(100))),

  tar_target(summary, summ(data)),
  # Call your custom functions as needed.

  # Workflowr -------------------------------------------------------

  # target factory to render all workflowr Rmd files in analysis directory
  tar_render_workflowr(
    workflowr_dir = "analysis",
    output_dir = "public",
    target_prefix = "WORKFLOWR_"
  ),

  # Manuscript ------------------------------------------------------

  ## Main manuscript ------------------------------------------
  tar_render(
    MANUSCRIPT_RMD_HTML,
    "manuscript.Rmd",
    output_file =  file.path("public",
                             "manuscript.nb.html"),
    output_format = "bookdown::html_notebook2",
    quiet = FALSE
  ),

  tar_render(
    MANUSCRIPT_RMD_WORD,
    "manuscript.Rmd",
    output_file =  file.path("public",
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
    output_file = file.path("public",
                            "figures.html"),
    output_format = "rmarkdown::html_document",
    quiet = FALSE
  ),

  tar_render(
    MANUSCRIPT_FIGURES_PDF,
    "figures.Rmd",
    output_file = file.path("public",
                            "figures.pdf"),
    output_format = "bookdown::pdf_document2",
    quiet = FALSE
  ),

  ## Tables -----
  tar_render(
    MANUSCRIPT_TABLES_HTML,
    "tables.Rmd",
    output_file = file.path("public",
                            "tables.html"),
    output_format = "rmarkdown::html_document",
    quiet = FALSE
  ),

  tar_render(
    # all tables, no confidence intervals
    MANUSCRIPT_TABLES_DOCX,
    "tables.Rmd",
    output_file = file.path("public",
                            "tables.docx"),
    output_format = "bookdown::word_document2",
    quiet = FALSE
  ),

  # README ------------------------------------------------------------------
  tar_render(name = README_RMD,
             path = "README.Rmd")

)
