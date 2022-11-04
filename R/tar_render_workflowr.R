
# PUBLIC ------------------------------------------------------------------

# Target factories --------------------------------------------------------

# for guidance, see https://wlandau.github.io/targetopia/contributing.html

#' Target factory for workflowr R markdown reports.
#'
#' Generates a list of target objects to render multiple workflowr R markdown
#' reports.
#'
#' @param output_dir Directory where html report files will be rendered to. By
#'   default this is 'public'.
#' @param workflowr_dir Directory where workflowr Rmd files are located. By
#'   default this is 'analysis'.
#' @param target_prefix Prefix for workflowr target names.
#' @param include Optional character vector of .Rmd files to be rendered.
#' @param exclude Optional character vector of .Rmd files to be excluded.
#' @param include_pattern By default, include all .Rmd files in `workflowr_dir`.
#' @param exclude_pattern By default, exclude any files with names prefixed by
#'   "_".
#' @param verbose If `TRUE`, prints a message listing the .Rmd files for which
#'   targets will be created.
#'
#' @return A list of target objects with `format = 'file'`, one for each
#'   workflowr report.
#' @export
#'
#' @examples
#' #TODO
tar_render_workflowr <- function(output_dir = "public",
                                 workflowr_dir = "analysis",
                                 target_prefix = "WFLWR_",
                                 include = NULL,
                                 exclude = NULL,
                                 include_pattern = "\\.Rmd$",
                                 exclude_pattern = "^_",
                                 verbose = FALSE) {

  # workflowr files to be knitted
  wflwr_rmds <- list.files(workflowr_dir)

  # include/exclude by pattern first
  if (!is.null(include_pattern)) {
    wflwr_rmds <- subset(wflwr_rmds,
                         stringr::str_detect(wflwr_rmds,
                                             pattern = include_pattern))
  }

  if (!is.null(exclude_pattern)) {
    wflwr_rmds <- subset(
      wflwr_rmds,
      stringr::str_detect(wflwr_rmds,
                          pattern = exclude_pattern,
                          negate = TRUE)
    )
  }

  # ...include/exclude specific files
  if (!is.null(include)) {
    wflwr_rmds <- subset(wflwr_rmds,
                         wflwr_rmds %in% include)
  }

  if (!is.null(exclude)) {
    wflwr_rmds <- subset(wflwr_rmds,!wflwr_rmds %in% exclude)
  }

  # list files to be knitted
  if (verbose) {
    message(paste0("Attempting to create targets for the following workflowr files: ",
                   stringr::str_c(wflwr_rmds,
                                  sep = "",
                                  collapse = ", "),
                   "."))
  }

  # parameters to loop through
  params <- data.frame(
    rmd_filename = wflwr_rmds,
    workflowr_dir = workflowr_dir,
    output_dir = output_dir,
    target_prefix = target_prefix
  )

  # return a list of targets
  purrr::pmap(params,
              tar_render_workflowr_single)
}

# PRIVATE -----------------------------------------------------------------

#' Target factory for a single workflowr R markdown report
#'
#' Helper function for [tar_render_workflowr()].
#'
#' @inheritParams tar_render_workflowr
#'
#' @return
tar_render_workflowr_single <- function(rmd_filename,
                                        workflowr_dir = "analysis",
                                        output_dir = "public",
                                        target_prefix = "WFLWR_") {

  # check that input Rmd file exists
  input_rmd_filepath <- file.path(workflowr_dir, rmd_filename)

  assertthat::assert_that(file.exists(input_rmd_filepath),
                          msg = paste0("No file exists at ",
                                       input_rmd_filepath))

  # html output must go to either `docs` (github) or `public` (gitlab) dir
  match.arg(output_dir,
            choices = c("public",
                        "docs"))

  # names
  html_filename <- stringr::str_replace(rmd_filename,
                                        pattern = "Rmd$",
                                        replacement = "html")
  output_html_filepath <- file.path(output_dir, html_filename)

  target_name <- toupper(rmd_filename)
  target_name <- stringr::str_replace_all(target_name,
                                          "\\.",
                                          "_")
  target_name <- paste0(target_prefix,
                        target_name)

  # return a target
  targets::tar_target_raw(
    target_name,
    command = substitute({
      suppressMessages(workflowr::wflow_build(input_rmd_filepath,
                                              verbose = FALSE))

      # returns file paths to both input Rmd and knitted output html
      c(input_rmd_filepath,
        output_html_filepath)
    }),
    deps = tarchetypes::tar_knitr_deps(input_rmd_filepath),
    format = "file"
  )
}
