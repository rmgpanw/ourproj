
# PUBLIC ------------------------------------------------------------------

# Target factories --------------------------------------------------------

# for guidance, see https://wlandau.github.io/targetopia/contributing.html

#' Target factory for workflowr R markdown reports.
#'
#' Generates a list of target objects to render multiple workflowr R markdown
#' reports.
#'
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
tar_render_workflowr <- function(include = NULL,
                                 exclude = NULL,
                                 include_pattern = NULL,
                                 exclude_pattern = "^_",
                                 verbose = FALSE) {

  # constants
  target_prefix <- "WFLWR_"
  workflowr_dir <- "analysis"
  output_dir <- file.path(workflowr_dir, "_site.yml") %>%
    yaml::read_yaml() %>%
    .$output_dir %>%
    fs::path_file()

  # workflowr files to be knitted
  wflwr_rmds <- list.files(workflowr_dir) %>%
    subset(.,
           stringr::str_detect(.,
                               pattern = "\\.Rmd$"))

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

  # render targets for analysis notebooks
  workflowr_rmds <- purrr::pmap(params,
                                tar_render_workflowr_single)

  # workflowr yml files
  workflowr_ymls <- list(
    targets::tar_target_raw(
      name = paste0(target_prefix, "WORKFLOWR_YML"),
      "_workflowr.yml",
      format = "file"
    ),
    targets::tar_target_raw(
      name = paste0(target_prefix, "SITE_YML"),
      file.path("analysis", "_site.yml"),
      format = "file"
    )
  )

  # reactable listing and linking to workflowr reports, to be included in
  # index.Rmd
  params <- params %>%
    dplyr::mutate(
      target_name = wflwr_target_name_from_rmd(rmd_filename = .data[["rmd_filename"]],
                                               target_prefix = !!target_prefix)
    )

  params_workflowr_rmds_reactable <- params %>%
    dplyr::filter(!.data[["rmd_filename"]] %in% c("index.Rmd", "about.Rmd", "license.Rmd")) %>%
    dplyr::mutate("output_html" := stringr::str_replace(.data[["rmd_filename"]],
                                                        "Rmd$",
                                                        "html")) %>%
    dplyr::mutate(
      "title" := purrr::map_chr(
        .data[["rmd_filename"]],
        ~ file.path("analysis", .x) %>%
          rmarkdown::yaml_front_matter() %>%
          .$title %>%
          ifelse(is.null(.),
                 yes = .x,
                 no = .)
      )
    ) %>%
    dplyr::mutate("title" := ifelse(is.na(.data[["title"]]),
                                    .data[["rmds"]],
                                    .data[["title"]])) %>%
    dplyr::mutate("Analysis" := .data[["output_html"]]) %>%
    dplyr::mutate("Date modified" := file.info(file.path("analysis",
                                                         .data[["rmd_filename"]]))$ctime)

  workflowr_rmds_reactable <- list(
    targets::tar_target_raw(
      name = "WFLWR_RMDS_REACTABLE",
      command = substitute({
        # for including URL link in reactable, see https://glin.github.io/reactable/articles/examples.html#cell-rendering
        params_workflowr_rmds_reactable %>%
          dplyr::select(tidyselect::all_of(c("Analysis",
                                             "Date modified"))) %>%
          reactable::reactable(columns = list(Analysis = reactable::colDef(
            cell = function(value, index) {
              # Render as a link
              htmltools::tags$a(href = value,
                                # open links in new browser tab
                                # target = "_blank",
                                as.character(params_workflowr_rmds_reactable[index, "title"]))
            }
          )),
          filterable = TRUE,
          resizable = TRUE,
          showPageSizeOptions = TRUE,
          paginationType = "jump")
      }),
      deps = params %>%
        dplyr::filter(.data[["rmd_filename"]] != "index.Rmd") %>%
        dplyr::pull(.data[["target_name"]]),
    )
  )

  # return a list of targets
  c(workflowr_ymls,
    workflowr_rmds,
    workflowr_rmds_reactable)
}

# PRIVATE -----------------------------------------------------------------

#' Target factory for a single workflowr R markdown report
#'
#' Helper function for [tar_render_workflowr()].
#' @param rmd_filename Name of a workflowr Rmd file to be rendered.
#' @param workflowr_dir Directory where workflowr Rmd files are located.
#' @param output_dir Directory where rendered workflowr reports (html files) should be
#'   written to.
#' @param target_prefix Prefix to start workflowr target names with.
#'
#' @noRd
#' @return A target object (`format = 'file'`) for a single workflowr report.
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


  target_name <- wflwr_target_name_from_rmd(rmd_filename = rmd_filename,
                                            target_prefix = target_prefix)

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
    deps = c(tarchetypes::tar_knitr_deps(input_rmd_filepath),
             "WFLWR_SITE_YML",
             "WFLWR_WORKFLOWR_YML"),
    error = "continue",
    format = "file"
  )
}

wflwr_target_name_from_rmd <- function(rmd_filename,
                                       target_prefix) {
  target_name <- toupper(rmd_filename)
  target_name <- stringr::str_replace_all(target_name,
                                          "\\.",
                                          "_")
  target_name <- paste0(target_prefix,
                        target_name)

  return(target_name)
}
