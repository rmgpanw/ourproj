
#' Start a new workflowr/targets project
#'
#' Creates a directory with the essential files for a
#' [workflowr](https://workflowr.github.io/workflowr/) or
#' [quarto](https://quarto.org/) project, using GitHub/Gitlab Pages and a
#' [targets](https://books.ropensci.org/targets/) data analysis pipeline.
#'
#' Examples of available project templates:
#'
#' - [workflowr_targets](https://rmgpanw.github.io/workflowr_targets/)
#' - [quarto_website_targets](https://rmgpanw.github.io/quarto_website_targets/)
#'
#' @param directory character. The directory where the new project will be
#'   created, e.g. "~/myproj". An error is raised if this directory already
#'   exists.
#' @param project_title character. The title of the project.
#' @param git_username character. GitHub/GitLab user name.
#' @param github_gitlab character. Either 'github' or 'gitlab'
#' @param template Project template to be used. Options: 'workflowr_targets',
#'   'quarto_website_targets'.
#'
#' @return `directory` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'  ourproj_start(
#'    directory = tempdir(),
#'    project_title = "My Project",
#'    git_username = "user"
#'  )
#' }
ourproj_start <- function(directory,
                          project_title,
                          git_username,
                          github_gitlab = "gitlab",
                          template = 'workflowr_targets') {


  # Validate args -----------------------------------------------------------

  assertthat::assert_that(assertthat::is.string(github_gitlab) &&
                            (github_gitlab %in% c("github", "gitlab")),
                          msg = "Argument `github_gitlab` must be either 'github' or 'gitlab'.")

  output_dir <- switch(github_gitlab,
                       github = "docs",
                       gitlab = "public")

  assertthat::is.string(directory)
  assertthat::is.string(project_title)
  assertthat::is.string(git_username)
  match.arg(
    template,
    choices = c(
      'workflowr_targets',
      'quarto_website_targets'
    )
  )

  # Copy template project to specified path (`directory`) -------------

  # For discussion on benefits of `fs::path_package()` vs `system.file()`, see
  # https://r-pkgs.org/data.html#sec-data-system-file

  # check `directory` exists, and `project_parent_directory` does not yet exist
  project_name <- fs::path_file(directory)
  project_parent_directory <- fs::path_dir(directory)

  assertthat::assert_that(fs::dir_exists(project_parent_directory),
                          msg = paste0("Directory does not exist: ",
                                       project_parent_directory))

  assertthat::assert_that(!fs::dir_exists(directory),
                          msg = paste0("A folder named '",
                                       project_name,
                                       "' already exists at ",
                                       project_parent_directory))

  fs::dir_copy(path = fs::path_package(template,
                                       package = "ourproj"),
               new_path = directory,
               overwrite = FALSE)

  # Render files using `whisker.render()` ---------------------

  tryCatch({
  file_paths <- list.files(
    path = directory,
    full.names = TRUE,
    recursive = TRUE,
    all.files = TRUE
  )

  # subset for files to be rendered
  files_to_render <- c("_site.yml",
                       "_quarto.yml")

  file_paths_to_render <- subset(file_paths,
                                 stringr::str_detect(
                                   string = file_paths,
                                   pattern = paste(
                                     paste0(files_to_render,
                                            "$"),
                                     sep = "",
                                     collapse = "|"
                                   )
                                 )) %>%
    subset(.,
           stringr::str_detect(
             string = .,
             pattern = "renv",
             negate = TRUE
           ))

  # render with `whisker.render()`
  file_paths_to_render %>%
    purrr::walk(~ readLines(.x) %>%
                  whisker::whisker.render(
                    data = list(PROJECT_NAME = project_name,
                                PROJECT_TITLE = project_title,
                                GIT_USERNAME = git_username,
                                GITHUB_GITLAB = github_gitlab,
                                OUTPUT_DIR = output_dir)
                  ) %>%
                  writeLines(con = .x))

  # copy hidden files
  hidden_files_dir <- fs::path_package("hidden_files",
                                      package = "ourproj")

  hidden_files <- list.files(
    hidden_files_dir,
    full.names = TRUE,
    recursive = TRUE
  )

  hidden_files %>%
    purrr::set_names(
      ~ .x %>%
        stringr::str_remove(hidden_files_dir) %>%
        file.path(directory,
                  .) %>%
        stringr::str_replace(
          pattern = fs::path_file(.),
          replacement = paste0(".",
                               fs::path_file(.))
        )
    ) %>%
    purrr::iwalk(~ {
      fs::file_copy(path = .x,
                    new_path = .y,
                    overwrite = TRUE)
    })

  # rename .Rproj file
  old_rproj_filename <- paste0(template, ".Rproj")
  new_rproj_filename <- paste0(project_name, ".Rproj")

  file.rename(
    from = file.path(directory, old_rproj_filename),
    to = file.path(directory, new_rproj_filename)
  )

  # deactivate renv - possibly only needed when installing package from source
  # locally (i.e. not from GitHub), hence 'if' statement. Also note that
  # renv::deactivate() restarts the current R session, even when using
  # withr::with_dir()
  if (file.exists(file.path(directory,
                            "renv"))) {
    unlink(file.path(directory,
                     "renv"),
           recursive = TRUE,
           force = TRUE)
  }

  if (file.exists(file.path(directory,
                            "renv.lock"))) {
    unlink(file.path(directory,
                     "renv.lock"))
  }

  },
  error = function(e) {
    # If any errors, remove the newly created directory before exiting
    unlink(directory,
           recursive = TRUE)

    stop(e$message)
  }
  )

  # return `directory` invisibly
  invisible(directory)
}
