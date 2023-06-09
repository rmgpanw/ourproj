#' Start a new workflowr/targets project
#'
#' Creates a directory with the essential files for a workflowr project, using
#' Gitlab Pages and a targets data analysis pipeline.
#'
#' @param directory character. The directory where the new project will be
#'   created, e.g. "~/myproj". An error is raised if this directory already
#'   exists.
#' @param project_title character. The title of the project.
#' @param git_username character. GitHub/GitLab user name.
#' @param github_gitlab character. Either 'github' or 'gitlab'
#' @param minimal logical. If `TRUE`, sets up a minimal project structure.
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
                          minimal = TRUE) {


  # Validate args -----------------------------------------------------------

  assertthat::assert_that(assertthat::is.string(github_gitlab) &&
                            (github_gitlab %in% c("github", "gitlab")),
                          msg = "Argument `github_gitlab` must be either 'github' or 'gitlab'.")

  assertthat::is.string(directory)
  assertthat::is.string(project_title)
  assertthat::is.string(git_username)
  assertthat::is.flag(minimal)

  # Copy template project to specified path (`directory`) -------------

  # For discussion on benefits of `fs::path_package()` vs `system.file()`, see
  # https://r-pkgs.org/data.html#sec-data-system-file

  if (minimal) {
    template_dir <- "ourproj_template_minimal"
  } else {
    template_dir <- "ourproj_template"
  }

  # check `directory` exists, and `project_directory` does not yet exist
  project_name <- fs::path_file(directory)
  project_directory <- fs::path_dir(directory)

  assertthat::assert_that(fs::dir_exists(project_directory),
                          msg = paste0("Directory does not exist: ",
                                       project_directory))

  assertthat::assert_that(!fs::dir_exists(directory),
                          msg = paste0("A folder named '",
                                       project_name,
                                       "' already exists at ",
                                       project_directory))

  fs::dir_copy(path = fs::path_package(template_dir,
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
  files_to_render <- c("_site.yml")

  file_paths_to_render <- subset(file_paths,
                                 stringr::str_detect(
                                   string = file_paths,
                                   pattern = paste(
                                     paste0(files_to_render,
                                            "$"),
                                     sep = "",
                                     collapse = "|"
                                   )
                                 ))

  # render with `whisker.render()`
  file_paths_to_render %>%
    purrr::walk(~ readLines(.x) %>%
                  whisker::whisker.render(
                    data = list(PROJECT_NAME = project_name,
                                PROJECT_TITLE = project_title,
                                GIT_USERNAME = git_username,
                                GITHUB_GITLAB = github_gitlab)
                  ) %>%
                  writeLines(con = .x))

  # rename .Rproj file
  old_rproj_filename <- paste0(template_dir, ".Rproj")
  new_rproj_filename <- paste0(project_name, ".Rproj")

  file.rename(
    from = file.path(directory, old_rproj_filename),
    to = file.path(directory, new_rproj_filename)
  )},
  error = function(e) {
    # If any errors, remove the newly created directory before exiting
    unlink(directory,
           recursive = TRUE)

    stop(e$message)
  }
  )

  # return `directory` invisibly
  invisible(project_directory)
}
