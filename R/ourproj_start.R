#' Start a new workflowr/targets project
#'
#' Creates a directory with the essential files for a workflowr project, using
#' Gitlab Pages and a targets data analysis pipeline.
#'
#' @param directory character. The directory where the new project will be
#'   created, e.g. "~/myproj". An error is raised if this directory already
#'   exists.
#' @param project_name character. The name of the project.
#' @param git_username character. Gitlab user name.
#'
#' @return `NULL` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'  ourproj_start(
#'    directory = tempdir(),
#'    project_name = "my-proj",
#'    git_username = "user"
#'  )
#' }
ourproj_start <- function(directory,
                          project_name,
                          git_username) {

  # Copy template project to specified path (`directory`) -------------

  # An error is raised by `fs::dir_copy()` if a file/directory already exists
  # here

  # For discussion on benefits of `fs::path_package()` vs `system.file()`, see
  # https://r-pkgs.org/data.html#sec-data-system-file
  fs::dir_copy(path = fs::path_package("ourproj_template",
                                       package = "ourproj"),
               new_path = directory,
               overwrite = FALSE)

  # Render files using `whisker.render()` ---------------------
  file_paths <- list.files(path = directory,
                           full.names = TRUE,
                           recursive = TRUE)

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
                                GIT_USERNAME = git_username)
                  ) %>%
                  writeLines(con = .x))

  # rename .Rproj file
  file.rename(from = file.path(directory, "ourproj_template.R"),
              to = paste0(project_name,
                          ".Rproj"))
}
