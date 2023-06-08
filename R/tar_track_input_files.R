# PUBLIC ------------------------------------------------------------------

# Target factories --------------------------------------------------------

# for guidance, see https://wlandau.github.io/targetopia/contributing.html

#' Target factory for pipeline input files
#'
#' @param yaml_file File path to a yaml file
#'
#' @return A list of target objects with `format = 'file'`, one for each input
#'   file.
#'
#' @examples
#' # TODO
tar_track_input_files_yaml <- function(yaml_file = NULL) {

  # validate args
  if (is.null(yaml_file)) {
    yaml_file <- "input_file_targets.yaml"
  }

  stopifnot(rlang::is_string(yaml_file))
  stopifnot(file.exists(yaml_file))

  # read yaml into a single level list
  input_files <- yaml::read_yaml(file = yaml_file) %>%
    purrr::flatten()

  # check all file paths exist, raising error if not
  missing_files <- input_files %>%
    as.character() %>%
    subset(.,
           !file.exists(.))

  assertthat::assert_that(rlang::is_empty(missing_files),
                          msg = cli::cli_abort(c("{length(missing_files)} files listed in '{yaml_file}' do not exist, including:",
                                                 "x" = paste(utils::head(missing_files),
                                                             sep = "",
                                                             collapse = "\n"))))

  # create targets
  input_files %>%
    purrr::imap( ~ targets::tar_target_raw(.y,
                                           substitute(INPUT_FILE, env = list(INPUT_FILE = .x)),
                                           format = "file"))
}

#' Target factory for pipeline input files
#'
#' Searches the local environment for global objects that input file paths and
#' generates targets for these with `format = "file"`.
#'
#' @param suffix Suffix indicating global objects that are input file paths.
#'
#' @return A list of target objects with `format = 'file'`, one for each input
#'   file.
#' @export
#'
#' @examples
#' # dummy input file to create target for
#' DATA_IN_PATH <- tempfile(fileext = ".txt")
#' file.create(DATA_IN_PATH)
#'
#' # create target for this with `format = "file"`
#' tar_track_input_files()
tar_track_input_files <- function(suffix = "_IN_PATH") {

  # validate args
  assertthat::is.string(suffix)

  # character vector of global object names matching `suffix`
  input_files <- ls(envir = globalenv(), pattern = suffix)

  # check all file paths are type character
  invalid_file_paths <- input_files %>%
    purrr::set_names() %>%
    purrr::map(~ is.character(get(.x))) %>%
    purrr::keep(~ !.x) %>%
    purrr::imap(~ .y) %>%
    as.character()

  assertthat::assert_that(rlang::is_empty(invalid_file_paths),
                          msg = cli::cli_abort(c(
                            cli::cli_text(
                              "{length(invalid_file_paths)} object{?s} in global environment {?is an/are} invalid file path{?s}{?/, including}:"
                            ),
                            "x" = paste(
                              utils::head(invalid_file_paths),
                              sep = "",
                              collapse = ",\n"
                            )
                          )))

  # check all file paths exist, raising error if not
  missing_files <- input_files %>%
    purrr::set_names() %>%
    purrr::map(~ file.exists(get(.x))) %>%
    purrr::keep(~ !.x) %>%
    purrr::imap(~ paste0("`",
                         .y,
                         "`: '",
                         get(.y),
                         "'")) %>%
    as.character()

  assertthat::assert_that(rlang::is_empty(missing_files),
                          msg = cli::cli_abort(
                            c(
                              "{length(missing_files)} file path{?s} obtained from global objects {?does/do} not exist{?/, including}:",
                              "x" = paste(utils::head(missing_files),
                                          sep = "",
                                          collapse = ",\n")
                            )
                          ))

  # create targets, removing suffix from targets names
  result <- input_files %>%
    purrr::set_names() %>%
    purrr::map(get)

  names(result) <- stringr::str_remove(names(result),
                                       pattern = suffix)

  result %>%
    purrr::imap( ~ targets::tar_target_raw(.y,
                                           substitute(INPUT_FILE, env = list(INPUT_FILE = .x)),
                                           format = "file"))
}
