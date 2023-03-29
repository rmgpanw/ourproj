# PUBLIC ------------------------------------------------------------------

# Target factories --------------------------------------------------------

# for guidance, see https://wlandau.github.io/targetopia/contributing.html

#' Target factory for pipeline input files
#'
#' @param yaml_file File path to a yaml file
#'
#' @return A list of target objects with `format = 'file'`, one for each input
#'   file.
#' @export
#'
#' @examples
#' # TODO
tar_track_input_files <- function(yaml_file = NULL) {

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
                                                 "x" = paste(head(missing_files),
                                                             sep = "",
                                                             collapse = "\n"))))

  # create targets
  input_files %>%
    purrr::imap( ~ targets::tar_target_raw(.y,
                                           substitute(INPUT_FILE, env = list(INPUT_FILE = .x)),
                                           format = "file"))
}
