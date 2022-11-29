#' Create a filterable table of Workflowr analysis reports
#'
#' To be used in `analysis/index.Rmd` within a [Workflowr
#' project](https://workflowr.github.io/workflowr/index.html).
#'
#' @return A [reactable](reactable) table.
#' @export
#'
#' @examples
#' \dontrun{
#'  #TODO
#' }
wflow_analyses_reactable <- function() {
  result <- data.frame(rmds = list.files("analysis")) %>%
    dplyr::filter(stringr::str_detect(.data[["rmds"]], "\\.Rmd$")) %>%
    dplyr::filter(!.data[["rmds"]] %in% c("index.Rmd",
                                          "about.Rmd",
                                          "license.Rmd")) %>%
    dplyr::mutate(output_html = stringr::str_replace(.data[["rmds"]],
                                                     "Rmd$",
                                                     "html")) %>%
    dplyr::mutate(title = purrr::map_chr(.data[["rmds"]],
                                         ~ {
                                           rmd_lines <- readLines(file.path("analysis", .x))

                                           rmd_title <- subset(rmd_lines,
                                                               stringr::str_detect(rmd_lines,
                                                                                   "title: "))[1]

                                           rmd_title %>%
                                             stringr::str_replace("title: ",
                                                                  "") %>%
                                             stringr::str_replace_all('\\"',
                                                                      "")
                                         })) %>%
    dplyr::mutate("title" = ifelse(is.na(.data[["title"]]),
                                 .data[["rmds"]],
                                 .data[["title"]])) %>%
    dplyr::mutate("Analysis" = .data[["output_html"]])


  result %>%
    dplyr::select(tidyselect::all_of(c("Analysis"))) %>%
    reactable::reactable(columns = list(Analysis = reactable::colDef(
      cell = function(value, index) {
        # Render as a link
        htmltools::tags$a(href = value,
                          # open links in new browser tab
                          # target = "_blank",
                          as.character(result[index, "title"]))
      }
    )))
}
