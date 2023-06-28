# Manuscript utils --------------------------------------------------------

bold_cap <- function(x) {
  # to be used with captioner, e.g. bold_cap(caps$main_figure('crp-replication-heatmap-cis-v-trans'))

  # bolds 'figure'/'table'
  x <- paste0("**", x)

  x <- stringr::str_replace(string = x,
                            pattern = ":",
                            replacement = ".**")

  return(x)
}
