#' Insert formatted headers
#'
#' `make_chunk()` is a function that allows a user to easily make highlighted
#' text into an rmarkdown code chunk
#'
#' @import rstudioapi
#' @name make_chunk

make_chunk <- function() {
  cont <- rstudioapi::getActiveDocumentContext()
  txt <- cont$selection[[1]]$text
  rstudioapi::insertText(paste0("```{r}\n", txt, "\n```\n"))
}

