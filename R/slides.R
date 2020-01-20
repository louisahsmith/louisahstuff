#' @title Slides template
#'
#' @description This function produces an R Markdown document corresponding to a particular Beamer template
#' @param metadata Extra yaml metadata to include. Defaults to NULL.
#' @keywords template
#' @export
#' @examples
#' rmarkdown::render("doc.Rmd", slides())

slides <- function(..., metadata = NULL) {
  beamer_presentation_format(...,
                             format = "slides",
                             template = "slides.tex",
                             metadata = metadata,
                             latex_engine = "xelatex",
                             citation_package = "biblatex")

  # if(!type %in% c("pdf")) stop("Other formats not yet supported")
}
