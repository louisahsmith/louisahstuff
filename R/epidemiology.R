#' @title Epidemiology template
#'
#' @description This function produces an R Markdown document corresponding to the Epidemiology instructions for authors
#' @param metadata Extra yaml metadata to include. Defaults to NULL.
#' @param type Either "pdf" or "word". Defaults to "pdf".
#' @keywords template
#' @export epidemiology
#' @examples
#' rmarkdown::render("doc.Rmd", epidemiology())

epidemiology <- function(..., metadata = NULL, type = "pdf") {
  if(type == "pdf"){
    return(pdf_document_format(...,
                        format = "epidemiology",
                        template = "epidemiology.tex",
                        metadata = metadata,
                        bibstyle = "epidemiology",
                      bibliography = "library"))
  }
  if(type == "word"){
    return(word_document_format(...,
                        format = "epidemiology",
                        template = "epidemiology.docx",
                        metadata = metadata,
                        bibstyle = "epidemiology",
                      bibliography = "library_fixed"))
  }
  if(!type %in% c("pdf", "word")) stop("Other formats not yet supported")
}
