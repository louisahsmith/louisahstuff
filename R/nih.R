#' @title NIH grant template
#'
#' @description This function produces an R Markdown document corresponding approximately to what an NIH grant may look like
#' @param metadata Extra yaml metadata to include. Defaults to NULL.
#' @param type Either "pdf" or "word". Defaults to "pdf".
#' @keywords template
#' @export
#' @examples
#' rmarkdown::render("doc.Rmd", nih())

nih <- function(..., metadata = NULL, type = "pdf") {
  if(type == "pdf"){
    return(pdf_document_format(...,
                               format = "nih",
                               template = "nih.tex",
                               metadata = metadata,
                               bibstyle = "nih",
                               bibliography = "library_fixed"))
  }
  if(type == "word"){
    return(word_document_format(...,
                                format = "nih",
                                template = "nih.docx",
                                metadata = metadata,
                                bibstyle = "nih",
                                bibliography = "library_fixed"))
  }
  if(!type %in% c("pdf", "word")) stop("Other formats not yet supported")
}
