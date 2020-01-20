#' @title Homework template
#'
#' @description This function produces an R Markdown document for homeworkds
#' @param metadata Extra yaml metadata to include. Defaults to NULL.
#' @keywords template
#' @export
#' @examples
#' rmarkdown::render("doc.Rmd", homework())

homework <- function(..., metadata = NULL) {
  #if(type == "pdf"){
  pdf_document_format(...,
                      format = "homework",
                      template = "homework.tex",
                      metadata = metadata)
  # }
  # if(type == "word"){
  #   word_document_format(...,
  #                       format = "homework",
  #                       template = "homework.docx",
  #                       metadata = "homework_word.yaml")
  # }
  # if(!type %in% c("pdf", "word")) stop("Other formats not yet supported")
}
