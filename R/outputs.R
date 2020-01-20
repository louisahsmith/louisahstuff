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

notes <- function(..., metadata = NULL) {
  #if(type == "pdf"){
  pdf_document_format(...,
                      format = "notes",
                      template = "notes.tex",
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

slides <- function(..., metadata = NULL) {
  beamer_presentation_format(...,
                        format = "slides",
                        template = "slides.tex",
                        metadata = metadata,
                        latex_engine = "xelatex",
                        citation_package = "biblatex")

  # if(!type %in% c("pdf")) stop("Other formats not yet supported")
}


epidemiology <- function(..., metadata = NULL, type = "pdf") {
  if(type == "pdf"){
    return(pdf_document_format(...,
                        format = "epidemiology",
                        template = "epidemiology.tex",
                        metadata = metadata,
                        bibstyle = "epidemiology",
                      bibliography = "library_fixed"))
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
