# Call rmarkdown::pdf_document and mark the return value as inheriting pdf_document
inherit_pdf_document <- function(...) {
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"
  fmt
}

# Call rmarkdown::word_document and mark the return value as inheriting word_document
inherit_word_document <- function(...) {
  fmt <- rmarkdown::word_document(...)
  fmt$inherits <- "word_document"
  fmt
}

# Call rmarkdown::beamer_presentation and mark the return value as inheriting beamer_presentation
inherit_beamer_presentation <- function(...) {
  fmt <- rmarkdown::beamer_presentation(...)
  fmt$inherits <- "beamer_presentation"
  fmt
}

find_file <- function(template, file) {
  template <- system.file("rmarkdown", "templates", template, file,
                          package = "templates")
  if (template == "") {
    stop("Couldn't find template file ", template, "/", file, call. = FALSE)
  }

  template
}

find_resource <- function(template, file) {
  find_file(template, file.path("skeleton", file))
}

# Helper function to create a custom format derived from pdf_document
# that includes a custom LaTeX template and YAML metadata
pdf_document_format <- function(..., format, template = "default", metadata = NULL, bibstyle = "epidemiology", bibliography = "library_fixed") {

  # base format
  fmt <- inherit_pdf_document(..., template = find_resource(format, template))

  if (!is.null(metadata)) {
    fmt$pandoc$args <- c(fmt$pandoc$args,
                         "--metadata-file",
                         rmarkdown::pandoc_path_arg(find_resource(format, metadata)))
  }

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--bibliography",
                       rmarkdown::pandoc_path_arg(system.file(paste0("rmarkdown/resources/", bibliography, ".bib"), package = "templates")))

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--csl",
                       rmarkdown::pandoc_path_arg(system.file(paste0("rmarkdown/resources/", bibstyle, ".csl"), package = "templates")))

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--citation-abbreviations",
                       rmarkdown::pandoc_path_arg(system.file("rmarkdown/resources/abbreviations.json", package = "templates")))

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--filter",
                       "/usr/local/bin/pandoc-crossref")
  # return format
  fmt
}

word_document_format <- function(..., format, template = "default", metadata = NULL, bibstyle = "epidemiology", bibliography = "library_fixed") {

  # base format
  fmt <- inherit_word_document(..., reference_docx = find_resource(format, template))

  if (!is.null(metadata)) {
    fmt$pandoc$args <- c(fmt$pandoc$args,
                         "--metadata-file",
                         rmarkdown::pandoc_path_arg(find_resource(format, metadata)))
  }

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--bibliography",
                       rmarkdown::pandoc_path_arg(system.file(paste0("rmarkdown/resources/", bibliography, ".bib"), package = "templates")))

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--csl",
                       rmarkdown::pandoc_path_arg(system.file(paste0("rmarkdown/resources/", bibstyle, ".csl"), package = "templates")))

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--citation-abbreviations",
                       rmarkdown::pandoc_path_arg(system.file("rmarkdown/resources/abbreviations.json", package = "templates")))

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--filter",
                       "/usr/local/bin/pandoc-crossref")

   fmt$pandoc$args <- c(fmt$pandoc$args,
                        "--filter",
                        "/usr/local/bin/pandoc-word-newpage")

  # return format
  fmt
}

# Helper function to create a custom format derived from pdf_document
# that includes a custom LaTeX template and YAML metadata
beamer_presentation_format <- function(..., format, template = "default", metadata = NULL) {

  # base format
  fmt <- inherit_beamer_presentation(..., template = find_resource(format, template))

  if (!is.null(metadata)) {
    fmt$pandoc$args <- c(fmt$pandoc$args,
                         "--metadata-file",
                         rmarkdown::pandoc_path_arg(find_resource(format, metadata)))
  }

  fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--bibliography",
                       rmarkdown::pandoc_path_arg(system.file("rmarkdown/resources/library.bib", package = "templates")))

  fmt
}
