#' Create a targets outline
#'
#' `outline_targets()` is used to clean up `_targets.R` files (in the current
#' working directory) by inserting headers at every target and set of targets
#' so that the RStudio outline function can be used.
#'
#' @import purrr
#' @import stringr
#' @import rstudioapi
#' @name outline_targets

outline_targets <- function() {

  if (file.exists(file.path(getwd(), "_targets.R"))) {
    rstudioapi::navigateToFile(file.path(getwd(), "_targets.R"))
  } else {
    warning("No _targets.R file in the working directory")
  }

  file <- rstudioapi::getSourceEditorContext()

  text <- file$contents

  # ignore commented out lines
  no_comments <- text[!stringr::str_starts(text, "\\s*\\#")]

  # look for tar_target() calls (does not currently find other tar_ objects)
  targets <- stringr::str_detect(no_comments, "tar\\_target\\(")
  targets_over2 <- paste(no_comments[targets], no_comments[which(targets) + 1])
  target_names <- stringr::str_split(targets_over2, "tar\\_target\\(|\\,",
                                     n = 3, simplify = TRUE)[,2]
  target_headers <- paste("  ###", stringr::str_squish(target_names), "----\n")
  target_lines <- which(stringr::str_detect(text, "tar\\_target\\(") &
                          !stringr::str_starts(text, "\\s*\\#"))

  # find lists of tar_target() objects (or other things)
  objects <- stringr::str_detect(no_comments, "list\\(")
  objects_over2 <- paste(no_comments[which(objects) - 1], no_comments[objects])
  objects_over2_found <- stringr::str_detect(objects_over2,
                                             "(\\<\\-|\\=)\\s*list\\(")
  object_names <- stringr::str_split(objects_over2[objects_over2_found],
                                     "(\\<\\-|\\=)\\s*", n = 2,
                                     simplify = TRUE)[,1]
  objects_loc <- objects
  objects_loc[objects] <- objects_over2_found
  objects_loc[which(objects_loc) - 1] <- TRUE

  object_headers <- paste("##", stringr::str_squish(object_names), "----\n")
  object_lines_no_comments <- objects_loc & stringr::str_detect(no_comments, "(\\<\\-|\\=)")
  object_lines <- (1:length(text))[!stringr::str_starts(text, "\\s*\\#")][object_lines_no_comments]

  new_text <- c(object_headers, target_headers)
  rows <- c(object_lines, target_lines)

  # just assume that duplicates are unwanted (shouldn't have two objects with same name)
  to_remove <- stringr::str_squish(new_text) %in% stringr::str_squish(text)
  new_text <- new_text[!to_remove]
  rows <- rows[!to_remove]

  rstudioapi::insertText(location = purrr::map(rows, ~c(.x, 1)),
                         text = new_text,
                         id = file$id)
}
