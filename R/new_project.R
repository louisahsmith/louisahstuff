#' Create a new project
#'
#'
#' @name new_project

new_project <- function(path, use_targets = TRUE, use_renv = TRUE, use_git = TRUE,
                        packages = c("tidyverse"), ...) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  setwd(path)

  if (use_targets) {
    if (!require(targets)) stop("targets package not installed")

    file.copy(system.file("extdata", "_targets.R",
                          package = "louisahstuff"), "_targets.R")

    if (use_renv) {
      tar_renv(extras = packages)
    }
  }

  if (use_git) {
    if (!require(usethis)) stop("usethis package not installed")

    # use_git()
    # use_github(private = TRUE)

  }

  if (use_renv) {
    if (!require(renv)) stop("renv package not installed")

    init(".")
  }

}
