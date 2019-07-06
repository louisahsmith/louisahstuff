#' Tables for printing
#'
#' @description This function makes it easy to print all output from an rmarkdown document in a certain way.
#' @import knitr
#' @import kableExtra
#' @import tableone
#' @import gt
#' @name my_tables

#' @rdname my_tables
#' @export my_df_html

my_df_html <- function(df) {
  kable(df) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE, position = "left"
      ) %>%
    row_spec(0, angle = -45)
}

# function to replace element of list
replace_elem <- function(.data, element, replacement) {
  .data[[element]] <- replacement[[element]]
  .data
}

#' @rdname my_tables
#' @export my_gt_table1
#' @param .data a dataframe
#' @param strata a character vector of variables to stratify by (if NULL, only overall table)
#' @param allVars a character vector of variables to be rows in the table
#' @param factorVars a character vector of factor variables
#' @description Other parameters to `print.TableOne()` can be passed as well
my_gt_table1 <- function(.data, strata, allVars, factorVars, test = FALSE,
                      printToggle = FALSE, noSpaces = TRUE, showAllLevels = TRUE, ...) {

  tableOne <- CreateTableOne(vars = allVars, data = .data,
                                 factorVars = factorVars)

  if (!is.null(strata)) {
    # create the stratified
    tableOne_strat <- CreateTableOne(vars = allVars, strata = strata, data = .data,
                                     factorVars = factorVars)

    # combine
    tableOne <- c(transpose(tableOne_strat), transpose(tableOne)) %>%
      transpose() %>%
      map2(tableOne_strat, ~structure(.x, class = class(.y))) %>%
      structure(class = class(tableOne_strat)) %>%
      replace_elem("MetaData", tableOne_strat)
  }

  tableOne %>%
    print(test = test,
          printToggle = printToggle,
          noSpaces = noSpaces,
          showAllLevels = showAllLevels, ...) %>%
    as_tibble(rownames = NA) %>%
    rownames_to_column() %>%
    mutate(rowname = ifelse(rowname == "", NA, rowname),
           rowname = str_remove_all(rowname, coll(" (mean (SD))")),
           level = ifelse(level == "", rowname, level)) %>%
    fill(rowname, .direction = "down") %>%
    mutate(rowname = case_when(
      rowname == "n" ~ NA_character_,
      rowname == level ~ "other characteristics (mean (sd))",
      TRUE ~ rowname
    )) %>%
    rename_at(vars(starts_with("V")), ~paste("Overall"))

  grps <- tableOne$CatTable[[1]] %>% names %>% paste0(., " (%)")

  gt(tab, rowname_col = "level", groupname_col = "rowname") %>%
    row_group_order(groups = c(NA, grps))
}

