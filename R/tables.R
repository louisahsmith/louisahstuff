#' Tables for printing
#'
#' @description These functions make it easy to print output in an rmarkdown document in a certain way.
#' @import knitr
#' @importFrom kableExtra kable_styling row_spec
#' @import tableone
#' @import gt
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import tibble
#' @import purrr
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
#' @param iqrVars a character vector of variables to calculate median IQR for
#' @param missing_text = a string to print instead of NA in the table (default = "---")
#' @description Other parameters to `print.TableOne()` can be passed as well
my_gt_table1 <- function(.data, strata = NULL, allVars, factorVars = NULL, iqrVars = NULL,
                         contDigits = 1, catDigits = 1, test = FALSE,
                      printToggle = FALSE, noSpaces = TRUE, showAllLevels = TRUE,
                      missing_text = "---", ...) {

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

  tab <- tableOne %>%
    print(test = test,
          printToggle = printToggle,
          noSpaces = noSpaces,
          showAllLevels = showAllLevels,
          nonnormal = iqrVars,
          catDigits = catDigits,
          contDigits = contDigits, ...) %>%
    as_tibble(rownames = NA) %>%
    rownames_to_column() %>%
    mutate_at(vars(-level, -rowname), ~ case_when(
      str_detect(., coll("NA")) ~ NA_character_,
      str_detect(., coll("NaN")) ~ NA_character_,
      TRUE ~ .)) %>%
    mutate(
      rowname = ifelse(rowname == "", NA, rowname),
           rowname = str_remove_all(rowname, coll(" (mean (SD))")),
      rowname = str_remove_all(rowname, coll(" (median [IQR])")),
           level = ifelse(level == "", rowname, level),
      IQR = level %in% iqrVars) %>%
    fill(rowname, .direction = "down") %>%
    mutate(rowname = case_when(
      rowname == "n" ~ NA_character_,
      rowname == level & !IQR ~ "Other characteristics (mean (SD))",
      rowname == level ~ "Other characteristics (median [IQR])",
      TRUE ~ rowname
    )) %>%
    rename_at(vars(starts_with("V")), ~paste("Overall")) %>%
    select(-IQR)

  grps <- tableOne$CatTable[[1]] %>% names %>% paste0(., " (%)")
  tab$rowname[is.na(tab$rowname)] <- ""

  gt(tab, rowname_col = "level", groupname_col = "rowname") %>%
    row_group_order(groups = c("", grps)) %>%
    fmt_missing(columns = everything(), missing_text = missing_text)
}

