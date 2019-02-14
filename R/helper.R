#' Insert formatted headers
#'
#' `helper()` is a function that runs a Shiny gadget in the viewpane
#' to allow for the formatting and insertion of headers
#'
#' @param lw Integer. Width at which to split the lines. Default = 70.
#' @param name Character. Default name to use as author. Set to NULL
#' if no default, else will choose from option helper.name.
#'
#' @import shiny
#' @import miniUI
#' @import stringr
#' @import rstudioapi

#====================================================================#
# Author:  Louisa H. Smith                                           #
# Date:    2019-01-22                                                #
# Revised: 2019-02-13                                                #
# Purpose: Create a Shiny gadget that can be installed as an add-    #
#          in in order to help keep code organized in a consistent   #
#          manner                                                    #
#====================================================================#


#### load packages ---------------------------------------------------
library(shiny)
library(miniUI)
library(stringr)

#### create gadget ---------------------------------------------------
# the gadget is created here and installed as an add-in in the file
# inst/rstudio/addins.dcf

#### FUN:helper() ****************************************************
# input:   lw = linewidth, name = default or NULL
# output:  runs in viewpane
# purpose: creates and runs a Shiny gadget in viewpane which allows
#          RStudio user to use text input to create file headers,
#          function descriptions, and section headers with consistent
#          formatting
helper <- function(lw = 70, name = getOption("helper.name")) {
  ui <- miniPage(
    gadgetTitleBar("Headers", right = NULL),
    miniTabstripPanel(
      miniTabPanel("File Head",
        icon = icon("play-circle"),
        miniContentPanel(
          fillCol(
            fillRow(h5("Name:"),
              textInput("name", "", value = name),
              flex = c(1, 3)
            ),
            fillRow(h5("Purpose:"),
              textAreaInput("purpose", "", rows = 6),
              flex = c(1, 3)
            ),
            flex = c(1, 4)
          )
        ),
        miniButtonBlock(
          actionButton("done_head", "Done")
        )
      ),
      miniTabPanel("Function",
        icon = icon("wrench"),
        miniContentPanel(
          fillCol(
            fillRow(h5("Function:"),
              textInput("func", ""),
              flex = c(1, 3)
            ),
            fillRow(h5("Arguments:"),
              textInput("args", ""),
              flex = c(1, 3)
            ),
            fillRow(h5("Outputs:"),
              textInput("returnvals", ""),
              flex = c(1, 3)
            ),
            fillRow(h5("Purpose:"),
              textAreaInput("func_purp", "", rows = 4),
              flex = c(1, 3)
            ),
            flex = c(1, 1, 1, 3)
          )
        ),
        miniButtonBlock(
          actionButton("done_func", "Done")
        )
      ),
      miniTabPanel("Section",
        icon = icon("bookmark"),
        miniContentPanel(
          fillCol(
            fillRow(h5("Section title:"),
              textInput("section", ""),
              flex = c(1, 3)
            ),
            fillRow(h5("Comments:"),
              textAreaInput("comments", "", rows = 3),
              flex = c(1, 3)
            )
          )
        ),
        miniButtonBlock(
          actionButton("done_sect", "Done")
        )
      )
    )
  )

  server <- function(input, output) {
    observeEvent(input$done_head, {
      purpose <- str_wrap(input$purpose, width = lw - 13)
      splits <- str_split(purpose, "\n")[[1]]
      splits <- str_trim(splits, side = "both")
      s <- vapply(splits, nchar, FUN.VALUE = integer(1))
      keep <- paste0(splits[1], strrep(" ", lw - 13 - s[1]), " #\n")
      if (length(splits) > 1) {
        for (i in 2:length(splits)) {
          keep <- paste0(
            keep, "#", strrep(" ", 10),
            splits[i], strrep(" ", lw - 13 - s[i]), " #\n"
          )
        }
      }
      use_name <- ifelse(is.null(name), input$name, name)
      n <- nchar(use_name)
      dchar <- as.character(Sys.Date())
      d <- nchar(dchar)
      paste_text <- paste0(
        "#", strrep("=", lw - 2), "#", "\n",
        "# Author:  ", use_name, strrep(" ", lw - 12 - n), "#\n",
        "# Date:    ", dchar, strrep(" ", lw - 12 - d), "#\n",
        "# Revised: ", dchar, strrep(" ", lw - 12 - d), "#\n",
        "# Purpose: ", keep,
        "#", strrep("=", lw - 2), "#", "\n\n"
      )
      rstudioapi::insertText(paste_text)
      stopApp()
    })

    observeEvent(input$done_func, {
      func_purp <- str_wrap(input$func_purp, width = lw - 10)
      func_purp <- str_replace_all(func_purp, "\n", "\n#          ")
      args <- str_wrap(input$args, width = lw - 10)
      args <- str_replace_all(args, "\n", "\n#          ")
      returnvals <- str_wrap(input$returnvals, width = lw - 10)
      returnvals <- str_replace_all(returnvals, "\n", "\n#          ")
      x <- nchar(input$func)
      paste_text <- paste0(
        "#### FUN:", input$func, "() ", strrep("*", lw - 12 - x), "\n",
        "# input:   ", args, "\n",
        "# output:  ", returnvals, "\n",
        "# purpose: ", func_purp
      )
      rstudioapi::insertText(paste_text)
      stopApp()
    })

    observeEvent(input$done_sect, {
      x <- nchar(input$section)
      comments <- str_wrap(input$comments, width = lw - 5)
      comments <- str_replace_all(comments, "\n", "\n# ")
      part1 <- paste0("#### ", input$section, " ", strrep("-", lw - 6 - x))
      paste_text <- ifelse(comments != "",
        paste0(part1, "\n# ", comments),
        part1
      )
      rstudioapi::insertText(paste_text)
      stopApp()
    })
    observeEvent(input$cancel, {
      stopApp()
    })
  }

  runGadget(ui, server)
}
