library(shiny)
library(shinycssloaders)
library(DT)
library(dplyr)


# This code was used in the FiCli database to create a datatable with a download button.
# To view the full code, see "https://www.sciencebase.gov/catalog/item/5e6a68a9e4b01d509260e72f"

#' @title UI for downloadable datatable
#' @description The tableUI function creates a download button and datatable display from the given id. It is intended for use with tableServer
#' @param id The character vector to represent the id of the module, Should be the same when used with tableServer.
#' @param spinner Whether the table spinner should spin when the server is doing something.
#' @details This function returns a download button and datatable user interface that can be used independently of each other (i.e. a map table and a filtered table)
tableUI <- function(id, spinner = TRUE, width = 82.5, height = 64) {
  ns <- NS(id)
  div(
    id = ns("div"),
    style = paste("height: ", height, "vh; width: ", width, "vw; overflow-x: scroll;", sep = "", collapse = ""),
    uiOutput(ns("download_button")),
    if (spinner) {
      DTOutput(ns("table")) %>% withSpinner()
    }
    else {
      DTOutput(ns("table"))
    }
  )
}

#' @title Server for downloadable datatable.
#' @description The tableServer function creates a download button and datatable display from the given id. It is intended for use with tableUI.
#' @param id The character vector to represent the id of the module, Should be the same when used with tableUI.
#' @param data_table dataframe to visualize. Note: Should be reactive.
#' @details This function sends information to create a download button and datatable user interface that can be used independently of each other (i.e. a map table and a filtered table)
tableServer <- function(id, data_table) {
  moduleServer(id, function(input, output, session) {
    stopifnot(is.reactive(data_table))


    output$table <- renderDT(
      expr = {
        req(data_table(), nrow(data_table()) != 0)
        dplyr::select(data_table(), all_of(cols_to_show))
      },
      plugins = c("ellipsis"),
      # Allows for single selections
      selection = "single",
      # Hides Row Names
      rownames = FALSE,
      options = list(
        # Page Control, Length, Table, Information Summary, Page Control
        dom = "pltip",
        # Automatically handles with of the columns
        autoWidth = TRUE,
        # Allows for the vertical and horizontal scroll bar
        scrollX = TRUE,
        scrollY = TRUE,
        columnDefs = list(
          list(render = JS("$.fn.dataTable.render.ellipsis(200, true)"), targets = c(10, 11, 12)),
          # Makes the second column (Title) 360 pixels
          list(width = "360px", targets = c(2, 10, 11, 12)),
          # Hides Internal Paper ID
          list(visible = FALSE, targets = c(0))
        )
      )
    )

    # Download button for table.
    output$download_button <- renderUI(expr = {
      req(data_table(), nrow(data_table()) != 0)
      downloadBttn(NS(id, "download_df"), label = "Download Table", size = "xs")
    })

    # Download handler.
    output$download_df <- downloadHandler(
      filename = "database.csv",
      content = function(file) {
        write.csv(
          x = {
            req(data_table(), nrow(data_table()) != 0)
            select(data_table(), all_of(cols_to_show), -any_of(no_download_col))
          },
          file = file
        )
      }
    )
  })
}
