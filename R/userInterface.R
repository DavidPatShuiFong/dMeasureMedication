# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' Interface elements of dMeasureMedication
#'
#' requires R6 methods from Custom.R
#'
#' @include Medication.R
NULL

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  x <- list(
    shinydashboard::menuItem(
      "Medication",
      tabName = "medication",
      icon = shiny::icon("capsules")
    )
  )

  return(x)
}

#' center panel description
#'
#' @name dMeasureShinytabItems
#'
#' @return shinytabItems
#'
#' @export
dMeasureShinytabItems <- function() {
  x <- list(
    shinydashboard::tabItem(
      tabName = "medication",
      shiny::fluidRow(shiny::column(
        width = 12, align = "center",
        shiny::h2("Medication")
      )),
      shiny::fluidRow(shiny::column(
        width = 12,
        dMeasureMedication::datatableUI("Medication_dt")
      ))
    )
  )
  return(x)
}


#' Medication module - UI function
#'
#' Display appointments within selected range of dates and providers
#'
#' @name datatableUI
#'
#' @param id module ID (used in conjunction with 'callModule')
#'
#' @return Shiny user interface element
#'
#' @export
datatableUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        4,
        shinyWidgets::switchInput(
          inputId = ns("printcopy_view"),
          label = paste(
            "<i class=\"fas fa-print\"></i>",
            "<i class=\"far fa-copy\"></i>",
            "  Print and Copy View"
          ),
          labelWidth = "12em",
          width = "20em"
        )
      ),
      shiny::column(
        2,
        offset = 3,
        shinyWidgets::dropdown(
          inputId = ns("medication_dropdown"),
          label = "Number of medications",
          icon = shiny::icon("list-ol"),
          shiny::sliderInput(
            inputId = ns("n_medication"),
            label = "Minimum number of medications",
            value = 4,
            min = 1,
            max = 10,
            step = 1
          )
        )
      )
    ),
    DT::DTOutput(ns("medication_table"))
  )
}

.reactive(dMeasureMedication, "printcopy_view", TRUE)

#' Medication module - server
#'
#' @name datatableServer
#'
#' @param id id
#' @param dMMedication dMeasureMedication R6 object
#'
#' @return none
#'
#' @export
datatableServer <- function(id, dMMedication) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$printcopy_view, ignoreNULL = TRUE, {
      dMMedication$printcopy_view(input$printcopy_view)
    })
    shiny::observeEvent(input$n_medication,
                        ignoreInit = TRUE,ignoreNULL = FALSE, {
                          dMMedication$n_medication <- input$n_medication
                        })

    styled_medication_list <- shiny::reactive({
      shiny::validate(
        shiny::need(
          dMMedication$dM$appointments_filtered_timeR(),
          "No appointments in selected range"
        )
      )
      if (input$printcopy_view == TRUE) {
        DailyMeasure::datatable_styled(
          dMMedication$appointments_medicationR()
        )
      } else {
        escape_column <- which(
          names(dMMedication$appointments_medicationR()) == "Medication"
        )
        DailyMeasure::datatable_styled(
          dMMedication$appointments_medicationR(),
          escape = c(escape_column),
          copyHtml5 = NULL, printButton = NULL,
          downloadButton = NULL # no copy/print buttons
        )
      }
    })

    output$medication_table <- DT::renderDT({
      styled_medication_list()
    })
  })
}
