#' dMeasureMedication - medication module for dMeasure
#'
#'
#' @name dMeasureMedication
#' @title dMeasureMedication
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureMedication class
#' @title dMeasureMedication class
#' @description list appointments (with medication information) by clinician providers
#' @export
dMeasureMedication <- R6::R6Class(
  "dMeasureMedication",
  public = list(
    # dM is a dMeasure object
    dM = NULL, # pointer to dMeasure R6 object
    initialize = function(dMeasure_obj) {
      # dMeasure_obj is a R6 dMeasure object
      self$dM <- dMeasure_obj

      if (length(public_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(public_init_fields$name)) {
          if (public_init_fields$obj[[i]] == "dMeasureMedication") {
            self[[public_init_fields$name[[i]]]] <-
              eval(public_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }
      if (length(private_init_fields$name) > 0) { # only if any defined
        for (i in 1:length(private_init_fields$name)) {
          if (private_init_fields$obj[[i]] == "dMeasureMedication") {
            private[[private_init_fields$name[[i]]]] <-
              eval(private_init_fields$value[[i]]) # could 'quote' the value
          }
        }
      }

      if (requireNamespace("shiny", quietly = TRUE)) {
        # set reactive version only if shiny is available
        # note that this is for reading (from programs calling this object) only!
        if (length(reactive_fields$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_fields$name)) {
            if (reactive_fields$obj[[i]] == "dMeasureMedication") {
              self[[reactive_fields$name[[i]]]] <- shiny::reactiveVal(
                eval(reactive_fields$value[[i]]) # could 'quote' the value
              )
            }
          }
        }
        if (length(reactive_event$name) > 0) { # only if any .reactive() defined
          for (i in 1:length(reactive_event$name)) {
            if (reactive_event$obj[[i]] == "dMeasureMedication") {
              self[[reactive_event$name[[i]]]] <-
                eval(reactive_event$value[[i]]) # could 'quote' the value
            }
          }
        }
      }
    }
  )
  # this is a 'skeleton' class
  # it is filled in the with the '.public' function
)

##### special reactive functions ##########################

.private(dMeasureMedication, "set_reactive", function(myreactive, value) {
  # reactive (if shiny/reactive environment is available) is set to 'value'
  # myreactive is passed by reference
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(value)
  }
})
.private(dMeasureMedication, "trigger", function(myreactive) {
  # toggles a reactive between (usually) 0 and 1
  if (requireNamespace("shiny", quietly = TRUE)) {
    myreactive(1 - shiny::isolate(myreactive()))
  }
})

###########################################################

#' item description for left sidebar menu
#'
#' @name shinydashboardmenuItem
#'
#' @return shinydashboard menuItem object
#'
#' @export
shinydashboardmenuItem <- function() {
  return(
    shinydashboard::menuItem(
      "Medication",
      tabName = "medication", icon = shiny::icon("capsules")
    )
  )
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
        dMeasureMedication::datatableUI("medication_dt")
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
    shinycssloaders::withSpinner(
      DT::DTOutput(ns("medication_table")),
      type = 8,
      hide.element.when.recalculating = FALSE,
      proxy.height = NULL
    )
  )
}

.reactive(dMeasureMedication, "printcopy_view", TRUE)

#' Medication module - server
#'
#' @name datatableServer
#'
#' @param input as required by Shiny modules
#' @param output as required by Shiny modules
#' @param session as required by Shiny modules
#' @param dMMedication dMeasureMedication R6 object
#'
#' @return none
#'
#' @export
datatableServer <- function(input, output, session, dMMedication) {
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
}

.private(dMeasureMedication, ".n_medication", 4)
.active(dMeasureMedication, "n_medication", function(value) {
  if (missing(value)) {
    return(private$.n_medication)
  }
  if (is.numeric(value) & value == as.integer(value)) {
    private$.n_medication <- value
    private$set_reactive(self$n_medicationR, value)
  } else {
    warning(
      "'n_medication' must be an integer."
    )
  }
})
.reactive(dMeasureMedication, "n_medicationR", 4)

#' add tags to a patient list
#'
#' @param patient_list dataframe, must contain column internalID
#' @param medication_list dataframe, contains InternalID and DrugName
#' @param screentag add fomantic/semantic tags
#' @param screentag_print  add printable/copyable text-only tags
#'
#' @return dataframe, with added column screentag or screentag_print
add_medicationTags <- function(
  patient_list,
  medication_list,
  screentag = FALSE,
  screentag_print = TRUE) {

  if (nrow(medication_list) > 0) {
    medication_list <- medication_list %>>%
      dplyr::group_by(InternalID) %>>%
      dplyr::mutate(n = dplyr::n()) %>>%
      dplyr::ungroup()
  } else {
    medication_list$n <- numeric(0)
    # create an empty column
  }

  if (screentag_print) {
    tag <- medication_list %>>%
      dplyr::group_by(InternalID) %>>%
      dplyr::summarise(
        InternalID = max(InternalID),
        # 'max' converts vectors to a single number
        screentag_print = paste0(
          "(", max(n), ") ",
          dMeasure::paste2(
            DrugName, na.rm = TRUE, collapse = ", "
          )
        )
      ) %>>%
      dplyr::ungroup()
  } else if (screentag) {
    tag <- medication_list %>>%
      dplyr::group_by(InternalID) %>>%
      dplyr::summarize(
        InternalID = max(InternalID),
        screentag = dMeasure::semantic_tag(
          tag = as.character(max(n)),
          colour = "green",
          popuphtml = paste0(
            "<p><font size = \'+0\'>",
            dMeasure::paste2(
              DrugName, na.rm = TRUE, collapse = ", "
            ),
            "</p>"
          )
        )
      ) %>>%
      dplyr::ungroup()
  }

  tagged_list <- patient_list %>>%
    dplyr::left_join(
      tag,
      by = "InternalID"
    )

  return(tagged_list)
}

#' patient appointment list combined with medications
#'
#'  derived from dM$appointments_filtered_time
#'
#' @md
#'
#' @param dMeasureMedication_obj R6 object
#' @param n_medication minimum number of medications
#' @param screentag if neither `screentag` or `screentag_print` is
#'     defined then values are derived from `self$printcopy_view`
#' @param screentag_print
#'
#' @return dataframe of apppointments
#'  $Patient, $AppointmentDate, $AppointmentTime,
#'  $Provider, $Status, $Label
#'
#' @export
appointments_medication <- function(
  dMeasureMedication_obj,
  n_medication = NA,
  screentag = FALSE,
  screentag_print = TRUE) {
  dMeasureMedication_obj$appointments_medication(
    n_medication,
    screentag,
    screentag_print
  )
}
.public(
  dMeasureMedication, "appointments_medication",
  function(
    n_medication = NA,
    screentag = FALSE,
    screentag_print = TRUE
  ) {
    if (is.na(n_medication)) {
      n_medication <- self$n_medication
    }
    if (!screentag && !screentag_print) {
      stop("One of 'screentag' or 'screentag_print' must be set to TRUE")
    } else if (screentag && screentag_print) {
      stop("Only one of 'screentag' or 'screentag_print' can be set to TRUE")
    }

    intID <- c(-1) # create 'empty' vector of intID

    l <- self$dM$appointments_filtered_time
    intID <- c(-1, l %>>% dplyr::pull(InternalID))

    l_medications <-
      self$dM$db$currentRx %>>%
      dplyr::filter(InternalID %in% intID) %>>%
      dplyr::select(InternalID, DrugName) %>>%
      dplyr::collect()

    if (nrow(l_medications) > 0) {
      l_medications <- l_medications %>>%
        dplyr::group_by(InternalID) %>>%
        dplyr::mutate(n = dplyr::n()) %>>%
        dplyr::ungroup() %>>%
        dplyr::filter(n >= n_medication) %>>%
        dplyr::select(-n)
      # keep only if more than n_medication
    }

    l <- add_medicationTags(
      l,
      l_medications,
      screentag = screentag,
      screentag_print = screentag_print
    )

    if (screentag) {
      l <- l %>>%
        dplyr::rename(Medication = screentag)
    } else if (screentag_print) {
      l <- l %>>%
        dplyr::rename(Medication = screentag_print)
    }

    l <- l %>>%
      dplyr::select(
        Patient, AppointmentDate, AppointmentTime,
        Provider, Status, Medication
      )

    return(l)
  }
)
.reactive_event(
  dMeasureMedication, "appointments_medicationR",
  quote(
    shiny::eventReactive(
      c(
        self$n_medicationR(),
        self$dM$appointments_filtered_timeR(),
        self$printcopy_view()
      ), {
        self$appointments_medication(
          screentag = !self$printcopy_view(),
          screentag_print = self$printcopy_view()
        )
      }
    )
  )
)
