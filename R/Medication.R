# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' dMeasureMedication - medication module for dMeasure
#'
#' @md
#'
#' @name dMeasureMedication
#' @title dMeasureMedication
#'
#' @include r6_helpers.R
#' functions to help create R6 classes
NULL

#' dMeasureIntegration
#'
#' @name dMeasureIntegration
#'
#' @description integration with dMeasure
#'   (especially DailyMeasure)
#'
#' @param information the information required
#'   `Provides` - modules provided (in this case, `dMeasureMedication`)
#'   `Requires` - the modules required (including `dMeasure`)
#'   `moduleID` - IDs of modules to create
#'
#' @return vector of required information
#'
#' @export
dMeasureIntegration <- function(information) {
  if (information == "Provides") {return(c("dMeasureMedication"))}
  if (information == "Requires") {return(c("dMeasure"))}
  if (information == "moduleID") {return(c("Medication_dt"))}
}

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
