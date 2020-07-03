#' methods of dMeasureCustom
#'
#' requires R6 methods from Custom.R
#'
#' @include Medication.R
NULL

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
      dplyr::arrange(InternalID, DrugName) %>>%
      # arrange does not respect group by default
      # variably, on the same data!, using .group_by = TRUE
      #  can fail! (perhaps different version of dplyr?)
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
        ThisInternalID = max(InternalID),
        # 'max' converts vectors to a single number
        screentag_print = paste0(
          "(", max(n), ") ",
          dMeasure::paste2(
            DrugName, na.rm = TRUE, collapse = ", "
          )
        )
      ) %>>%
      dplyr::ungroup() %>>%
      dplyr::mutate(InternalID = ThisInternalID) %>>%
      dplyr::select(InternalID, screentag_print)
  } else if (screentag) {
    tag <- medication_list %>>%
      dplyr::group_by(InternalID) %>>%
      dplyr::summarize(
        ThisInternalID = max(InternalID),
        screentag = dMeasure::semantic_tag(
          tag = as.character(max(n)),
          colour = "green",
          popuphtml = paste0(
            "<p><font size = \'+0\'>",
            dMeasure::paste2(
              DrugName, na.rm = TRUE, collapse = "<br>"
            ),
            "</p>"
          )
        )
      ) %>>%
      dplyr::ungroup() %>>%
      dplyr::mutate(InternalID = ThisInternalID) %>>%
      dplyr::select(InternalID, screentag)
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
