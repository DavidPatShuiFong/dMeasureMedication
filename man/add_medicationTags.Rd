% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{add_medicationTags}
\alias{add_medicationTags}
\title{add tags to a patient list}
\usage{
add_medicationTags(
  patient_list,
  medication_list,
  screentag = FALSE,
  screentag_print = TRUE
)
}
\arguments{
\item{patient_list}{dataframe, must contain column internalID}

\item{medication_list}{dataframe, contains InternalID and DrugName}

\item{screentag}{add fomantic/semantic tags}

\item{screentag_print}{add printable/copyable text-only tags}
}
\value{
dataframe, with added column screentag or screentag_print
}
\description{
add tags to a patient list
}
