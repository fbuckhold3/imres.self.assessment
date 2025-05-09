% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{handle_s_eval_submission}
\alias{handle_s_eval_submission}
\title{Handle the submission of s_eval fields}
\usage{
handle_s_eval_submission(fields_to_collect, record_id)
}
\arguments{
\item{fields_to_collect}{Character vector of field names to submit}

\item{record_id}{The REDCap record_id to which these belong}
}
\value{
TRUE on success, FALSE (with notification) on failure
}
\description{
Handle the submission of s_eval fields
}
