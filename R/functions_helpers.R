# Helper Functions ---------------------------

#' @title Helper: Date validation
#'
#' @description Validate date
#'
#' @param date Date
#' @return TRUE if date has a correct format.
.validate_fecha <- function(date) {
  tryCatch({!is.na(format.Date(x = date, "%Y-%m-%d"))}, error = function(e) {FALSE})
}
