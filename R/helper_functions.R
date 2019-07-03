# Helper Functions ---------------------------
# If date invalid, returns FALSE
.validate_fecha <- function(date) {
  tryCatch({!is.na(format.Date(x = date, "%Y-%m-%d"))}, error = function(e) {FALSE})
}
