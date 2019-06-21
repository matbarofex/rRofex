# Helper Functions ---------------------------
.validate_fecha <- function(fecha) {
  tryCatch({!is.na(format.Date(x = fecha, "%Y-%m-%d"))}, error = function(e) {FALSE})
}
