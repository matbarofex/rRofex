# Export functions ---------------------------

#' @export trading_ws_md
#' @export trading_ws_close

#' @include s4_object.R
#' NULL

#' @title Web Sockets: Market Data Real Time
#'
#' @description This method brings Market Data in Real Time using web socket protocol.
#'
#' @param connection S4. \strong{Mandatory} Formal rRofexConnection class object
#' @param websocket_name String. Name chosen for the connection. It is use to locate the connection.
#' @param destination String. Name of the tibble where the data is going to be stored.
#' @param symbol String. \strong{Mandatory}. Use \code{\link{trading_instruments}} to see which symbols are available.
#' @param entries Vector of Strings. It contains the information to be queried:
#' \itemize{
#' \item \strong{BI} - Bid.
#' \item \strong{OF} - Offer.
#' \item \strong{LA} - Last Available Price.
#' \item \strong{OP} - Open Price.
#' \item \strong{CL} - Close Price.
#' \item \strong{SE} - Settlement Price.
#' \item \strong{OI} - Open Interest.
#' \item \strong{HI} - Trading Session High Price
#' \item \strong{LO} - Trading Session Low Price
#' \item \strong{TV} - Trading Volume
#' \item \strong{IV} - Index Value
#' \item \strong{EV} - Trading Effective Volume
#' \item \strong{NV} - Nominal Volume
#' \item \strong{TC} - Trade Count
#' }
#' @param market_id String. Market to which you are going to connect.
#'
#' @return If correct, it will load a tibble.
#'
#' @family account functions
trading_ws_md <- function(connection, websocket_name, destination, symbol, entries=c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI', 'HI', 'LO', 'TV', 'IV', 'EV', 'NV', 'TC'), market_id='ROFX') {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter.")

  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")
  if (length(symbol) > 1) stop("'symbol' parameter can not be more than one.")

  if (some(entries, ~ !.x %in% c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI', 'HI', 'LO', 'TV', 'IV', 'EV', 'NV', 'TC'))) stop("'entries' parameter is invalid. See documentation.")

  if (!exists(destination, envir = .GlobalEnv, inherits = FALSE)) {
    assign(x = destination, value = NULL, envir = .GlobalEnv)
  }

  ws <- WebSocket$new(url = gsub(pattern = "(.+)(:.+)", replacement = "wss\\2/", x = connection@base_url),
                      headers = list("X-Auth-Token" = connection@token),
                      accessLogChannels = "none",
                      errorLogChannels = "none",
                      autoConnect = TRUE)

  ws$onOpen(function(event){
    message(glue("Client connected with rRofex using websockets to {connection@base_url}..."))
    ws$send(toJSON(list(type = "smd", level = 1, entries = entries, products = list(list(symbol = symbol, marketId = market_id))), auto_unbox = T))
  })

  ws$onClose(function(event) {
    message(glue("Websocket disconnected from {url} with code {code} and reason '{reason}'",
                 url = connection@base_url,
                 code = event$code,
                 reason = event$reason))
    if (event$code == 1006) {
      trading_ws_md(connection = connection, websocket_name = websocket_name, destination = destination, symbol = symbol, entries = entries, market_id = market_id)
    }
  })

  ws$onError(function(event) {
    message(glue("Something went wrong, here's the error:
                 {event$message}"))
  })

  ws$onMessage(function(event) {
    fromJSON(event$data)[c("timestamp", "marketData")] %>%
      unlist(x = ., recursive = T, use.names = T) %>%
      as_tibble_row() %>%
      mutate(timestamp = as.POSIXct(timestamp/1000, origin = "1970-01-01", tz = "America/Buenos_Aires")) %>%
      mutate_at(.tbl = ., .vars = vars(matches(".date")), .funs = list(~ as.POSIXct(x = unlist(.)/1000, origin = "1970-01-01", tz = "America/Buenos_Aires"))) %>%
      rename_all(.tbl = ., .funs = list(~ gsub(pattern = "marketData\\.", replacement = "", x = .))) %>%
      rename_all(.tbl = ., .funs = list(~ gsub(pattern = "\\.", replacement = "_", x = .))) %>%
      bind_rows(get(x = destination, envir = .GlobalEnv), .) %>%
      distinct() %>%
      mutate(Symbol = symbol) %>%
      assign(x = destination, value = ., envir = .GlobalEnv)
  })

  assign(x = websocket_name, value = ws, envir = .GlobalEnv)

}

#' WS: Close connection
#'
#' @param websocket_connections web socket that is going to be closed.
trading_ws_close <- function(websocket_connection) {
  get(x = websocket_connection)$close()
}

