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
#' @param destination String. Name of the tibble where the data is going to be stored.
#' @param symbol String. \strong{Mandatory}. Use \code{\link{trading_instruments}} to see which symbols are available.
#' @param entries List of Strings. It contains the information to be queried:
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
#' @param listen_to List. Column names to by listen to.
#'
#' @return If correct, it will load a tibble.
#'
#' @family websocket functions
#'
#' @examples
#'
#' # To create simultaneously many connections
#'
#' \dontrun{
#' purrr::walk2(
#' .x = symbols,
#' .y = tickers,
#' .f = ~ trading_ws_md(connection = conn, destination = .y, symbol = .x)
#' )
#' }
trading_ws_md <- function(connection, destination, symbol, entries=list('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI', 'HI', 'LO', 'TV', 'IV', 'EV', 'NV', 'TC'), listen_to = NA, market_id='ROFX') {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter.")

  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")
  if (length(symbol) > 1) stop("'symbol' parameter can not be more than one.")

  if (class(entries) != "list") stop("'entries' must be a list")
  if (some(entries, ~ !.x %in% c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI', 'HI', 'LO', 'TV', 'IV', 'EV', 'NV', 'TC'))) stop("'entries' parameter is invalid. See documentation.")

  if (!missing(listen_to) && class(listen_to) != "list") stop("'listen_to' must be either a list or NA.")

  if (!exists(destination, envir = parent.frame(), inherits = FALSE)) {
    assign(x = destination, value = NULL, envir = parent.frame())
  }

  if (!exists("rRofexWebsockets", mode = "environment", where = parent.frame(), inherits = FALSE)) {
    assign(x = "rRofexWebsockets", value = new.env(parent = emptyenv()), envir = parent.frame())
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
    message(glue("Websocket '{destination}' disconnected from {url} with code {code} and reason '{reason}'",
                 url = connection@base_url,
                 code = event$code,
                 reason = event$reason,
                 destination = destination))
    if (event$code == 1006) {
      trading_ws_md(connection = connection, destination = destination, symbol = symbol, entries = entries, market_id = market_id)
    }
  })

  ws$onError(function(event) {
    message(glue("Something went wrong, here's the error:
                 {event$message}"))
  })

  ws$onMessage(function(event) {
    fromJSON(event$data)[c("timestamp", "marketData")] %>%
      modify_depth(.x = ., .depth = 2, .f = function(x) if (is_null(x)) NA else x) %>%
      modify_depth(.x = ., .depth = 2, .f = function(x) if (class(x) == "list" && is_null(unlist(x))) NA else x) %>%
      unlist(x = ., recursive = T, use.names = T) %>%
      as_tibble_row() %>%
      mutate(timestamp = as.POSIXct(timestamp/1000, origin = "1970-01-01", tz = "America/Buenos_Aires")) %>%
      mutate_at(.tbl = ., .vars = vars(matches(".date")), .funs = list(~ as.POSIXct(x = unlist(.)/1000, origin = "1970-01-01", tz = "America/Buenos_Aires"))) %>%
      rename_all(.tbl = ., .funs = list(~ gsub(pattern = "marketData\\.", replacement = "", x = .))) %>%
      rename_all(.tbl = ., .funs = list(~ gsub(pattern = "\\.", replacement = "_", x = .))) %>%
      mutate(Symbol = symbol, Changes = "") %>%
      bind_rows(get(x = destination, envir = parent.frame()), .) %>%
      assign_in(where = list("Changes", nrow(.)),
                value = ifelse(nrow(.) > 1,
                               glue_collapse(colnames(select(., -c("timestamp", "Changes")))[which(
                                 replace_na(slice(., nrow(.)) %>% select(-c("timestamp", "Changes")) != slice(., nrow(.) - 1) %>% select(-c("timestamp", "Changes")), replace = TRUE)
                                 )], sep = ","),
                               glue_collapse(colnames(select(., -c("timestamp", "Changes"))), sep = ","))
                ) %>%
      filter(if (any(!is.na(listen_to))) {map_lgl(Changes, .f = ~ any(strsplit(.x, ",") %>% pluck(1) %in% listen_to))} else {!is.na(Changes)}) %>%
      distinct_at(.tbl = ., .vars = vars(-timestamp), .keep_all = TRUE) %>%
      assign(x = destination, value = ., envir = parent.frame())
  })

  assign(x = destination, value = ws, envir = rRofexWebsockets)

}

#' @title Web Sockets: Close connection
#'
#' @description This method it is use to close open Websocket connections.
#'
#' @param close_all Logical. Should all connections be closed or only the selected ones.
#' @param selection List. Is the same name that you have chosen for destination in \code{\link{trading_ws_md}}
#'
#' @return If correct, it will show a message saying that the connection has been closed.
#'
#' @family websocket functions
#'
#' @examples
#'
#' # To close all connections at once
#'
#' \dontrun{
#' trading_ws_close(close_all = TRUE)
#' }
trading_ws_close <- function(close_all=FALSE, selection) {
  if (!exists("rRofexWebsockets", mode = "environment")) stop("There is no rRofexWebsockets environment created.")
  if (is_empty(ls(rRofexWebsockets))) stop("rRofexWebsockets is empty.")

  if (close_all == TRUE) {
    walk(.x = ls(rRofexWebsockets), .f = function(x) {
      get(x = x, envir = rRofexWebsockets)$close()
      rm(list = x, envir = rRofexWebsockets)
    })
  } else {
    if (missing(selection) || (!missing(selection) && class(selection) != "list")) stop("'selection' must be a not empty list.")
    walk(.x = selection, .f = function(x) {
      if (x %in% ls(rRofexWebsockets)) {
        get(x = x, envir = rRofexWebsockets)$close()
        rm(list = x, envir = rRofexWebsockets)
        } else {
          message(glue("'", x, "' it isn't present on the environment."))
        }
      })
    }
}

