# Export functions ---------------------------

#' @export trading_login
#' @export trading_instruments
#' @export trading_md
#' @export trading_mdh
#' @export trading_new_order
#' @export trading_lookup
#' @export trading_orders
#' @export trading_cancel_order
NULL

#' @include s4_object.R
#' NULL

# Primary API Login ---------------------------

#' @title Create rRofex Connection Object
#'
#' @description \code{rRofex_connection} creates a New Connection Object.
#'
#' @param token String. **Mandaroty** Obtained with \code{\link{trading_login}}
#' @param base_url String. **Mandaroty** URL given by  \code{\link{trading_login}} or known by the client.
#'
#' @return Creates a 'rRofex_connection' S4 Object
#'
#' @note You can use accesors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{login_date_time(conn)}
#' }
rRofex_connection <- function(token, base_url) {
  new("rRofexConnection", token = token, base_url = base_url, login_date_time = as.character(Sys.time()))
}

#' @title Primary API Log-In
#'
#' @description \code{trading_login} log in the user into de Primary API
#'
#' @param username String. User Name
#' @param password String. Password
#' @param base_url String. Wich environment are you going to connect:
#' \itemize{
#' \item reMarkets: 'http://api.remarkets.primary.com.ar'
#' \item production: 'https://api.primary.com.ar'
#' \item xOMS: 'https://api.<BROKER>.xoms.com.ar'
#' }
#'
#' Note that reMarkets works with 'http' not 'https'.
#'
#' @note
#' \itemize{
#' \item reMarkets: Testing environment. For credentials go to \url{https://remarkets.primary.ventures}
#' \item production: Production environment. For credentials send an email to \email{mpi@@primary.com.ar}
#' \item xOMS: Ask your broker about it.
#' }
#'
#' @note Accesors:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{login_date_time(conn)}
#' }
#'
#' @return If correct, it will save a hidden token into the current environment
#'
#' @examples
#' \dontrun{conn <- trading_login(username = "pepe", password = "pepino", base_url = "http://api.remarkets.primary.com.ar")}
trading_login <- function(username, password, base_url) {
  if (missing(username) | missing(password)) stop("Username and Password are needed.")
  if (missing(base_url)) stop("BaseURL is needed.")

  url <- glue(base_url, "/auth/getToken")

  query <- tryCatch(POST(url = url,
                         add_headers(.headers = c("X-Username" = username,
                                                  "X-Password" = password)
                                     )
                         ), error = function(cnd) conditionMessage(cnd))

  if (typeof(query) == "list" && status_code(query) != 200) {
    warn_for_status(query)
    NULL
  } else if (typeof(query) == "list" && status_code(query) == 200) {

    message_for_status(query)
    message(glue("

                 Succesfully connected with rRofex to {base_url}..."))
    invisible(rRofex_connection(token = headers(query)$`x-auth-token`, base_url = base_url))

  } else {
    message(glue("Something went wrong...

                 Error: {query}

                 Check function's arguments"))
    NULL
  }
}

# Primary Instruments ---------------------------

#' @title Primary API Insturments
#'
#' @description \code{trading_instruments} list segments and instruments details currently available in Primary API.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param request String. The type of request that you are making:
#' \itemize{
#' \item segments: Available Market Segments
#' \item securities: Available Instruments listed on Rofex
#' }
#' @param sec_detailed Logical. Optional for environment=securities. Brings aditional information like segment, price, minimal/maximal trading quantity, settlement date, etc.
#'
#' @return If correct, it will load a data frame.
#'
#' @examples
#' \dontrun{trading_instruments()}
trading_instruments <- function(connection, request, sec_detailed = FALSE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!request %in% c("segments", "securities")) stop("Invalid 'request' parameter.")

  # Segments
  query <- if (request == 'segments') {
    GET(url = paste0(connection@base_url, "/rest/segment/all"),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
        )
  } else if (request == 'securities' & sec_detailed == F) {
    GET(url = paste0(connection@base_url, "/rest/instruments/all"),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
    )
  } else if (request == 'securities' & sec_detailed == T) {
    GET(url = paste0(connection@base_url, "/rest/instruments/details"),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
    )
  }

  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")

  result <- fromJSON(content(x = query, as = "text"))

  # Return
  data <- if (request == 'segments') {
    result$segments
  } else if (request == 'securities' & sec_detailed == F) {
    flatten(result$instruments)
  } else if (request == 'securities' & sec_detailed == T) {
    flatten(result$instruments)
  }

  return(data)
}

# Market Data ---------------------------

#' @title  Primary API Market Data Real Time
#'
#' @description \code{trading_md} retrivies Market Data in Real Time.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param market_id String. Market to wich you are going to connect.
#' \itemize{
#' \item ROFX. Matba Rofex
#' }
#' @param symbol String. Use \code{\link{trading_instruments}} to see which symbols are available.
#' @param entries Vector of Strings. It contains the information to be required:
#' \itemize{
#' \item BI. Bid.
#' \item OF. Offer.
#' \item LA. Last Available Price.
#' \item OP. Open Price.
#' \item CL. Close Price.
#' \item SE. Settlement Price.
#' \item OI. Open Interest.
#' }
#' @param depth Integer. Depth of the book to be retrivied.
#'
#' @return If correct, it will load a data frame.
#'
#' @examples
#' \dontrun{trading_md(symbol='I.RFX20')}
trading_md <- function(connection, market_id='ROFX', symbol, entries=c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI'), depth=1L) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter.")
  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")

  if (!all(sapply(entries, function(x) x %in% c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI')))) stop("Invalid 'entries' parameter")

  # Base URL
  url <- paste0(connection@base_url, "/rest/marketdata/get")

  # Query
  query <- GET(url = url,
               query = list(
                 marketId=market_id,
                 symbol=symbol,
                 entries=paste0(entries, collapse = ","),
                 depth=depth),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")

  result <- enframe(unlist(content(x = query)$marketData))

  data <- suppressWarnings(result %>%
    separate(col = name, into = c("entries", "type"), sep = '\\.') %>%
    mutate(type = case_when(
      is.na(type) ~ 'value',
      TRUE ~ type
    )))

  return(data)
}

#' @title Primary API Historical Market Data
#'
#' @description \code{trading_mdh} retrivies Historical Trades for a given instrument.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param market_id String. Market to wich we are going to connect.
#' \itemize{
#' \item ROFX. Rofex: Rosario Futures Exchange.
#' \item MATBA. Matba: Mercado a Termino de Buenos Aires.
#' }
#' @param symbol String. Use \code{\link{trading_instruments}} to see which symbols are available.
#' @param date String. Date to be queried. With format '\%Y-\%m-\%d'.
#' @param date_from String. Used together with 'date_to'.
#' @param date_to String. Userd together with 'date_from'.
#'
#' @return If correct, it will load a data frame.
#'
#' @examples
#' \dontrun{trading_mdh(symbol='I.RFX20')}
trading_mdh <- function(connection, market_id='ROFX', symbol, date, date_from, date_to) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter")
  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")
  if (missing(date) & (missing(date_from) | missing(date_to))) stop("Invalid date parameters")

  if (!missing(date)) {
    if (!.validate_fecha(date = date)) stop("The correct format for 'date' is %Y-%m-%d")
  } else {
    if (!missing(date_from) & !.validate_fecha(date = date_from)) stop("The correct format for 'date_from' is %Y-%m-%d")
    if (!missing(date_to) & !.validate_fecha(date = date_to)) stop("The correct format for 'date_to' is %Y-%m-%d")
  }

  # Base URL
  url <- paste0(connection@base_url, "/rest/data/getTrades")

  # Query
  query <- if (!missing(date)) {
    GET(url = url,
        query = list(
          marketId=market_id,
          symbol=symbol,
          date=date
        ),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))
  } else if (!missing(date_from) & !missing(date_to)) {
    GET(url = url,
        query = list(
          marketId=market_id,
          symbol=symbol,
          dateFrom=date_from,
          dateTo=date_to
        ),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))
  }

  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")
  if (!length(content(query)$trades)) stop("There is no data for the product / period selected.")

  result <- fromJSON(content(x = query, as = "text"))

  # Return
  data <- flatten(result$trades)

  return(data)
}

# Orders ---------------------------

#' @title Send Order to the Market
#'
#' @description The method \code{trading_new_order} is use to send orders.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param symbol String. Use \code{\link{trading_instruments}} to see which symbols are available.
#' @param side String. Either 'Buy' or 'Sell'.
#' @param quantity Numeric. Quantity of the order.
#' @param price Numeric. Price of the order.
#' @param order_type String. Type of order.
#' \itemize{
#' \item Limit. Limit order sets the maximum or minimum price at which you are willing to buy or sell.
#' \item MLL. Market with Leftover as Limit (market order then unexecuted quantity becomes limit order at last price).
#' }
#' @param time_in_force String. Specifies how long the order remains in effect. Absence of this field is interpreted as 'Day':
#' \itemize{
#' \item Day. Day or session.
#' \item IOC. Immediate or Cancel.
#' \item FOK. Fill or Kill.
#' \item GTD. Good Till Date.
#' }
#' @param iceberg Logical: if TRUE, then the order is 'iceberg'. FALSE as default.
#' @param expire_date String. \strong{Only for GDT orders}. Maturity date of the order, With format '\%Y-\%m-\%d'.
#' @param display_quantity Numeric. \strong{Only for Iceberg orders}. Indicate the disclosed quantity for the 'iceberg' order.
#' @param account String. Account Number / Account ID.
#' @return List with outputs like state of the order.
trading_new_order <- function(connection, symbol, side, quantity, price, order_type='Limit', time_in_force='Day', iceberg=FALSE, expire_date=NULL, display_quantity=NULL, account) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  market_id <- "ROFX"
  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter")

  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")

  if (missing(side)) stop("You should pick a 'side' to move forward.")
  if (!side %in% c("Buy", "Sell")) stop("Invalid 'side' parameter")

  if (missing(quantity)) stop("You should pick a 'quantity' to move forward.")

  if (order_type == "Limit" & missing(price)) stop("You should pick a 'price' to move forward.")
  if (order_type == "MLL" & missing(price)) {price <- ""}

  if (!order_type %in% c("Limit", "MLL")) stop("Invalid 'order_type' parameter")

  if (!time_in_force %in% c("Day", "IOC", "FOK", "GTD")) stop("Invalid 'time_in_force' parameter")

  if (time_in_force %in% c("GTD") & missing(expire_date)) stop("You should provide an 'expire_date' to move forward.")
  if (!missing(expire_date) & !.validate_fecha(date = expire_date)) {
    stop("The correct format for 'expire_date' is %Y-%m-%d")
  } else if(!missing(expire_date) & .validate_fecha(date = expire_date)) {
    expire_date <- gsub(pattern = "-", replacement = "", x = expire_date)
  }

  if (iceberg == "TRUE" & missing(display_quantity)) stop("You should provide a disclosed quantity")


  if (missing(account)) stop("You should pick a 'account' to move forward.")

  # Query
  query <- GET(url = paste0(connection@base_url, "/rest/order/newSingleOrder"),
               query = list(
                 marketId    = market_id,
                 symbol      = symbol,
                 side        = side,
                 orderQty    = quantity,
                 price       = price,
                 ordType     = if (order_type == "Limit") {"Limit"} else if (order_type == "MLL") {"Market_to_limit"},
                 timeInForce = time_in_force,
                 iceberg     = iceberg,
                 expireDate  = expire_date,
                 displayQty  = if (iceberg == F) {NULL} else {display_quantity},
                 account     = account
               ),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")

  # Result
  result <- if (content(query)$status == "OK") {
    trading_lookup(lookup_type = "COID",
                   order_id = content(query)$order$clientId,
                   proprietary =content(query)$order$proprietary)
  } else {
    content(query)
  }

  return(result)
}

#' @title Cancel Order Sent to the Market
#'
#' @description The method \code{trading_cancel_order} is use to send orders.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param order_id String. clOrdId given by the \code{trading_orders} method.
#' @param proprietary String. ID given by the \code{trading_orders} method.
#'
#' @return List with outputs like state of the order.
trading_cancel_order <- function(connection, order_id, proprietary) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(order_id)) stop("You should pick a 'order_id' to move forward.")
  if (missing(proprietary)) stop("You should pick a 'proprietary' to move forward.")

  # Query
  query <- GET(url = paste0(connection@base_url, "/rest/order/cancelById"),
               query = list(
                 clOrdId     = order_id,
                 proprietary = proprietary
               ),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  # Results
  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")

  result <- if (query$status_code == 200 & content(query)$status == "OK") {
    "The order has been canceled!"
  }

  return(result)
}

# Orders Lookup ---------------------------

#' @title Lookup Order Status
#'
#' @description The method \code{trading_lookup} is used to check the satus of an order.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param lookup_type String. Look-up by:
#' \itemize{
#' \item COID. Client Order ID.
#' \item OID. Order ID. (Not Available)
#' }
#' @param order_id String. ID given by the \code{trading_orders} method.
#' @param proprietary String. ID given by the \code{trading_orders} method.
#'
#' @return A data frame.
trading_lookup <- function(connection, lookup_type, order_id, proprietary) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(lookup_type)) stop("You should pick a 'lookup_type' to move forward.")
  if (!lookup_type %in% c("COID", "OID")) stop("Invalid 'lookup_type' parameter")

  if (missing(order_id)) stop("You should pick a 'order_id' to move forward.")

  if (lookup_type == "COID" & missing(proprietary)) stop("You should pick a 'proprietary' to move forward.")

  # Query
  query <- if (lookup_type == "COID") {
    GET(url = paste0(connection@base_url, "/rest/order/id"),
        query = list(
          clOrdId     = order_id,
          proprietary = proprietary
        ),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))
  } else if (lookup_type == "OID") {
    # paste0(.rRofexGlobalEnv$base_url, "/rest/order/cancelById")
  }

  # Results
  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")

  result <- if (query$status_code == 200 & content(query)$status == "OK") {
    list(
      ClientId   = content(query)$order$clOrdId,
      State      = content(query)$order$status,
      text       = content(query)$order$text
    )
  } else {
    list(
      ClientId   = "-",
      State      = "-",
      text       = "-"
    )
  }

  return(result)
}

#' @title View Orders
#'
#' @description The method \code{trading_orders} is used to see each order sent by Account.
#'
#' @param connection S4. **Mandaroty** Formal rRofexConnection class object
#' @param account String. Account Number / Account ID.
#'
#' @return A data frame.
trading_orders <- function(connection, account) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(account)) stop("You should pick an 'account' to move forward.")

  # Query
  query <- GET(url = paste0(connection@base_url, "/rest/order/all"),
               query = list(
                 accountId = account
               ),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  # Results
  if (query$status_code != 200 | content(query)$status != "OK") stop("The query returned an unexpected result.")

  result <- if (query$status_code == 200 & content(query)$status == "OK") {
    fromJSON(content(x = query, as = "text"))
  }

  return(result$orders)
}
