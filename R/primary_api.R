# Export functions ---------------------------

#' @export trading_login
#' @export trading_instruments
#' @export trading_md
#' @export trading_mdh
NULL

# Primary API Login ---------------------------

#' Primary API LogIn
#'
#'\code{trading_login} log in the user into de Primary API
#'
#'@param username User Name
#'@param password Password
#'@param env String. Wich environment are you going to connect
#'\itemize{
#'\item reMarkets: Testing environment. For credentials go to \url{https://remarkets.primary.ventures}
#'\item production: Production environment. For credentials send an email to \email{mpi@@primary.com.ar}
#'}
#'
#'@return If correct, it will save a hidden token into the current environment
#'
#'@examples
#'\dontrun{trading_login(username="pepe", password="pepino", env="reMarkets")}
trading_login <- function(username, password, env="reMarkets") {
  if (missing(username) | missing(password)) stop("Username and Password are needed.")
  if (missing(env)) stop("Environment is needed.")
  if (!env %in% c("reMarkets", "production")) stop("Environrment is invalid.")


  # Environment
  .base_url <<- if (env == 'reMarkets') {
    "http://pbcp-remarket.cloud.primary.com.ar"
    } else if (env == 'production') {
      "https://api.primary.com.ar"
    }

  url <- paste0(.base_url, "/auth/getToken")

  token <- POST(url = url,
                add_headers(.headers = c("X-Username" = username,
                                         "X-Password" = password)
                            )
                )

  head <- headers(token)

  active_token <- head$`x-auth-token`

  if (!is.null(active_token)) {
    .x_auth_token <<- active_token
    message("Connected Successfully")
  } else {
    warning("Something went wrong... =/")
  }

}

# Primary Instruments ---------------------------

#' Primary API Insturments
#'
#'\code{trading_instruments} list segments and instruments details currently available in Primary API.
#'
#'@param request The type of request that you are making:
#'\itemize{
#'\item segments: Available Market Segments
#'\item securities: Available Instruments listed on Rofex
#'}
#'@param sec_detailed Logical. Optional for environment=securities. Brings aditional information like segment, price, minimal/maximal trading quantity, settlement date, etc.
#'
#'@return If correct, it will load a data frame.
#'
#'@examples
#'\dontrun{trading_instruments()}
trading_instruments <- function(request, sec_detailed = FALSE) {
  if (!exists(".x_auth_token")) stop("You should first log in using primary_login()")
  if (!request %in% c("segments", "securities")) stop("Invalid 'request' parameter.")

  # Segments
  query <- if (request == 'segments') {
    GET(url = paste0(.base_url, "/rest/segment/all"),
        add_headers(.headers = c("X-Auth-Token" = .x_auth_token))
        )
  } else if (request == 'securities' & sec_detailed == F) {
    GET(url = paste0(.base_url, "/rest/instruments/all"),
        add_headers(.headers = c("X-Auth-Token" = .x_auth_token))
    )
  } else if (request == 'securities' & sec_detailed == T) {
    GET(url = paste0(.base_url, "/rest/instruments/details"),
        add_headers(.headers = c("X-Auth-Token" = .x_auth_token))
    )
  }

  result <- fromJSON(content(x = query, as = "text"))

  if (result$status != 'OK') stop("The query returned an unexpected result.")

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

# Market Data Real Time ---------------------------

#' Primary API Market Data Real Time
#'
#'\code{trading_md} retrivies Market Data in Real Time.
#'
##'@param market_id String. Market to wich you are going to connect.
#'\itemize{
#'\item ROFX. Matba Rofex
#'}
#'@param symbol String. Use \code{\link{primary_instruments}} to see which symbols are available.
#'@param entries Vector of Strings. It contains the information to be required:
#'\itemize{
#'\item BI. Bid.
#'\item OF. Offer.
#'\item LA. Last Available Price.
#'\item OP. Open Price.
#'\item CL. Close Price.
#'\item SE. Settlement Price.
#'\item OI. Open Interest.
#'}
#'@param depth Integer. Depth of the book to be retrivied.
#'
#'@return If correct, it will load a data frame.
#'
#'@examples
#'\dontrun{trading_md(symbol='I.RFX20')}
trading_md <- function(market_id='ROFX', symbol, entries=c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI'), depth=1L) {
  if (!exists(".x_auth_token")) stop("You should first log in using primary_login()")
  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter.")
  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")

  # Base URL
  url <- paste0(.base_url, "/rest/marketdata/get")

  # Query
  query <- GET(url = url,
               query = list(
                 marketId=market_id,
                 symbol=symbol,
                 entries=paste0(entries, collapse = ","),
                 depth=depth),
               add_headers(.headers = c("X-Auth-Token" = .x_auth_token)))

  if (content(query)$status != "OK") stop("The query returned an unexpected result.")

  result <- enframe(unlist(content(x = query)$marketData))

  data <- suppressWarnings(result %>%
    separate(col = name, into = c("entries", "type"), sep = '\\.') %>%
    mutate(type = case_when(
      is.na(type) ~ 'value',
      TRUE ~ type
    )))

  return(data)
}

# Historical Market Data ---------------------------

#' Primary API Historical Market Data
#'
#'\code{trading_mdh} retrivies Historical Trades for a given instrument.
#'
#'@param market_id String. Market to wich we are going to connect.
#'\itemize{
#'\item ROFX. Rofex: Rosario Futures Exchange.
#'\item MATBA. Matba: Mercado a Termino de Buenos Aires.
#'}
#'@param symbol String. Use \code{\link{primary_instruments}} to see which symbols are available.
#'@param date String. Date to be queried. With format '%Y-%m-%d'
#'@param date_from String. Used together with 'date_to'.
#'@param date_to String. Userd together with 'date_from'.
#'
#'@return If correct, it will load a data frame.
#'
#'@examples
#'\dontrun{trading_mdh(symbol='I.RFX20')}
trading_mdh <- function(market_id='ROFX', symbol, date, date_from, date_to) {
  if (!exists(".x_auth_token")) stop("You should first log in using primary_login()")
  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter")
  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")
  if (missing(date) & (missing(date_from) | missing(date_to))) stop("Invalid date parameters")

  validate_fecha <- function(fecha) {
    tryCatch({!is.na(format.Date(x = fecha, "%Y-%m-%d"))}, error = function(e) {FALSE})
  }

  if (!missing(date)) {
    if (!validate_fecha(fecha = date)) stop("The correct format for 'date' is %Y-%m-%d")
  } else {
    if (!missing(date_from) & !validate_fecha(fecha = date_from)) stop("The correct format for 'date_from' is %Y-%m-%d")
    if (!missing(date_to) & !validate_fecha(fecha = date_to)) stop("The correct format for 'date_to' is %Y-%m-%d")
  }

  # Base URL
  url <- paste0(.base_url, "/rest/data/getTrades")

  # Query
  query <- if (!missing(date)) {
    GET(url = url,
        query = list(
          marketId=market_id,
          symbol=symbol,
          date=date
        ),
        add_headers(.headers = c("X-Auth-Token" = .x_auth_token)))
  } else if (!missing(date_from) & !missing(date_to)) {
    GET(url = url,
        query = list(
          marketId=market_id,
          symbol=symbol,
          dateFrom=date_from,
          dateTo=date_to
        ),
        add_headers(.headers = c("X-Auth-Token" = .x_auth_token)))
  }

  if (content(query)$status != "OK") stop("The query returned an unexpected result.")
  if (!length(content(query)$trades)) stop("There is no data for the product / period selected.")

  result <- fromJSON(content(x = query, as = "text"))

  # Return
  data <- flatten(result$trades)

  return(data)
}



