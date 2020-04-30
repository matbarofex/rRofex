# Export functions ---------------------------

#' @export trading_login
#' @export trading_instruments
#' @export trading_instruments_fronts
#' @export trading_md
#' @export trading_mdh
#' @export trading_currencies
#' @export trading_new_order
#' @export trading_lookup
#' @export trading_orders
#' @export trading_cancel_order
#' @export trading_account
#' @export trading_account_report

NULL

#' @include s4_object.R
#' NULL

# Primary API Login ---------------------------

#' @title Create rRofex Connection Object
#'
#' @description \code{rRofex_connection} creates a New Connection Object.
#'
#' @param token String. \strong{Mandaroty} Obtained with \code{\link{trading_login}}
#' @param base_url String. \strong{Mandaroty} URL given by  \code{\link{trading_login}} or known by the client.
#'
#' @return S4 rRofexConnection object.
#'
#' @note You can use accesors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{login_date_time(conn)}
#' }
#'
#' @family connection functions
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
#' @return S4 rRofexConnection object.
#'
#' @note Accesors:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{login_date_time(conn)}
#' }
#'
#' @family connection functions
#'
#' @examples
#' \dontrun{
#' conn <- trading_login(username = "pepe", password = "pepino", base_url = "http://api.remarkets.primary.com.ar")
#' }
trading_login <- function(username, password, base_url) {
  if (missing(username) || missing(password)) stop("Username and Password are needed.")
  if (missing(base_url)) stop("'base_url' is needed.")

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
    message(glue("Succesfully connected with rRofex to {base_url}..."))

    invisible(rRofex_connection(token = headers(query)$`x-auth-token`, base_url = base_url))

  } else {
    message(glue("Something went wrong...

                 Error: {query}

                 Check function's arguments"))
    NULL
  }
}

# Primary Instruments ---------------------------

#' @title List of Instruments
#'
#' @description Method to list segments and instruments currently available through the Trading API.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param request String. \strong{Mandaroty} The type of request that you are making:
#' \itemize{
#' \item \strong{segments}: List available market segments
#' \item \strong{securities}: List available instruments listed on Matba Rofex. \emph{Depends on 'sec_detailed'}.
#' \item \strong{by_segment}: List available instruments searching by market segment. \emph{Depends on 'market_id' and 'segment_id'}
#' \item \strong{by_cfi_code}: List available instruments searching by CFI Code. \emph{Depends on 'cfi_code'}
#' \item \strong{by_type}: List available instruments searching by Instrument Type. See section Instrument Types. \emph{Depends on 'sec_detailed' and 'sec_type'}.
#' }
#' @param sec_detailed Logical. Optional for request='securities'. Brings aditional information like segment, price, minimal/maximal trading quantity, settlement date, etc.
#' @param market_id String. Needed for request='by_segment'. Market ID.
#' \itemize{
#' \item \strong{ROFX}: Matba Rofex
#' }
#' @param segment_id String. Needed for request='by_segment'. Market Segment ID.
#' \itemize{
#' \item \strong{DDF}: Financial Derivatives
#' \item \strong{DDA}: Agropecuarial Derivatives
#' \item \strong{DUAL}: Other Derivatives
#' \item \strong{MERV}: S&P Merval
#' }
#' @param cfi_code String. Needed for request='by_cfi_code'. CFI Code. See \url{https://www.quotemedia.com/apifeeds/cfi_code}
#' @param sec_type String. Needed for request='by_type'.
#' \itemize{
#' \item \strong{E}: Equities
#' \item \strong{D}: Debt
#' \item \strong{C}: Collective Investment Vehicles
#' \item \strong{R}: Entitlements (Rights)
#' \item \strong{O}: Listed Options
#' \item \strong{F}: Futures
#' \item \strong{T}: Referencial Instruments
#' \item \strong{M}: Others
#' }
#'
#' @return If correct, it will load a tibble data frame
#'
#' @family reference data functions
trading_instruments <- function(connection, request, sec_detailed = FALSE, market_id = "ROFX", segment_id, cfi_code, sec_type) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(request)) stop("'request' parameter is required.")
  if (some(request, ~ !.x %in% c("segments", "securities", "by_segment", "by_cfi_code", "by_type"))) stop("'request' parameter is invalid. See documentation.")
  if (length(request) > 1) stop("'request' parameter can not be more than one.")

  if (request == 'by_segment' && some(market_id, ~ !.x %in% c("ROFX"))) stop("'market_id' parameter is invalid. See documentation.")
  if (request == 'by_segment' && missing(segment_id)) stop("'segment_id' parameter is required when searching by Segment.")
  if (request == 'by_segment' && length(segment_id) > 1) stop("'segment_id' parameter can not be more than one.")
  if (request == 'by_segment' && some(segment_id, ~ !.x %in% c("DDF", "DDA", "DUAL", "MERV"))) stop("'segment_id' parameter is invalid. See documentation.")

  if (request == 'by_cfi_code' && missing(cfi_code)) stop("'cfi_code' parameter is required when searching by CFI Code.")
  if (request == 'by_cfi_code' && length(cfi_code) > 1) stop("'cfi_code' parameter can not be more than one.")

  if (request == 'by_type' && missing(sec_type)) stop("'sec_type' parameter is required when searching by Type")
  if (request == 'by_type' && some(sec_type, ~ !.x %in% c("E", "D", "C", "R", "O", "F", "T", "M"))) stop("'sec_type' parameter is invalid. See documentation.")

  # Query
  query <- if (request == 'segments') {
    GET(url = glue(connection@base_url, "/rest/segment/all"),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
        )
  } else if (request %in% c('securities', 'by_type') & sec_detailed == F) {
    GET(url = glue(connection@base_url, "/rest/instruments/all"),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
    )
  } else if (request %in% c('securities', 'by_type') & sec_detailed == T) {
    GET(url = glue(connection@base_url, "/rest/instruments/details"),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
    )
  } else if (request == 'by_segment') {
    GET(url = glue(connection@base_url, "/rest/instruments/bySegment"),
        query = list(
          MarketID = market_id,
          MarketSegmentID = segment_id),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
    )
  } else if (request == 'by_cfi_code') {
    GET(url = glue(connection@base_url, "/rest/instruments/byCFICode"),
        query = list(
          CFICode = cfi_code),
        add_headers(.headers = c("X-Auth-Token" = connection@token))
    )
  }

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else {

    data <- fromJSON(toJSON(content(query)))

    if (request == 'segments') {

      data <- data$segments %>%
        mutate_all(., unlist)

    } else if (request %in% c('securities', 'by_type') & sec_detailed == F) {

      data <- data$instruments %>%
        jsonlite::flatten(recursive = F) %>%
        mutate_all(., unlist) %>%
        rename_all(., .funs = list(~gsub(pattern = ".+\\.", replacement = "", x = .)))

    } else if (request %in% c('securities', 'by_type') & sec_detailed == T) {

      data <- data$instruments %>%
        jsonlite::flatten(x = ., recursive = F) %>%
        mutate_all(., ~ replace_na(., replace = NA)) %>%
        mutate_all(., unlist) %>%
        select(-segment.marketId) %>%
        rename_all(., .funs = list(~gsub(pattern = ".+\\.", replacement = "", x = .))) %>%
        mutate(maturityDate = as.Date(maturityDate, format = "%Y%m%d"))

    } else if (request == 'by_segment') {

      data <- data$instruments %>%
        mutate_all(., unlist)

    } else if (request == 'by_cfi_code') {

      data <- data$instruments %>%
        mutate_all(., unlist)
    }

    if (request == 'by_type') {
      data <- data %>%
        filter(grepl(glue("^[", glue_collapse(sec_type, sep = "|"), "]"), cficode)) %>%
        arrange(cficode)
    }

    data <- data %>%
      rename_all(., .funs = list(~gsub(pattern = "(^.)", replacement = "\\U\\1", x = ., perl = TRUE))) %>%
      as_tibble()

    if (request %in% c('securities', 'by_type') & sec_detailed != F) {
      suppressWarnings(data <- data %>%
                         mutate(
                           ProductType = factor(gsub(pattern = "(^.)(.+)", replacement = "\\1", x = Cficode), levels = c("E", "D", "C", "R", "O", "F", "T", "M"), labels = c("Equities", "Debt", "Collective Investment Vehicles", "Entitlements", "Options", "Futures", "Referencial Instruments", "Others")),
                           Settlement = case_when(
                             grepl(pattern = ".+ - (.+[hs|CI|D])$", x = Symbol) == TRUE ~ trimws(gsub(pattern = ".+ - (.+)$", replacement = "\\1", x = Symbol, ignore.case = T), which = "both"),
                             TRUE ~ NA_character_
                             ),
                           OptionType = factor(gsub(pattern = "(^.{2})(.+)", replacement = "\\1", x = Cficode), levels = c("OC", "OP", "OM"), labels = c("Call", "Put", "Others")),
                           Ticker = case_when(
                             ProductType %in% c('Equities', 'Debt', 'Options') ~ trimws(gsub(pattern = "(MERV - XMEV - )(.+)( - .+)", replacement = "\\2", x = Symbol, ignore.case = T), which = "both"),
                             ProductType %in% c('Entitlements') ~ trimws(gsub(pattern = "(MERV - XMEV - )(.+)", replacement = "\\2", x = Symbol, ignore.case = T), which = "both"),
                             ProductType %in% c('Others') ~ trimws(gsub(pattern = "(.+)( - )(.+)$", replacement = "\\3", x = Symbol, ignore.case = T), which = "both"),
                             TRUE ~ Symbol
                             ),
                           Underlying = case_when(
                             ProductType %in% c('Options') & MarketSegmentId == 'MERV' ~ trimws(gsub(pattern = "(.{3})(.+)", replacement = "\\1", x = Ticker, ignore.case = T), which = "both"),
                             ProductType %in% c('Options') & MarketSegmentId != 'MERV'~ trimws(gsub(pattern = "(.)?([[:alpha:]]{3})?([0-9]{2})?(/)?([0-9]{2})?( )([0-9]+)([p|c])$", replacement = "\\1", x = Symbol, ignore.case = T), which = "both"),
                             ProductType %in% c('Futures') & grepl(pattern = "(Dispo)$", x = Ticker) == TRUE ~ trimws(gsub(pattern = "(.+)(Dispo)$", replacement = "\\1", x = Symbol, ignore.case = T), which = "both"),
                             ProductType %in% c('Futures') ~ trimws(gsub(pattern = "(.)?([[:alpha:]]{3})?([0-9]{2})?(/)?([0-9]{2})?( )?((A|M)||([0-9]){2})$", replacement = "\\1", x = Symbol, ignore.case = T), which = "both"),
                             TRUE ~ NA_character_
                             ),
                           StrikePrice = case_when(
                             ProductType %in% c('Options') & MarketSegmentId == 'MERV' ~ as.double(gsub(pattern = "(.{4})([0-9]+?\\.[0-9]*)([[:alpha:]]+)", replacement = "\\2", x = Ticker, ignore.case = T)),
                             ProductType %in% c('Options') & MarketSegmentId != 'MERV'~ as.double(gsub(pattern = "(.+)( )([0-9]+)([p|c])$", replacement = "\\3", x = Symbol, ignore.case = T)),
                             TRUE ~ NA_real_
                             )
                           ) %>%
                         select(Symbol, ProductType, MarketSegmentId, Ticker, OptionType, StrikePrice, Underlying, Settlement, MaturityDate, Cficode, everything()))
      }

  }

  return(data)

}


#' @title Front Month of Futures
#'
#' @description List all front month contracts for futures.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#'
#' @return If correct, it will load a tibble data frame
#'
#' @family reference data functions
trading_instruments_fronts <- function(connection) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  data <- trading_instruments(connection = connection, request = "by_type", sec_type = "F", sec_detailed = T)

  if (!is_null(data)) {
    data <- data %>%
      group_by(Underlying) %>%
      arrange(MaturityDate) %>%
      summarise(MaturityDate = first(MaturityDate), Symbol = first(Symbol))
  } else {
    data <- NULL
  }

  return(data)

}

# Market Data ---------------------------

#' @title  Market Data Real Time
#'
#' @description This method brings Market Data in Real Time.
#'
#' @param connection S4. \strong{Mandaroty}. Formal rRofexConnection class object
#' @param market_id String. Market to wich you are going to connect.
#' \itemize{
#' \item \strong{ROFX} - Matba Rofex
#' }
#' @param symbol String. \strong{Mandaroty}. Use \code{\link{trading_instruments}} to see which symbols are available.
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
#'
#' }
#' @param depth Integer. Depth of the book.
#' @param tidy Logical. Data arranged on a tidy format.
#'
#' @return If correct, it will load a tibble data frame
#'
#' @family market data functions
trading_md <- function(connection, market_id='ROFX', symbol, entries=c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI', 'HI', 'LO', 'TV', 'IV', 'EV', 'NV'), depth = 1L, tidy = FALSE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!market_id %in% c("ROFX")) stop("Invalid 'market_id' parameter.")

  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")

  if (some(entries, ~ !.x %in% c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI', 'HI', 'LO', 'TV', 'IV', 'EV', 'NV'))) stop("'entries' parameter is invalid. See documentation.")

  # Query
  query <- GET(url = glue(connection@base_url, "/rest/marketdata/get"),
               query = list(
                 marketId   =     market_id,
                 symbol     =     symbol,
                 entries    =     glue_collapse(entries, sep = ","),
                 depth      =     depth),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else {

    if (tidy == TRUE) {

      data <- fromJSON(toJSON(content(query), auto_unbox = T, null = "null"))

      data <- data$marketData %>%
        enframe() %>%
        mutate(value = map(.x = value, function(x) if(is_null(x)) {NA_real_} else {x})) %>%
        pivot_wider() %>%
        mutate_if(., .predicate = ~ class(.[[1]]) == 'list', .funs = ~ modify_depth(.x = ., .depth = 1, ~ replace_na(data = ., replace = NA_real_))) %>%
        mutate_if(., .predicate = ~ length(unlist(.)) == 1, .funs =  ~ unlist(x = ., recursive = F)) %>%
        mutate_if(., .predicate = ~ class(.) == 'list', .funs = ~ modify_depth(.x = ., .depth = 1, ~ as_tibble(.))) %>%
        mutate_if(., .predicate = ~ class(.) == 'list', .funs = ~ modify_depth(.x = ., .depth = 1, ~ mutate_at(.tbl = ., .vars = vars(matches("date")), .funs = ~ as.POSIXct(./1000, origin = "1970-01-01", tz = "America/Buenos_Aires")))) %>%
        mutate_if(., .predicate = ~ class(.) == 'list', .funs = ~ modify_depth(.x = ., .depth = 1, ~ mutate_at(.tbl = ., .vars = vars(matches("price")), .funs = ~ as.double(.)))) %>%
        mutate_if(., .predicate = ~ class(.) == 'list', .funs = ~ modify_depth(.x = ., .depth = 1, ~ mutate_at(.tbl = ., .vars = vars(matches("size")), .funs = ~ as.double(.))))

    } else {
      result <- enframe(unlist(content(x = query)$marketData))

      data <- suppressWarnings(result %>%
                                 separate(col = name, into = c("entries", "type"), sep = '\\.') %>%
                                 mutate(type = case_when(
                                   is.na(type) ~ 'value',
                                   TRUE ~ type)
                                   )
                               )
      }
    }

  return(data)
}

#' @title Historical Market Data
#'
#' @description Access Historical Trades for a given instrument.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param market_id String. Market to wich we are going to connect.
#' \itemize{
#' \item \strong{ROFX} - Matba Rofex.
#' \item \strong{MERV} - S&P Merval.
#' }
#' @param symbol String. Use \code{\link{trading_instruments}} to see which symbols are available.
#' @param date String. Date to be queried. With format '\%Y-\%m-\%d'.
#' @param date_from String. Used together with 'date_to'.
#' @param date_to String. Userd together with 'date_from'.
#' @param tidy Logical. Data arranged on a tidy format.
#'
#' @return If correct, it will load a data frame.
#'
#' @family market data functions
trading_mdh <- function(connection, market_id='ROFX', symbol, date, date_from, date_to, tidy = FALSE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (!market_id %in% c("ROFX", "MERV")) stop("Invalid 'market_id' parameter")
  if (missing(symbol)) stop("You should pick a 'symbol' to move forward.")
  if (missing(date) & (missing(date_from) | missing(date_to))) stop("Invalid date parameters")

  if (!missing(date)) {
    if (!.validate_fecha(date = date)) stop("The correct format for 'date' is %Y-%m-%d")
  } else {
    if (!missing(date_from) & !.validate_fecha(date = date_from)) stop("The correct format for 'date_from' is %Y-%m-%d")
    if (!missing(date_to) & !.validate_fecha(date = date_to)) stop("The correct format for 'date_to' is %Y-%m-%d")
  }

  # Query
  query <- if (!missing(date)) {
    GET(url = glue(connection@base_url, "/rest/data/getTrades"),
        query = list(
          marketId   =   market_id,
          symbol     =   symbol,
          date       =   date,
          external   =   ifelse(market_id != "ROFX", TRUE, FALSE)
        ),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))
  } else if (!missing(date_from) & !missing(date_to)) {
    GET(url = glue(connection@base_url, "/rest/data/getTrades"),
        query = list(
          marketId   =   market_id,
          symbol     =   symbol,
          dateFrom   =   date_from,
          dateTo     =   date_to,
          external   =   ifelse(market_id != "ROFX", TRUE, FALSE)
        ),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))
  }

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if (!length(content(query)$trades)) {

    message("There is no data for the product / period selected.")
    data <- NULL

  } else {

    if (tidy == TRUE) {

      data <- fromJSON(toJSON(content(query), auto_unbox = T, null = "null"))

      data <- data$trades %>%
        mutate_all(.funs = ~ map(.x = ., function(x) if(is_null(x)) {NA_real_} else {x})) %>%
        mutate_at(., .vars = vars(matches("price|size")), .funs = ~ as.double(.)) %>%
        mutate_at(., .vars = vars(matches("datetime")), .funs = ~ as.POSIXct(x = unlist(.), tz = "America/Buenos_Aires")) %>%
        mutate_at(., .vars = vars(matches("servertime")), .funs = ~ as.POSIXct(x = unlist(.)/1000,  origin = "1970-01-01", tz = "America/Buenos_Aires")) %>%
        mutate_at(., .vars = vars(matches("symbol")), .funs = ~ as.character(.)) %>%
        as_tibble()

    } else {
      result <- fromJSON(content(x = query, as = "text"))
      data <- flatten(result$trades)
    }
  }

  return(data)
}

#' @title Currencies
#'
#' @description Access currencies prices.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#'
#' @return If correct, it will load a data frame.
#'
#' @family market data functions
trading_currencies <- function(connection) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  # Query
  query <- GET(url = glue(connection@base_url, "/rest/risk/currency/getAll"),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else {

    data <- fromJSON(toJSON(content(query), auto_unbox = T, null = "null"))

    data <- data$currencies %>%
      mutate_if(.tbl = ., .predicate = is.character, .funs = ~ na_if(., y = "")) %>%
      as_tibble()

  }

  return(data)
}

# Orders ---------------------------

#' @title Send Order to the Market
#'
#' @description The method \code{trading_new_order} is use to send orders.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
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
#'
#' @return List with outputs like state of the order.
#'
#' @family order placements functions
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
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param order_id String. clOrdId given by the \code{trading_orders} method.
#' @param proprietary String. ID given by the \code{trading_orders} method.
#'
#' @return List with outputs like state of the order.
#'
#' @family order placements functions
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
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param lookup_type String. Look-up by:
#' \itemize{
#' \item COID. Client Order ID.
#' \item OID. Order ID. (Not Available)
#' }
#' @param order_id String. ID given by the \code{trading_orders} method.
#' @param proprietary String. ID given by the \code{trading_orders} method.
#'
#' @return A data frame.
#'
#' @family order management functions
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
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param account String. Account Number / Account ID.
#'
#' @return A data frame.
#'
#' @family order management functions
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

# Account Information ---------------------------

#' @title Account Information
#'
#' @description Access information about the trading account.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param account String. \strong{Mandaroty} Account Number
#' @param detailed Logical. Expanded information.
#'
#' @return If correct, it will load a tibble.
#'
#' @family account functions
trading_account <- function(connection, account, detailed = FALSE) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(account)) stop("'account' parameter cannot be empty.")

  # Query
  query <- if (detailed == FALSE) {

    GET(url = glue(connection@base_url, "/rest/risk/position/getPositions/{account}"),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))

    } else if(detailed == TRUE) {

    GET(url = glue(connection@base_url, "/rest/risk/detailedPosition/{account}"),
        add_headers(.headers = c("X-Auth-Token" = connection@token)))

    }

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else if (content(query)$status != "OK") {

    message(glue(content(query)$message, "\n", content(query)$description))
    data <- NULL

  } else {

    data <- fromJSON(toJSON(content(query), auto_unbox = T, null = "null"))

    if (detailed == FALSE) {

      if(length(data$positions)) {1} else {0}

      data <- if (length(data$positions)) {
        data$positions %>%
          jsonlite::flatten(., recursive = F) %>%
          mutate_all(., .funs = ~ map(.x = ., function(x) if(is_null(x)) {NA_real_} else {x})) %>%
          simplify_all() %>%
          as_tibble() %>%
          rename_all(.tbl = ., .funs = ~ gsub(pattern = "^instrument\\.", replacement = "", x = .))
      } else {
        message("No data available at the moment...")
        NULL
      }

    } else if(detailed == TRUE) {

      data <- if (length(data$detailedPosition$report)) {
        data$detailedPosition %>%
          t() %>%
          as_tibble() %>%
          mutate_if(., .predicate = ~ length(unlist(.)) == 1, .funs =  ~ unlist(x = ., recursive = F)) %>%
          mutate_if(., .predicate = ~ any(map(.[[1]], .f = ~ length(.)) > 1), .funs = ~ list(unlist(.[[1]], recursive = F))) %>%
          mutate(report = list(unlist(report, recursive = F) %>% purrr::map_df(., .f = ~ pluck(., "detailedPositions")) %>% as_tibble())) %>%
          mutate(lastCalculation = as.POSIXct(lastCalculation/1000, origin = "1970-01-01", tz = "America/Buenos_Aires")) %>%
          mutate_at(.tbl = ., .vars = vars(matches("report")), .funs = ~ modify_depth(.x = ., .depth = 1, ~ mutate_at(.tbl = ., .vars = vars(matches("date")), .funs = ~ as.POSIXct(./1000, origin = "1970-01-01", tz = "America/Buenos_Aires"))))
      } else {
        message("No data available at the moment...")
        NULL
      }

    }

  }

  return(data)
}

#' @title Account Report
#'
#' @description Access report about your trading account.
#'
#' @param connection S4. \strong{Mandaroty} Formal rRofexConnection class object
#' @param account String. \strong{Mandaroty} Account Number
#'
#' @return If correct, it will load a tibble.
#'
#' @note
#' To access nested data is strongly recommended the use of `pluck`.
#'
#' @family account functions
#'
#' @examples
#' \dontrun{
#' data %>% pluck("detailedAccountReports", 1, "availableToOperate", 1, "cash")
#' }
trading_account_report <- function(connection, account) {

  if (missing(connection)) stop("Connection cannot be empty.")
  if (!isS4(connection) || rev(class(connection)) != "rRofexConnection" || !validObject(connection)) stop("The 'connection' must be a valid 'rRofexConnection'.")
  if (as.Date(connection@login_date_time) != Sys.Date()) stop("The 'acyRsaConnection' is no longer valid. Please log-in again.")

  if (missing(account)) stop("'account' parameter cannot be empty.")

  # Query
  query <- GET(url = glue(connection@base_url, "/rest/risk/accountReport/{account}"),
               add_headers(.headers = c("X-Auth-Token" = connection@token)))

  if (status_code(query) != 200) {

    warn_for_status(query)
    message("\r")
    data <- NULL

  } else {

    data <- fromJSON(toJSON(content(query), auto_unbox = T, null = "null"), simplifyDataFrame = T)

    data <- if (length(data$accountData)) {

      data <- data$accountData %>%
        replace_na(data = ., replace = NA) %>%
        t() %>%
        as_tibble() %>%
        mutate_at(.tbl = ., .vars = vars(matches("detailedAccountReports")), .funs = ~ modify_depth(., .depth = 3, ~ replace_na(., replace = NA))) %>%
        mutate_if(.tbl = ., .predicate = ~ length(unlist(.)) == 1, .funs =  ~ unlist(x = ., recursive = F))

      data <- if (data$detailedAccountReports[[1]] %>% length() > 1) {
        data %>%
          mutate(detailedAccountReports = list(
            select(.data = ., detailedAccountReports) %>%
              unlist() %>%
              enframe() %>%
              separate(data = ., col = name, into = c(glue("X{1:", .$name %>% strsplit(x = ., split = "\\.") %>% map_int(., length) %>% max, "}")), sep = "\\.", fill = "right") %>%
              select(-X1) %>%
              rename(Term = X2) %>%
              mutate(Term = as.integer(Term)) %>%
              split(x = .,f = .$X3) %>%
              t() %>%
              as_tibble() %>%
              mutate_at(.tbl = .,
                        .vars = vars(matches("settlementDate")),
                        .funs = ~ modify_depth(.x = .,
                                               .depth = 1,
                                               .f = ~ select(., Term, value) %>%
                                                 rename(settlementDate = value) %>%
                                                 mutate(settlementDate = as.POSIXct(unlist(settlementDate)/1000, origin = "1970-01-01", tz = "America/Buenos_Aires"))
                        )
              ) %>%
              mutate_at(.tbl = .,
                        .vars = vars(matches("availableToOperate")),
                        .funs = ~ modify_depth(.x = .,
                                               .depth = 1,
                                               .f = ~ split(x = ., .$X4) %>%
                                                 t() %>%
                                                 as_tibble(.) %>%
                                                 mutate_at(.tbl = .,
                                                           .vars = vars(matches("cash", ignore.case = F)),
                                                           .funs = ~ modify_depth(.x = .,
                                                                                  .depth = 1,
                                                                                  .f = ~ select(., Term, X5, X6, value) %>%
                                                                                    pivot_wider(data = ., names_from = c(X5, X6), values_from = value) %>%
                                                                                    rename_all(.tbl = ., .funs = ~ gsub(pattern = "_NA|detailedCash_", replacement = "", x = .)) %>%
                                                                                    rename_all(.tbl = ., .funs = ~ gsub(pattern = " ", replacement = "_", x = .))
                                                           )) %>%
                                                 mutate_at(.tbl = .,
                                                           .vars = vars(matches("movements", ignore.case = F)),
                                                           .funs = ~ modify_depth(.x = .,
                                                                                  .depth = 1,
                                                                                  .f = ~ select(., Term, value) %>%
                                                                                    rename(Movements = value)
                                                           )) %>%
                                                 mutate_at(.tbl = .,
                                                           .vars = vars(matches("credit", ignore.case = F)),
                                                           .funs = ~ modify_depth(.x = .,
                                                                                  .depth = 1,
                                                                                  .f = ~ select(., Term, value) %>%
                                                                                    rename(Credit = value) %>%
                                                                                    mutate(Credit = replace_na(Credit, 0))
                                                           )) %>%
                                                 mutate_at(.tbl = .,
                                                           .vars = vars(matches("total", ignore.case = F)),
                                                           .funs = ~ modify_depth(.x = .,
                                                                                  .depth = 1,
                                                                                  .f = ~ select(., Term, value) %>%
                                                                                    rename(Total = value)
                                                           )) %>%
                                                 mutate_at(.tbl = .,
                                                           .vars = vars(matches("pendingMovements", ignore.case = F)),
                                                           .funs = ~ modify_depth(.x = .,
                                                                                  .depth = 1,
                                                                                  .f = ~ select(., Term, value) %>%
                                                                                    rename(PendingMovements = value)
                                                           ))

                        )
              ) %>%
              mutate_at(.tbl = .,
                        .vars = vars(matches("currencyBalance")),
                        .funs = ~ modify_depth(.x = .,
                                               .depth = 1,
                                               .f = ~ select(., Term, X5, X6, value) %>%
                                                 pivot_wider(data = ., names_from = X5, values_from = value) %>%
                                                 rename(Type = X6) %>%
                                                 rename_all(.tbl = ., .funs = ~ gsub(pattern = " ", replacement = "_", x = .))
                        )
              ))
          )
      } else {
        message("No complete data available at the moment...")
        data
      }

    } else {
      message("No data available at the moment...")
      NULL
    }

  }

  return(data)
}
