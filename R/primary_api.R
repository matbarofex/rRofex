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
#'@param environment String. Wich environment are you going to connect
#'#'\itemize{
#'\item reMarkets: Testing environment.For credentials go to \url{https://remarkets.primary.ventures/}
#'}
#'
#'@return If correct, it will save a token into the current environment
#'
#'@examples
#'\dontrun{trading_login(username="pepe", password="pepino", environment="reMarkets")}
trading_login <- function(username="ahassel731", password="hheqvK5<", environment="reMarkets") {

  # Environment
  url <- if (environment == 'reMarkets') {
    "http://pbcp-remarket.cloud.primary.com.ar/auth/getToken"
    }

  token <- POST(url = url,
                add_headers(.headers = c("X-Username" = username,
                                         "X-Password" = password)
                            )
                )

  head <- headers(token)

  active_token <- head$`x-auth-token`

  if (!is.null(active_token)) {
    x_auth_token <<- active_token
    message("Conexión exitosa")
  } else {
    warning("Algo salió mal... verifique las credenciales...")
  }

}

# Primary Instruments ---------------------------

#' Primary API Insturments
#'
#'\code{trading_instruments} list segments and instruments currently available in Primary API.
#'
#'@param request The type of request that you are making:
#'\itemize{
#'\item segments: Available Market Segments
#'\item securities: Available Instruments listed on Rofex
#'}
#'@param environment String. Wich environment are you going to connect:
#'\itemize{
#'\item reMarkets: Testing environment
#'}
#'@param sec_detailed Logical. Optional for environment=securities. Brings aditional information like segment, price, minimal/maximal trading quantity, settlement date, etc.
#'
#'@return If correct, it will load a data frame.
#'
#'@examples
#'\dontrun{trading_instruments()}
trading_instruments <- function(request, environment="reMarkets", sec_detailed = FALSE) {
  if (!exists("x_auth_token")) stop("Primero debes iniciar sesión con primary_login()")
  if (!request %in% c("segments", "securities")) stop("El parámetro 'request' no es valido")
  if (!environment %in% c("reMarkets")) stop("El parámetro 'environment' no es valido")

  # Environment
  url <- if (environment == 'reMarkets') {
    "http://pbcp-remarket.cloud.primary.com.ar/rest/"
  }

  # Segments
  query <- if (request == 'segments') {
    GET(url = paste0(url, "segment/all"),
        add_headers(.headers = c("X-Auth-Token" = x_auth_token))
        )
  } else if (request == 'securities' & sec_detailed == F) {
    GET(url = paste0(url, "/instruments/all"),
        add_headers(.headers = c("X-Auth-Token" = x_auth_token))
    )
  } else if (request == 'securities' & sec_detailed == T) {
    GET(url = paste0(url, "/instruments/details"),
        add_headers(.headers = c("X-Auth-Token" = x_auth_token))
    )
  }

  result <- fromJSON(content(x = query, as = "text"))

  if (result$status != 'OK') stop("La query no ha tenido el resultado esperado")

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
#'@param environment String. Wich environment are you going to connect:
#'\itemize{
#'\item reMarkets: Testing environment
#'}
#'@param market_id String. Market to wich we are going to connect.
#'\itemize{
#'\item ROFX. Matba/Rofex
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
#'@param depth Ineteger. Depth of the book to be retrivied.
#'
#'@return If correct, it will load a data frame.
#'
#'@examples
#'\dontrun{trading_md(symbol='I.RFX20')}
trading_md <- function(environment="reMarkets", market_id='ROFX', symbol, entries=c('BI', 'OF', 'LA', 'OP', 'CL', 'SE', 'OI'), depth=1L) {
  if (!exists("x_auth_token")) stop("Primero debes iniciar sesión con primary_login()")
  if (!environment %in% c("reMarkets")) stop("El parámetro 'environment' no es valido")
  if (!market_id %in% c("ROFX")) stop("El parámetro 'market_id' no es valido")
  if (missing(symbol)) stop("Se debe seleccionar un 'symbol'.")

  # Environment
  url <- if (environment == 'reMarkets') {
    "http://pbcp-remarket.cloud.primary.com.ar/rest/marketdata/get"
  }

  # Symbol
  query <- GET(url = url,
               query = list(
                 marketId=market_id,
                 symbol=symbol,
                 entries=paste0(entries, collapse = ","),
                 depth=depth),
               add_headers(.headers = c("X-Auth-Token" = x_auth_token)))

  if (content(query)$status != "OK") stop("La query no ha tenido el resultado esperado")

  result <- enframe(unlist(content(x = query)$marketData))

  data <- result %>%
    separate(col = name, into = c("entries", "type"), sep = '\\.') %>%
    mutate(type = case_when(
      is.na(type) ~ 'value',
      TRUE ~ type
    ))

  return(data)
}

# Historical Market Data ---------------------------

#' Primary API Historical Market Data
#'
#'\code{trading_mdh} retrivies Historical Trades for a given instrument.
#'
#'@param environment String. Wich environment are you going to connect:
#'\itemize{
#'\item reMarkets: Testing environment
#'}
#'@param market_id String. Market to wich we are going to connect.
#'\itemize{
#'\item ROFX. Rofex: Rosario Futures Exchange.
#'\item MATBA. Matba: Mercado a Termino de Buenos Aires.
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
#'@param depth Ineteger. Depth of the book to be retrivied.
#'
#'@return If correct, it will load a data frame.
#'
#'@examples
#'\dontrun{trading_mdh(symbol='I.RFX20')}
trading_mdh <- function(environment="reMarkets", market_id='ROFX', symbol, date, date_from, date_to) {
  if (!exists("x_auth_token")) stop("Primero debes iniciar sesión con primary_login()")
  if (!environment %in% c("reMarkets")) stop("El parámetro 'environment' no es valido")
  if (!market_id %in% c("ROFX")) stop("El parámetro 'market_id' no es valido")
  if (missing(symbol)) stop("Se debe seleccionar un 'symbol'.")
  if (missing(date) & (missing(date_from) | missing(date_to))) stop("Se debe seleccionar parametros de fecha correcto.")

  validate_fecha <- function(fecha) {
    tryCatch({!is.na(format.Date(x = fecha, "%Y-%m-%d"))}, error = function(e) {FALSE})
  }

  if (!missing(date)) {
    if (!validate_fecha(fecha = date)) stop("El formato de 'date' es %Y-%m-%d")
  } else {
    if (!missing(date_from) & !validate_fecha(fecha = date_from)) stop("El formato de 'date_from' es %Y-%m-%d")
    if (!missing(date_to) & !validate_fecha(fecha = date_to)) stop("El formato de 'date_to' es %Y-%m-%d")
  }

  # Environment
  url <- if (environment == 'reMarkets') {
    "http://pbcp-remarket.cloud.primary.com.ar/rest/data/getTrades"
  }

  # Symbol
  query <- if (!missing(date)) {
    GET(url = url,
        query = list(
          marketId=market_id,
          symbol=symbol,
          date=date
        ),
        add_headers(.headers = c("X-Auth-Token" = x_auth_token)))
  } else if (!missing(date_from) & !missing(date_to)) {
    GET(url = url,
        query = list(
          marketId=market_id,
          symbol=symbol,
          dateFrom=date_from,
          dateTo=date_to
        ),
        add_headers(.headers = c("X-Auth-Token" = x_auth_token)))
  }

  if (content(query)$status != "OK") stop("La query no ha tenido el resultado esperado")
  if (!length(content(query)$trades)) stop("No hay datos para el producto / periodo seleccionado")

  result <- fromJSON(content(x = query, as = "text"))

  # Return
  data <- flatten(result$trades)

  return(data)
}



