# Primary API Login ---------------------------

#' Primary API LogIn
#'
#'\code{primary_login} log in the user into de Primary API
#'
#'@param username User Name
#'@param password Password
#'@param environment Wich environment are you going to connect
#'#'\itemize{
#'\item reMarkets: Testing environment
#'}
#'
#'@return If correct, it will save a token into the current environment
#'
#'@import httr
#'
#'@examples
#'\dontrun{primary_login(username="pepe", password="pepino", environment="reMarkets")}
primary_login <- function(username="ahassel731", password="hheqvK5<", environment="reMarkets") {

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

# Primary Instrument ---------------------------

#' Primary API Insturments
#'
#'\code{primary_instruments} list segments and instruments currently available in Primary API.
#'
#'@param request The type of request that you are making:
#'\itemize{
#'\item segments: Available Market Segments
#'\item securities: Available Instruments listed on Rofex
#'}
#'@param environment Wich environment are you going to connect:
#'\itemize{
#'\item reMarkets: Testing environment
#'}
#'@param sec_detailed Logical. Optional for environment=securities. Brings aditional information like segment, price, minimal/maximal trading quantity, settlement date, etc.
#'
#'@return If correct, it will load a data frame.
#'
#'@import httr
#'@import jsonlite
#'@import magrittr
#'
#'@examples
#'\dontrun{primary_instruments()}
primary_instruments <- function(request, environment="reMarkets", sec_detailed = FALSE) {
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

  result <- fromJSON(content(x = query,as = "text"))

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

#' @export primary_login
#' @export primary_instruments
NULL

