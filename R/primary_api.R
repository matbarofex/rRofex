#' Primary API LogIn
#'
#'\code{primary_login} log in the user into de Primary API
#'
#' @return
#' @export
#'
#' @examples
primary_login <- function(username="ahassel731", password="hheqvK5<", where="reMarkets") {
  url <- if (where == 'reMarkets') {
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

primary_instruments <- function() {
  if (!exists("x_auth_token")) stop("Primero debes iniciar sesión con primary_login()")
}


