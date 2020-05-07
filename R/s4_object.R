#' @export token
#' @export base_url
#' @export login_date_time
#' @export agent
#' @export user_name
NULL

#' @title rRofexConnection-method
#' @name rRofexConnection-method
#' @rdname rRofexConnection-method
#' @param x S4 Class. rRofexConnection object
#' @param object S4 Class. rRofexConnection object
NULL

# class definition -----

#' @title Connection Class: rRofexConnection
#'
#' @description Creates a rRofex connection object
#'
#' @slot token character. Obtained from login method
#' @slot base_url character. Connected environment
#' @slot login_date_time character. Log-in date time. Valid for a day.
#' @slot agent character. User Agent to pass to the API. Format: 'rRofex-<environment>-user_name'
#' @slot user_name character. User Name.
setClass("rRofexConnection",
         slots = c(
           token = "character",
           base_url = "character",
           login_date_time = "character",
           agent = "character",
           user_name = "character"
         ),
         prototype = list(
           token = NA_character_,
           base_url = NA_character_,
           login_date_time = NA_character_,
           agent = NA_character_,
           user_name = NA_character_
         )
)

# class validation ----

setValidity("rRofexConnection", function(object){
  if (length(object@token) != 1 || length(object@base_url) != 1 || length(object@agent) != 1 || length(object@user_name) != 1) {
    "@token, @base_url, @agent and @user_name must have lenght 1"
  } else if (object@token == "" || object@base_url == "" || object@agent == "" || object@user_name == "") {
    "@token, @base_url, @agent and @user_name can not be empty"
  } else if (!grepl(pattern = "^(http|https)://", x = object@base_url)) {
    "@base_url has an invalid format"
  } else {
    TRUE
  }
})

 # generics -----

#' @rdname rRofexConnection-method
setGeneric("token", function(x) standardGeneric("token"))

#' @rdname rRofexConnection-method
setGeneric("base_url", function(x) standardGeneric("base_url"))

#' @rdname rRofexConnection-method
setGeneric("login_date_time", function(x) standardGeneric("login_date_time"))

#' @rdname rRofexConnection-method
setGeneric("agent", function(x) standardGeneric("agent"))

#' @rdname rRofexConnection-method
setGeneric("user_name", function(x) standardGeneric("user_name"))

# methods -----

#' @rdname rRofexConnection-method
setMethod("token", "rRofexConnection", function(x) x@token)

#' @rdname rRofexConnection-method
setMethod("base_url", "rRofexConnection", function(x) x@base_url)

#' @rdname rRofexConnection-method
setMethod("login_date_time", "rRofexConnection", function(x) x@login_date_time)

#' @rdname rRofexConnection-method
setMethod("agent", "rRofexConnection", function(x) x@agent)

#' @rdname rRofexConnection-method
setMethod("user_name", "rRofexConnection", function(x) x@user_name)

#' @rdname rRofexConnection-method
setMethod("show", "rRofexConnection", function(object){
  cat(is(object)[[1]], " Object\n",
      "   User:  ", object@user_name, "\n",
      "   Environment:  ", object@base_url, "\n",
      "   Log-in date time:  ", object@login_date_time, "\n",
      sep = "")
})

#' @title Create rRofex Connection Object
#'
#' @description \code{rRofex_connection} creates a New Connection Object.
#'
#' @param token String. \strong{Mandatory} Obtained with \code{\link{trading_login}}
#' @param base_url String. \strong{Mandatory} URL given by  \code{\link{trading_login}} or known by the client.
#' @param user_name character. User Name
#'
#' @return S4 rRofexConnection object.
#'
#' @note You can use accessors to get information about the Object by using:
#' \itemize{
#' \item \code{token(conn)}
#' \item \code{base_url(conn)}
#' \item \code{login_date_time(conn)}
#' \item \code{agent(conn)}
#' \item \code{user_name(conn)}
#' }
rRofex_connection <- function(token, base_url, user_name) {
  new(Class = "rRofexConnection",
      token = token,
      base_url = base_url,
      login_date_time = as.character(Sys.time()),
      agent = paste0("rRofex-", gsub(pattern = "(.+api\\.)(.+?)(\\..+)", replacement = "\\2",x = base_url), "-", user_name),
      user_name = user_name)
}
