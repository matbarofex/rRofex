#' @export token
#' @export base_url
#' @export login_date_time
#' @export agent
#' @export user_name
NULL

# class definition -----

#' @title Connection Class: rRofexConnection
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#' Creates an rRofex connection object that contains a summary from the \code{\link{trading_login}} function.
#'
#' @slot token character. Obtained from login method
#' @slot base_url character. Connected environment
#' @slot login_date_time character. Log-in date time. The connection object is only valid for a day.
#' @slot agent character. User Agent to pass to the API. Format: 'rRofex-<environment>-user_name'
#' @slot user_name character. User Name.
#'
#' @return S4 rRofexConnection object.
#' @rdname rRofexConnection
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

#' @title See Token
#' @description Shows information about the token thas has been generated with \code{\link{trading_login}}
#' @param x S4 Class. rRofexConnection object
#' @rdname token
#' @return Scalar with token
setGeneric("token", function(x) standardGeneric("token"))

#' @title See Base URL
#' @description Shows information about the 'base url' where the user has been connected with \code{\link{trading_login}}
#' @param x S4 Class. rRofexConnection object
#' @rdname base_url
#' @return Scalar with the 'base url'
setGeneric("base_url", function(x) standardGeneric("base_url"))

#' @title See Log-in Timestamp
#' @description Shows information about the connection timestamp when calling \code{\link{trading_login}}
#' @param x S4 Class. rRofexConnection object
#' @rdname login_date_time
#' @return Scalar with the 'log-in timestamp'
setGeneric("login_date_time", function(x) standardGeneric("login_date_time"))

#' @title See Agent
#' @description Shows information about the agent set with \code{\link{trading_login}}
#' @param x S4 Class. rRofexConnection object
#' @rdname agent
#' @return Scalar with the 'agent'
setGeneric("agent", function(x) standardGeneric("agent"))

#' @title See User Name
#' @description Shows information about the user name connected using \code{\link{trading_login}}
#' @param x S4 Class. rRofexConnection object
#' @rdname user_name
#' @return Scalar with the 'user_name'
setGeneric("user_name", function(x) standardGeneric("user_name"))

# methods -----

#' @rdname token
setMethod("token", "rRofexConnection", function(x) x@token)

#' @rdname base_url
setMethod("base_url", "rRofexConnection", function(x) x@base_url)

#' @rdname login_date_time
setMethod("login_date_time", "rRofexConnection", function(x) x@login_date_time)

#' @rdname agent
setMethod("agent", "rRofexConnection", function(x) x@agent)

#' @rdname user_name
setMethod("user_name", "rRofexConnection", function(x) x@user_name)

#' @title Show summary of rRofexConnection
#' @description Shows a summary about the rRofexConnection object created with \code{\link{trading_login}}
#' @param object S4 Class. rRofexConnection object
#' @return Summary text with User, Environment and Timestamp
setMethod("show", "rRofexConnection", function(object){
  cat(is(object)[[1]], " Object\n",
      "   User:  ", object@user_name, "\n",
      "   Environment:  ", object@base_url, "\n",
      "   Log-in date time:  ", object@login_date_time, "\n",
      sep = "")
})

#' @title Create rRofex Connection Object
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#' \code{rRofex_connection} creates a New Connection Object.
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
#'
#' @return A valid rRofexConecciont object.
rRofex_connection <- function(token, base_url, user_name) {
  new(Class = "rRofexConnection",
      token = token,
      base_url = base_url,
      login_date_time = as.character(Sys.time()),
      agent = paste0("rRofex-", gsub(pattern = "(.+api\\.)(.+?)(\\..+)", replacement = "\\2",x = base_url), "-", user_name),
      user_name = user_name)
}
