#' @export token
#' @export base_url
#' @export login_date_time
NULL

#' @title rRofexConnection-method
#' @name rRofexConnection-method
#' @rdname rRofexConnection-method
#' @param x S4 Class. rRofexConnection object
#' @param value New value to be assigned
NULL

# class definition -----

#' @title Connection Class: rRofexConnection
#'
#' @description Creates a rRofex connection object
#'
#' @slot token character. Obtained from login method
#' @slot base_url character. Connected environment
#' @slot login_date_time character. Log-in date time. Valid for a day.
setClass("rRofexConnection",
         slots = c(
           token = "character",
           base_url = "character",
           login_date_time = "character"
         ),
         prototype = list(
           token = NA_character_,
           base_url = NA_character_,
           login_date_time = NA_character_
         )
)

# class validation ----

setValidity("rRofexConnection", function(object){
  if (length(object@token) != 1 || length(object@base_url) != 1) {
    "@token and @base_url must have lenght 1"
  } else if (object@token == "" || object@base_url == "") {
    "@token and @base_url can not be empty"
  } else {
    TRUE
  }
})

 # generics -----

#' @rdname rRofexConnection-method
setGeneric("token", function(x) standardGeneric("token"))

#' @rdname rRofexConnection-method
setGeneric("token<-", function(x, value) standardGeneric("token<-"))

#' @rdname rRofexConnection-method
setGeneric("base_url", function(x) standardGeneric("base_url"))

#' @rdname rRofexConnection-method
setGeneric("base_url<-", function(x, value) standardGeneric("base_url<-"))

#' @rdname rRofexConnection-method
setGeneric("login_date_time", function(x) standardGeneric("login_date_time"))

# methods -----

#' @rdname rRofexConnection-method
setMethod("token", "rRofexConnection", function(x) x@token)

#' @rdname rRofexConnection-method
setMethod("token<-", "rRofexConnection", function(x, value) {
  x@token <- value
  validObject(x)
  x
})

#' @rdname rRofexConnection-method
setMethod("base_url", "rRofexConnection", function(x) x@base_url)

#' @rdname rRofexConnection-method
setMethod("base_url<-", "rRofexConnection", function(x, value) {
  x@base_url <- value
  validObject(x)
  x
})

#' @rdname rRofexConnection-method
setMethod("login_date_time", "rRofexConnection", function(x) x@login_date_time)
