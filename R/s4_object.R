#' @export token
#' @export base_url
#' @export login_date_time
NULL

# Connection Class
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

setValidity("rRofexConnection", function(object){
  if (length(object@token) != 1 || length(object@base_url) != 1) {
    "@token and @base_url must have lenght 1"
  } else if (object@token == "" || object@base_url == "") {
    "@token and @base_url can not be empty"
  } else {
    TRUE
  }
})

setGeneric("token", function(x) standardGeneric("token"))
setMethod("token", "rRofexConnection", function(x) x@token)

setGeneric("token<-", function(x, value) standardGeneric("token<-"))
setMethod("token<-", "rRofexConnection", function(x, value) {
  x@token <- value
  validObject(x)
  x
})

setGeneric("base_url", function(x) standardGeneric("base_url"))
setMethod("base_url", "rRofexConnection", function(x) x@base_url)

setGeneric("base_url<-", function(x, value) standardGeneric("base_url<-"))
setMethod("base_url<-", "rRofexConnection", function(x, value) {
  x@base_url <- value
  validObject(x)
  x
})

setGeneric("login_date_time", function(x) standardGeneric("login_date_time"))
setMethod("login_date_time", "rRofexConnection", function(x) x@login_date_time)
