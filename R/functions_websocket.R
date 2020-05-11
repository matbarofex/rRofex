#' @include s4_object.R
#' NULL

trading_ws_connection <- function(connection) {
  ws <- WebSocket$new(url = gsub(pattern = "(.+)(:.+)", replacement = "wss\\2", x = connection@base_url),
                      headers = list("X-Auth-Token" = connection@token))
  ws$onOpen(function(event){
    cat("Client connected\n")
  })
  ws$onClose(function(event) {
    cat("Client disconnected\n")
  })

  return(ws)
}

trading_ws_md <- function(websocket_connection) {
  websocket_connection$send('{"type":"smd","level":1, "entries":["BI", "OF"],"products":[{"symbol":"DODic20","marketId":"ROFX"}]}')
  websocket_connection$onMessage(function(event) {
    cat("Client got msg: ", event$data, "\n")
  })
}
