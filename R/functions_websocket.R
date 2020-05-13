#' @include s4_object.R
#' NULL

trading_ws_open <- function(connection) {
  ws <- WebSocket$new(url = gsub(pattern = "(.+)(:.+)", replacement = "wss\\2", x = connection@base_url),
                      headers = list("X-Auth-Token" = connection@token))
  ws$onOpen(function(event){
    message(glue("Client connected with rRofex using websockets to {connection@base_url}..."))
  })
  ws$onClose(function(event) {
    message(glue("Client disconnected from {connection@base_url}..."))
  })

  return(ws)
}

trading_ws_close <- function(websocket_connection) {
  websocket_connection$close()
}

trading_ws_send <- function(websocket_connection, msg) {
  websocket_connection$send()
}

trading_ws_md <- function(websocket_connection) {
  websocket_connection$send('{"type":"smd","level":1, "entries":["BI", "OF"],"products":[{"symbol":"DODic20","marketId":"ROFX"}]}')
  websocket_connection$onMessage(function(event) {
    cat("Client got msg: ", event$data, "\n")
  })
}
