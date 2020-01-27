output$online_offline <- renderText({
  
  if (! local_server_test) return(as.character(
    span(h4(icon("wifi"), "ONLINE"), br(), p("You are using the ONLINE version of the ACORN App.", br(), "Uploaded data is only used while the App is open and deleted immediately on browser close."))))
  if (local_server_test) return(as.character(
    span(h4(icon("laptop"), "OFFLINE"), p("You are using the OFFLINE version", br(), "No data will be uploaded at any stage."))))
  
})