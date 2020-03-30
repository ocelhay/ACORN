output$online_offline <- renderText({
  
  
  if (! local_server_test) return(as.character(
    span(h4(icon("wifi"), "ONLINE"), br(), p("You are using the ONLINE version ", version, br(), "Uploaded data is only used while the App is open and deleted immediately on browser close."))))
  if (local_server_test) return(as.character(
    span(h4(icon("laptop"), "OFFLINE"), p("You are using the OFFLINE version ", 
                                          ifelse("ACORN" %in% installed.packages(), 
                                                 as.character(packageVersion("ACORN"), br(), "No data will be uploaded at any stage."),
                                                 " dev.")))))
  
})