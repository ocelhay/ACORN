output$upload_lab_data <- renderUI({
  
  # WHONET file
  if(input$whonet_file == "WHONET file") {
    return(
      tagList(
        p("Upload WHONET data file:"),
        fileInput("file_lab_data", label = NULL, buttonLabel = "Browse ...")
      )
    )
  }
  
  # Not WHONET file
  if(input$whonet_file != "WHONET file") {
    return(
      tagList(
        p("Upload lab data (.csv, .txt, .xls(x) extensions):"),
        fileInput("file_lab_data", label = NULL, accept = c(".csv", ".txt", ".xls", ".xlsx"), buttonLabel = "Browse ...")
      )
    )
  }
  
})