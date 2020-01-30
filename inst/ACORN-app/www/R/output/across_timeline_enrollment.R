output$timeline_enrollment <- renderTimevis({
  req(patient())
  
  start_enrollment <- min(patient()$date_enrollment, na.rm = TRUE)
  end_enrollment <- max(patient()$date_enrollment, na.rm = TRUE)
  start_enrollment_filt <- input$filter_enrollment[1]
  end_enrollment_filt <- input$filter_enrollment[2]
  
  data <- data.frame(
    id      = 1:2,
    content = c("Range of Enrollment Dates", "Selected Range of Enrollment Dates"),
    start   = c(start_enrollment, start_enrollment_filt),
    end     = c(end_enrollment, end_enrollment_filt),
    type = c("range", "range") #,
  )
  
  timevis(data, options = list(editable = FALSE))
})