output$timeline_enrollment <- renderTimevis({
  
  # if(is.null(patient_filter())) return(NULL)
  # 
  # req(patient_filter())
  # req(nrow(patient_filter()) > 0)
  
  req(patient())
  
  start_enrollment <- min(patient()$date_enrollment, na.rm = TRUE)
  end_enrollment <- max(patient()$date_enrollment, na.rm = TRUE)
  start_enrollment_filt <- input$filter_enrollment[1]
  end_enrollment_filt <- input$filter_enrollment[2]
  
  # start_admission <- min(patient_filter()$date_admission)
  # end_admission <- max(patient_filter()$date_admission)
  
  # start_enrollment <- "2018-09-24"
  # end_enrollment <- "2019-09-24"
  # start_enrollment_filt <- "2018-09-24"
  # end_enrollment_filt <- "2019-09-24"
  
  data <- data.frame(
    id      = 1:2,
    content = c("Range Patients Date Enrollment", "Selected Patients"),
    start   = c(start_enrollment, start_enrollment_filt),
    end     = c(end_enrollment, end_enrollment_filt),
    type = c("range", "range") #,
    # className = c("eclipse", "confirmed")
  )
  
#   styles <- "
# .vis-item.possible { background-color: LightGray; }
# .vis-item.confirmed { background-color: Khaki; }
# .vis-item.eclipse { background-color: MediumSeaGreen; }
# 
# .vis-labelset .vis-label.possible { color: Black; }
# .vis-labelset .vis-label.confirmed { color: #71670f; }
# .vis-labelset .vis-label.eclipse { color: #004d00; }"
#   
#   
#   tv <- timevis(data, options = list(editable = FALSE))
#   
#   tagList(list(tags$head(tags$style(styles, type="text/css")), tv))
  
  timevis(data, options = list(editable = FALSE))
})