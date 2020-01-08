output$n_overview_patient <- renderText({
  p <- patient() %>% nrow()
  print(p)
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("user")),
            h3(p),
            span(strong("Patient Enrollments"), br(),
                 "with ", n_distinct(patient()$patient_id), " distincts patients.")
        )
      )
    )
  )
})



output$n_overview_specimen <- renderText({
  p <- n_distinct(microbio()$specimen_id)
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("vial")),
            h3(p),
            span(strong("Specimens Collected"))
        )
      )
    )
  )
})

output$n_overview_isolate <- renderText({
  
  p <- microbio() %>% nrow()
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("microscope")),
            h3(p),
            span(strong("Isolates"))
        )
      )
    )
  )
})

