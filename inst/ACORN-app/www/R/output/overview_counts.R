output$n_overview_patient <- renderText({
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("user")),
            h3(patient_filter() %>% nrow()),
            span(strong("Patient Enrollments"), br(),
                 "with ", patient() %>% pull(patient_id) %>% n_distinct(), " distinct patients.")
        )
      )
    )
  )
})



output$n_overview_specimen <- renderText({
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("vial")),
            h3(microbio_filter() %>% pull(specimen_id) %>% n_distinct()),
            span(strong("Specimens Collected"))
        )
      )
    )
  )
})


output$n_overview_pathogen <- renderText({
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("vial")),
            h3(microbio_filter() %>% fun_filter_target_pathogens %>% nrow()),
            span(strong("Isolates of Target Pathogens"))
        )
      )
    )
  )
})

