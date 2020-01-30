output$n_overview_patient <- renderText({
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("user")),
            h3(patient_filter() %>% nrow()),
            span(strong("Patient Enrollments"), br(),
                 "with ", n_distinct(patient()$patient_id), " distinct patients.")
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
            h3(n_distinct(microbio_filter()$specimen_id)),
            span(strong("Specimens Collected"))
        )
      )
    )
  )
})


output$n_overview_pathogen <- renderText({
  p <- microbio_filter() %>%
    filter(organism %in% c("Acinetobacter baumannii", "Escherichia coli", "Klebsiella pneumoniae", 
                           "Staphylococcus aureus", "Streptococcus pneumoniae") |
             str_detect(organism, "Salmonella")) %>%
    nrow()
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("vial")),
            h3(p),
            span(strong("Isolates of Target Pathogens"))
        )
      )
    )
  )
})

