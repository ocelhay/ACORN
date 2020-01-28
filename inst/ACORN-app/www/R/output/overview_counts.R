output$n_overview_patient <- renderText({
  p <- patient() %>% nrow()
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("user")),
            h3(p),
            span(strong("Patient Enrollments"), br(),
                 "with ", n_distinct(patient()$patient_id), " distinct patients.")
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


output$n_overview_pathogen <- renderText({
  p <- microbio() %>%
    filter(organism %in% c("Acinetobacter baumannii", "Escherichia coli", "Klebsiella pneumoniae", 
                           "Staphylococcus aureus", "Streptococcus pneumoniae")) %>%
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

