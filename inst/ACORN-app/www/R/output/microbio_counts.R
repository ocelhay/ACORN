output$n_patient <- renderText({

  return(
    paste0(
      as.character(
      div(class = "n_box",
          div(class = "icon_box", icon("user-check")),
          h3(n_distinct(microbio_filter() %>% fun_deduplication() %>% pull(patient_id))),
          strong("Patients with Microbiology")
      )
    )
    )
  )
})

output$n_specimen <- renderText({
  
  p <- n_distinct(microbio_filter() %>% fun_deduplication() %>% pull(specimen_id))
  n <- n_distinct(microbio_filter()  %>% fun_deduplication() %>% pull(patient_id))
  prop = round(p/n, 2)
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("vial")),
            h3(p),
            span(strong("Specimens Collected"), br(),  prop, " per patient")
        )
      )
    )
  )
})

output$n_isolate <- renderText({
  n <- n_distinct(microbio_filter() %>% fun_deduplication() %>% pull(isolate_id))
  
  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("microscope")),
            h3(n),
            span(strong("Isolates"), "from cultures that have growth")
        )
      )
    )
  )
})