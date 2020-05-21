output$n_patient <- renderText({

  return(
    paste0(
      as.character(
      div(class = "n_box",
          div(class = "icon_box", icon("user-check")),
          h3(microbio_filter() %>% fun_deduplication(method = input$deduplication_method) %>% pull(patient_id) %>% n_distinct()),
          strong("Patients with Microbiology")
      )
    )
    )
  )
})

output$n_specimen <- renderText({
  
  p <- microbio_filter() %>% fun_deduplication(method = input$deduplication_method) %>% pull(specimen_id) %>% n_distinct()
  n <- microbio_filter()  %>% fun_deduplication(method = input$deduplication_method) %>% pull(patient_id) %>% n_distinct()
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

  return(
    paste0(
      as.character(
        div(class = "n_box",
            div(class = "icon_box", icon("microscope")),
            h3(microbio_filter() %>% fun_filter_growth_only() %>% fun_deduplication(method = input$deduplication_method) %>% pull(isolate_id) %>% n_distinct()),
            span(strong("Isolates"), "from cultures that have growth")
        )
      )
    )
  )
})