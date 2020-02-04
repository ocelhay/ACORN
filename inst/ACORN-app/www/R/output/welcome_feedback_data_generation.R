output$feedback_data_generation <- renderText({
  req(generation_status$generate_acorn_data)
  
  paste0(
    div(class = 'box_feedback',
        
        h4("Data Generation Log"), br(),
        HTML(generation_status$version_CLSI), br(),
        HTML(generation_status$version_EUCAST), br(), br(),
        
        HTML(paste("<ul><li>", paste0(generation_status$log, collapse = "</li><li>"), "</li></ul>"))
    )
  )
})