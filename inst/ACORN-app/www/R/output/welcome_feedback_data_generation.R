output$feedback_data_generation <- renderText({
  
  paste0(
    div(class = 'box_feedback',
        HTML(generation_status$version_CLSI), br(),
        HTML(generation_status$version_EUCAST), br(),
        
        h3("Progression of data generation"),
        HTML(paste0(generation_status$log, sep = "<br>"))
    )
  )
})