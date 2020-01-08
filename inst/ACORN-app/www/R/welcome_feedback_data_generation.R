output$feedback_data_generation <- renderText({
  
  paste0(
    div(class = 'box_feedback',
        h4("Progression"),
        if(generation_status$file_data_dic) span(icon('check-square'), 'Data dictionnary', br()),
        if(! generation_status$file_data_dic) span(icon('minus'), 'Data dictionnary', br()),
        
        if(generation_status$file_lab_codes) span(icon('check-square'), 'Lab codes data', br()),
        if(! generation_status$file_lab_codes) span(icon('minus'), 'Lab codes data', br()),
        
        if(generation_status$file_lab_data) span(icon('check-square'), 'Lab data', br()),
        if(! generation_status$file_lab_data) span(icon('minus'), 'Lab data', br()),
        
        if(generation_status$file_odk_data) span(icon('check-square'), 'ODK data', br()),
        if(! generation_status$file_odk_data) span(icon('minus'), 'ODK data', br()),
        
        br(),
        
        if(generation_status$generate_acorn_data) span(icon('check-square'), 'ACORN data generated', br()),
        if(! generation_status$generate_acorn_data) span(icon('minus'), 'ACORN data generated', br())
    )
)
})