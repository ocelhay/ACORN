output$feedback_data_generation <- renderText({
  
  paste0(
    div(class = 'box_feedback',
        h3("Progression of data import"),
        if(generation_status$file_data_dic) span(icon('check-square'), 'Data dictionnary', br()),
        if(! generation_status$file_data_dic) span(icon('minus'), 'Data dictionnary', br()),
        
        if(generation_status$file_lab_codes) span(icon('check-square'), 'Lab codes data', br()),
        if(! generation_status$file_lab_codes) span(icon('minus'), 'Lab codes data', br()),
        
        if(generation_status$file_lab_data) span(icon('check-square'), 'Lab data', br()),
        if(! generation_status$file_lab_data) span(icon('minus'), 'Lab data', br()),
        
        if(generation_status$file_odk_data) span(icon('check-square'), 'ODK data', br()),
        if(! generation_status$file_odk_data) span(icon('minus'), 'ODK data', br())
    )
)
})


output$feedback_data_generation_2 <- renderText({
  
  paste0(
    div(class = 'box_feedback',
        HTML(generation_status$version_CLSI), br(),
        HTML(generation_status$version_EUCAST), br(),
        
        h3("Progression of data generation"),
        HTML(paste0(generation_status$log, sep = "<br>")),
        br(),
        HTML(generation_status$link_F01_F02), br(),
        HTML(generation_status$link_F01_F03), br(),
        br(),
        HTML(generation_status$patid), br(),
        HTML(generation_status$specid), br(),
        HTML(generation_status$specdate), br(),
        HTML(generation_status$specdate2), br(),
        HTML(generation_status$specgroup), br(),
        HTML(generation_status$orgname)
    )
  )
})