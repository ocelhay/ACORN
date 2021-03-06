output$bed_occupancy_ward_title <- renderText({
  req(input$filter_type_ward)
  req(hai_surveys_filter())
  req(nrow(hai_surveys_filter() > 0))
  
  return(paste0(br(), p("All ", paste0(input$filter_type_ward, collapse = " & "), " wards. Use filters to narrow by date range, type of ward or ward.")))
})

output$bed_occupancy_ward <- renderPlot({
  req(input$filter_type_ward)
  req(hai_surveys_filter())
  req(nrow(hai_surveys_filter() > 0))
  
  dta <- hai_surveys_filter() %>%
    mutate(beds = as.numeric(beds),
           patients = as.numeric(patients)) %>%
    mutate(occupancy = round(100*patients / beds)) %>%
    select(-ward) %>%
    rename(date_enrollment = date_survey, ward = ward_type)

  ggplot(dta, aes(x = date_enrollment, y = occupancy, group = date_enrollment)) +
    geom_boxplot() +
    lims(y = c(0, 100)) +
    labs(title = "Occupancy rate per type of ward", x = "Date Survey", y = "Occupancy") +
    facet_wrap(~ward) +
    theme_light(base_size = 15)
})