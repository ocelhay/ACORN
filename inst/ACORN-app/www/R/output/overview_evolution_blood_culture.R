output$evolution_blood_culture <- renderHighchart({
  req(microbio_filter())
  req(nrow(microbio_filter()) > 0)
  
  # Add date of enrollment to microbio
  microbio <- left_join(microbio_filter() %>%
                          mutate(episode_id = as.character(episode_id)), 
                        patient_filter() %>% select(date_enrollment, episode_id) %>% 
                          mutate(episode_id = as.character(episode_id)),
                        by = 'episode_id')
  
  # shiny_microbio <<- microbio
  
  dta <- left_join(
    patient_filter() %>%
      group_by(month = floor_date(date_enrollment, "month")) %>%
      summarise(all = n_distinct(episode_id)),  # Number of episodes per month of enrollment
    microbio %>%
      filter(specimen_type == "Blood") %>%
      group_by(month = floor_date(date_enrollment, "month")) %>%
      summarise(blood = n_distinct(episode_id)),  # Number of blood specimen per month of enrollment
    by = "month") %>%
    mutate(month = substr(as.character(month), 1, 7),
           percent = round(100 * blood/all, 1),
           color = "#e31a1c")
  
  hchart(dta, type = "column", hcaes(x = "month", y = "percent", color = "color")) %>%
    hc_yAxis(title = list(text = "%", rotation = 0), max = 100) %>% hc_xAxis(title = "Month of Enrollment") %>% 
    hc_tooltip(pointFormat = "<b>Blood specimens {point.percent}%</b><br> ({point.blood} of {point.all} enrollments)")
})