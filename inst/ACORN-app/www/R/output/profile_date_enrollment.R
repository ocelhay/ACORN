output$profile_date_enrollment <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  
  if(input$show_date_week) return({
  df <- patient_filter() %>%
    mutate(date_enrollment = floor_date(date_enrollment, "week")) %>%
    group_by(date_enrollment) %>%
    count() %>%
    mutate(week = paste0(year(date_enrollment), " - Week ", week(date_enrollment)))
  
  hchart(df, "line", hcaes(x = date_enrollment, y = n)) %>%
    hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
    hc_colors("#969696") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.week}: <br>{point.n} patients") %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
  })
  
  if(! input$show_date_week) return({
    df <- patient_filter() %>%
      mutate(date_enrollment = floor_date(date_enrollment, "month")) %>%
      group_by(date_enrollment) %>%
      count() %>%
      mutate(month = paste0(year(date_enrollment), " ", month(date_enrollment)))
    
    hchart(df, "line", hcaes(x = date_enrollment, y = n)) %>%
      hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
      hc_colors("#969696") %>%
      hc_tooltip(headerFormat = "", pointFormat = "{point.month}: <br>{point.n} patients") %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
  })
})